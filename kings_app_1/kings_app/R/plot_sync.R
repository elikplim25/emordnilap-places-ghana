# ============================================================================
#  plot_sync.R -- "Two kingdoms, one clock": a Sankey of the synchronisms
#    Left column : Israel's 19 kings (931-722 BC), then Israel in exile (722-586 BC)
#    Right column: Judah's 20 rulers (931-586 BC)
#    Bar height  = years on the throne. Ribbon = two kings who reigned at the same time;
#                  its thickness = the years they shared.
#  So that the ribbons add up, every year of each kingdom is given to ONE king: the senior king
#  (from sole_start_bc in chronology_reference.csv). Years a king spent as co-regent or rival
#  are drawn as a pale cap on top of his bar; those years flow from the senior king's bar.
# ============================================================================

# senior-reign segments [a, b] (BC, a > b) that tile one kingdom's timeline without overlap
sync_segments <- function(d, last_end) {
  d <- d[order(d$id), ]
  a <- numeric(nrow(d)); b <- numeric(nrow(d)); cur <- d$sole_start_bc[1]
  for (i in seq_len(nrow(d))) {
    nxt <- if (i < nrow(d)) d$sole_start_bc[i + 1] else last_end
    a[i] <- cur
    dur <- if (d$years[i] < 1) min(d$years[i], max(a[i] - nxt, 0)) else a[i] - nxt
    if (d$years[i] < 1 && dur <= 0) dur <- d$years[i]
    if (dur <= 0) dur <- d$years[i]                       # e.g. Ahaziah of Judah, 841-841 BC
    b[i] <- a[i] - dur; cur <- b[i]
  }
  d$a <- a; d$b <- b; d$senior <- a - b
  d$cap <- pmax(0, d$years - d$senior); d$cap[d$cap < 1.5] <- 0   # +/- 1 year is only the way years were counted
  d
}

plot_sync_sankey <- function(K, tips, ctx) {
  K <- as.data.frame(K)
  I <- sync_segments(K[K$kingdom == "Northern", ], 722)
  J <- sync_segments(K[K$kingdom == "Southern", ], 586)
  exile <- data.frame(id = 0L, label = "Israel in exile", years = 722 - 586, a = 722, b = 586, senior = 136, cap = 0,
                      house = NA_character_, stringsAsFactors = FALSE)
  I <- rbind(as.data.frame(I)[, names(exile)], exile)
  J <- J[, names(exile)]

  # ribbons: every pair with shared senior years
  P <- expand.grid(i = seq_len(nrow(I)), j = seq_len(nrow(J)))
  P$hi <- pmin(I$a[P$i], J$a[P$j]); P$lo <- pmax(I$b[P$i], J$b[P$j]); P$w <- P$hi - P$lo
  P <- P[P$w > 1e-6, ]
  P <- P[order(P$i, -P$hi), ]

  # stack the nodes top-down; the pale cap sits above the solid (senior) part
  gap <- 3.2
  stack <- function(N) {
    cur <- 0; N$top <- 0; N$stop <- 0; N$bot <- 0
    for (k in seq_len(nrow(N))) { N$top[k] <- cur; N$stop[k] <- cur - N$cap[k]; N$bot[k] <- N$stop[k] - N$senior[k]; cur <- N$bot[k] - gap }
    N
  }
  I <- stack(I); J <- stack(J)
  # ribbon slots: in each node, ribbons run in time order, so slot top = node's solid top - (node start - ribbon start)
  P$ly <- I$stop[P$i] - (I$a[P$i] - P$hi); P$ry <- J$stop[P$j] - (J$a[P$j] - P$hi)

  # colours: one per Israelite dynasty (alternating), grey for the exile
  hs <- unique(I$house[!is.na(I$house)])
  hcol <- setNames(rep(c("#4e54ac", "#0A9396", "#852f88", "#CA6702", "#005F73"), length.out = length(hs)), hs)
  I$col <- ifelse(is.na(I$house), "#8A8F91", hcol[I$house])
  P$col <- I$col[P$i]

  sel <- ctx$sel
  keep <- K$id[king_keep(K, ctx)]
  P$a <- if (is.null(sel)) 0.55 else ifelse(I$id[P$i] == sel | J$id[P$j] == sel, 0.9, 0.08)
  yr <- function(v) ifelse(v < 1, fmt_reign(v), paste0(round(v), ifelse(round(v) == 1, " year", " years")))
  P$key <- paste0("s", I$id[P$i], "_", J$id[P$j])
  P$tip <- ifelse(I$id[P$i] == 0,
    sprintf("<div class=\"tt\"><div class=\"tt-name\">%s reigned in Judah with no king in Israel</div><div class=\"tt-meta\">%s</div><div class=\"tt-notes\">Israel had been carried away by Assyria in 722 BC (2 Kings 17:6).</div></div>",
            esc(J$label[P$j]), paste0(yr(P$w), ", c. ", round(P$hi), "-", round(P$lo), " BC")),
    sprintf("<div class=\"tt\"><div class=\"tt-name\">%s (Israel) and %s (Judah)</div><div class=\"tt-meta\">On the throne together for %s</div><div class=\"tt-notes\">c. %s-%s BC (reference chronology)</div></div>",
            esc(I$label[P$i]), esc(J$label[P$j]), yr(P$w), round(P$hi), round(P$lo)))

  x0 <- 1; x1 <- 2; t <- seq(0, 1, length.out = 40); s <- (1 - cos(pi * t)) / 2
  R <- do.call(rbind, lapply(seq_len(nrow(P)), function(k) {
    top <- P$ly[k] + (P$ry[k] - P$ly[k]) * s
    data.frame(key = P$key[k], x = c(x0 + (x1 - x0) * t, rev(x0 + (x1 - x0) * t)), y = c(top, rev(top - P$w[k])))
  }))
  R <- cbind(R, P[match(R$key, P$key), c("col", "a", "tip")])

  # nodes: solid senior part + pale cap
  nodes <- function(N, side, kcol) {
    N$xmin <- if (side == "l") x0 - 0.05 else x1; N$xmax <- if (side == "l") x0 else x1 + 0.05
    N$fill <- if (side == "l") ifelse(N$id == 0, "#8A8F91", kcol) else kcol
    N$na <- if (is.null(sel)) ifelse(N$id == 0 | N$id %in% keep, 1, 0.3) else ifelse(N$id == sel, 1, 0.3)
    N$tip <- ifelse(N$id == 0,
      "<div class=\"tt\"><div class=\"tt-name\">Israel in exile, 722-586 BC</div><div class=\"tt-notes\">Assyria took Samaria and carried Israel away (2 Kings 17:6). Judah reigned alone for 136 years, until Babylon took Jerusalem in 586 BC (2 Kings 25).</div></div>",
      tips[as.character(N$id)])
    N$click <- ifelse(N$id == 0, "", js_pick(pmax(N$id, 1)))
    N$data_id <- paste0(side, N$id)
    N$txt <- ifelse(N$id == 0, "Israel in exile  136y", paste0(N$label, "  ", reign_short(N$years)))
    # labels: centre of the bar, pushed apart so short reigns stay readable
    c0 <- (N$top + N$bot) / 2; ly <- c0; minsep <- 5.2
    for (k in seq_along(ly)[-1]) ly[k] <- min(c0[k], ly[k - 1] - minsep)
    N$ly <- ly; N$lx <- if (side == "l") x0 - 0.075 else x1 + 0.075; N$hj <- if (side == "l") 1 else 0
    N$tx <- if (side == "l") x0 - 0.055 else x1 + 0.055
    N
  }
  NI <- nodes(I, "l", PAL$kingdom[["Northern"]]); NJ <- nodes(J, "r", PAL$kingdom[["Southern"]])
  NI$col <- NULL
  N <- rbind(NI, NJ)
  CAP <- N[N$cap > 0, ]
  CAP$tip <- sprintf("<div class=\"tt\"><div class=\"tt-name\">%s: %s of his %s on the throne</div><div class=\"tt-notes\">were shared with another king of his own kingdom (co-regency or rival reign). Those years flow from the senior king's bar.</div></div>",
                     esc(CAP$label), yr(CAP$cap), yr(CAP$years))
  LEAD <- N[abs(N$ly - (N$top + N$bot) / 2) > 0.5, ]
  ytop <- 0; ybot <- min(N$bot)

  p <- ggplot2::ggplot() +
    ggiraph::geom_polygon_interactive(data = R, ggplot2::aes(x = x, y = y, group = key, fill = I(col), alpha = a, tooltip = tip, data_id = key), colour = NA) +
    ggiraph::geom_rect_interactive(data = N, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = bot, ymax = stop, fill = I(fill), alpha = na, tooltip = tip, data_id = data_id, onclick = click), colour = NA) +
    ggiraph::geom_rect_interactive(data = CAP, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = stop, ymax = top, tooltip = tip, data_id = paste0(data_id, "c")),
                                   fill = CAP$fill, alpha = 0.3, colour = CAP$fill, linewidth = 0.3, linetype = "22") +
    ggplot2::geom_segment(data = LEAD, ggplot2::aes(x = tx, xend = lx, y = (top + bot) / 2, yend = ly), colour = "#9AA3A6", linewidth = 0.3) +
    ggiraph::geom_text_interactive(data = N, ggplot2::aes(x = lx, y = ly, label = txt, hjust = hj, alpha = na, tooltip = tip, data_id = data_id, onclick = click),
                                   family = F_BODY, fontface = "bold", size = 3.6, colour = PAL$ink) +
    ggplot2::annotate("text", x = c(x0 - 0.025, x1 + 0.025), y = ytop + 9, label = c("ISRAEL (North)", "JUDAH (South)"), hjust = c(1, 0),
                      family = F_HEAD, fontface = "bold", size = 4.8, colour = c(PAL$kingdom[["Northern"]], PAL$kingdom[["Southern"]])) +
    ggplot2::annotate("text", x = c(x0 - 0.025, x1 + 0.025), y = ytop + 3.5, label = c("931 BC", "931 BC"), hjust = c(1, 0),
                      family = F_BODY, size = 3.4, colour = PAL$ink_soft) +
    ggplot2::annotate("text", x = x1 + 0.075, y = ybot - 5, label = "586 BC: Babylon takes Jerusalem (2 Kings 25)", hjust = 1,
                      family = F_HEAD, fontface = "bold", size = 3.8, colour = PAL$verdict[["Evil"]]) +
    ggplot2::annotate("text", x = x0 - 0.075, y = NI$ly[NI$id == 0] - 13, label = "722 BC: Assyria takes\nSamaria and carries\nIsrael away (2 Kings 17)", hjust = 1, lineheight = 0.95,
                      family = F_HEAD, fontface = "bold", size = 3.8, colour = PAL$verdict[["Evil"]]) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_x_continuous(limits = c(0.4, 2.6), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(ybot - 9, ytop + 12), expand = c(0, 0)) +
    ggplot2::theme_void(base_family = F_BODY) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = PAL$card, colour = NA), plot.margin = ggplot2::margin(6, 6, 6, 6))
  list(plot = p, units = ytop - ybot + 21)
}
