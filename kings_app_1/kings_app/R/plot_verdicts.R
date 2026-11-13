# ============================================================================
#  plot_verdicts.R -- verdicts by dynasty (grouped bars, click to drill in) and one dot per king
# ============================================================================
VERDICT_LAB <- c(Good = "Good", Mixed = "Mixed", Evil = "Evil", `N/A` = "Not rated")

plot_verdict_bars <- function(K, ctx) {
  D <- K[as.character(K$verdict) %in% ctx$verdicts, ]
  ord_h <- unique(K$house[order(K$id)])
  T <- as.data.frame(table(house = D$house, verdict = D$verdict), stringsAsFactors = FALSE); T <- T[T$Freq > 0, ]
  T$names <- vapply(seq_len(nrow(T)), function(i) paste(D$label[D$house == T$house[i] & D$verdict == T$verdict[i]], collapse = ", "), "")
  T$hi <- length(ord_h) + 1 - match(T$house, ord_h)
  T$vk <- match(T$verdict, c("Good", "Mixed", "Evil", "N/A"))
  T <- T[order(T$hi, T$vk), ]
  T$nb <- ave(T$vk, T$hi, FUN = length); T$k <- ave(T$vk, T$hi, FUN = seq_along)
  bh <- 0.3
  T$yc <- T$hi + ((T$nb - 1) / 2 - (T$k - 1)) * bh                # Good on top, then Mixed, Evil, Not rated
  T$vlab <- VERDICT_LAB[T$verdict]
  T$tip <- sprintf("<div class=\"tt\"><div class=\"tt-name\">%s</div><div class=\"tt-badges\"><span class=\"pill\" style=\"background:%s;color:%s\">%s: %d</span></div><div class=\"tt-notes\">%s</div><div class=\"tt-hint\">Click to see this dynasty's kings</div></div>",
                   esc(T$house), PAL$verdict[T$verdict], PAL$verdict_txt[T$verdict], esc(T$vlab), T$Freq, esc(T$names))
  T$click <- vapply(T$house, js_house, "")
  T$a <- if (length(ctx$dyn)) ifelse(T$house %in% ctx$dyn, 1, 0.3) else 1
  T$verdict <- factor(T$verdict, levels = c("Good", "Mixed", "Evil", "N/A"))
  ggplot2::ggplot(T) +
    ggiraph::geom_rect_interactive(ggplot2::aes(xmin = 0, xmax = Freq, ymin = yc - bh * 0.46, ymax = yc + bh * 0.46, fill = verdict, alpha = a,
                                                 tooltip = tip, data_id = paste(house, verdict), onclick = click)) +
    ggplot2::geom_text(ggplot2::aes(x = Freq + 0.25, y = yc, label = Freq), hjust = 0, family = F_HEAD, fontface = "bold", size = 4.2, colour = PAL$ink) +
    ggplot2::scale_fill_manual(values = PAL$verdict, breaks = c("Good", "Mixed", "Evil", "N/A"), labels = VERDICT_LAB[c("Good", "Mixed", "Evil", "N/A")], name = NULL, drop = FALSE) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_y_continuous(breaks = seq_along(ord_h), labels = rev(ord_h), expand = ggplot2::expansion(add = 0.55)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 30, 2), expand = ggplot2::expansion(mult = c(0, 0.06))) +
    ggplot2::labs(x = "Number of kings", y = NULL) +
    theme_kings(13) +
    ggplot2::theme(legend.position = "top", legend.text = ggplot2::element_text(size = 13, face = "bold", colour = PAL$ink),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(family = F_HEAD, face = "bold", size = 12.5, colour = PAL$ink))
}

# one dot per king, in the column of his verdict; bigger dot = longer reign
plot_verdict_dots <- function(K, tips, ctx) {
  D <- K[king_keep(K, ctx), ]
  if (!nrow(D)) return(ggplot2::ggplot() + ggplot2::theme_void())
  D <- D[order(D$id), ]
  D$tip <- tips[as.character(D$id)]; D$click <- js_pick(D$id)
  D$col <- PAL$verdict[as.character(D$verdict)]
  D$vx <- match(as.character(D$verdict), c("Good", "Mixed", "Evil", "N/A"))
  D$row <- factor(D$label, levels = rev(D$label))
  D$hs <- factor(D$house, levels = unique(D$house))
  D$a <- if (is.null(ctx$sel)) 1 else ifelse(D$id == ctx$sel, 1, 0.3)
  ggplot2::ggplot(D, ggplot2::aes(x = vx, y = row)) +
    ggiraph::geom_point_interactive(ggplot2::aes(size = pmax(years, 0.5), tooltip = tip, data_id = id, onclick = click), shape = 21, stroke = 0.5,
                                    fill = D$col, colour = "#FFFFFF", alpha = D$a) +
    ggplot2::geom_text(ggplot2::aes(x = vx + 0.28, label = reign_txt), hjust = 0, family = F_BODY, fontface = "bold", size = 3.5, colour = PAL$ink) +
    ggplot2::facet_grid(hs ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_size_area(max_size = 11) +                          # bubble AREA is proportional to the reign; under 6 months = smallest dot
    ggplot2::scale_x_continuous(limits = c(0.6, 4.9), breaks = 1:4, labels = c("Good", "Mixed", "Evil", "Not rated"), position = "top", expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_kings(13) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "#EFE8CC"), panel.grid.major.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(family = F_HEAD, face = "bold", size = 13, colour = PAL$ink),
                   axis.text.y = ggplot2::element_text(family = F_BODY, face = "bold", size = 12, colour = PAL$ink),
                   strip.placement = "outside", panel.spacing = ggplot2::unit(0.4, "lines"),
                   strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, face = "bold", family = F_HEAD, size = 11, colour = "#FFFFFF"),
                   strip.background = ggplot2::element_rect(fill = PAL$ink_soft, colour = NA))
}

# ---------------------------------------------------------------------------
# dynasties at a glance: one bar per dynasty (in order of history), split by kingdom
#   measure "kings": how many kings the dynasty had
#   measure "years": how long it held the throne, from the reference chronology, with overlapping
#                    co-regencies counted once (so House of David is ~419 years, not the 468 you get by adding reigns)
# ---------------------------------------------------------------------------
span_years <- function(s, e, y) {                   # length of the union of reigns [e, s] (BC, s > e)
  iv <- data.frame(s = s, e = e, y = y); iv <- iv[order(-iv$s), ]
  tot <- 0; cs <- iv$s[1]; ce <- iv$e[1]
  for (i in seq_len(nrow(iv))[-1]) {
    if (iv$s[i] > ce) ce <- min(ce, iv$e[i]) else { tot <- tot + (cs - ce); cs <- iv$s[i]; ce <- iv$e[i] }
  }
  tot <- tot + (cs - ce)
  if (tot < 1) sum(y) else tot                      # Zimri (7 days), Shallum (1 month)
}

plot_dynasty_bars <- function(K, ctx, measure = c("kings", "years")) {
  measure <- match.arg(measure)
  ord_h <- unique(K$house[order(K$id)])
  B <- do.call(rbind, lapply(ord_h, function(h) {
    d <- K[K$house == h, ]
    do.call(rbind, lapply(intersect(KINGDOM_LEVELS, as.character(d$kingdom)), function(kg) {
      x <- d[as.character(d$kingdom) == kg, ]
      data.frame(house = h, kingdom = kg, kings = nrow(x), years = span_years(x$start_bc, x$end_bc, x$years),
                 names = paste(x$label, collapse = ", "))
    }))
  }))
  B$v <- B[[measure]]
  B$house <- factor(B$house, levels = rev(ord_h))
  B$kingdom <- factor(B$kingdom, levels = KINGDOM_LEVELS)
  B <- B[order(B$house, B$kingdom), ]
  B$x1 <- ave(B$v, B$house, FUN = cumsum); B$x0 <- B$x1 - B$v
  B$y <- as.numeric(B$house)
  fmt <- function(v) if (measure == "kings") paste0(v, ifelse(v == 1, " king", " kings")) else ifelse(v < 1, fmt_reign(v), paste0(round(v), " yrs"))
  B$tip <- sprintf("<div class=\"tt\"><div class=\"tt-name\">%s</div><div class=\"tt-meta\">%s</div><div class=\"tt-badges\"><span class=\"pill\" style=\"background:%s;color:#fff\">%s kings | %s</span></div><div class=\"tt-notes\">%s</div><div class=\"tt-hint\">Click to see this dynasty below</div></div>",
                   esc(as.character(B$house)), esc(KINGDOM_LABELS[as.character(B$kingdom)]), PAL$kingdom[as.character(B$kingdom)],
                   B$kings, ifelse(B$years < 1, fmt_reign(B$years), paste0(round(B$years), " years")), esc(B$names))
  B$click <- vapply(as.character(B$house), js_house, "")
  B$a <- if (length(ctx$dyn)) ifelse(as.character(B$house) %in% ctx$dyn, 1, 0.3) else 1
  T <- data.frame(y = seq_along(levels(B$house)), x = as.numeric(tapply(B$x1, B$house, max)))
  T$lab <- vapply(T$x, fmt, "")
  step <- if (measure == "kings") 2 else 50
  ggplot2::ggplot(B) +
    ggiraph::geom_rect_interactive(ggplot2::aes(xmin = x0, xmax = x1, ymin = y - 0.36, ymax = y + 0.36, fill = kingdom, alpha = a,
                                                 tooltip = tip, data_id = paste(house, kingdom), onclick = click), colour = "#FFFFFF", linewidth = 0.6) +
    ggplot2::geom_text(data = T, ggplot2::aes(x = x, y = y, label = lab), nudge_x = max(T$x) * 0.012, hjust = 0, family = F_HEAD, fontface = "bold", size = 4, colour = PAL$ink) +
    ggplot2::scale_fill_manual(values = PAL$kingdom, labels = KINGDOM_LABELS, name = NULL) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_y_continuous(breaks = seq_along(levels(B$house)), labels = levels(B$house), expand = ggplot2::expansion(add = 0.55)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 500, step), limits = c(0, max(T$x) * 1.13), expand = c(0, 0)) +
    ggplot2::labs(x = if (measure == "kings") "Number of kings" else "Years the dynasty held the throne (co-regencies counted once)", y = NULL) +
    theme_kings(13) +
    ggplot2::theme(legend.position = "top", legend.text = ggplot2::element_text(size = 13, face = "bold", colour = PAL$ink),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(family = F_HEAD, face = "bold", size = 12.5, colour = PAL$ink))
}
