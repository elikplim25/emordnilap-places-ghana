# ============================================================================
#  plot_ties.R -- "Ties & Alliances"
#    plot_ties_sankey : a Sankey of the ties. One ribbon per tie, coloured by kind
#                       (kin / marriage, alliance, war / rivalry, killed / overthrown).
#        scope "between": Israel's kings on the left, Judah's (and Solomon's United Kingdom) on the right
#        scope "within" : the earlier king on the left flows to the later king on the right
#    plot_tie_counts  : how many kings had each kind of tie (both scopes together)
#  Drawn by hand with ggiraph polygons, so no extra package is needed and every ribbon has a tooltip.
# ============================================================================

tie_tip <- function(type, label, ref, a, b) {
  sprintf("<div class=\"tt\"><div class=\"tt-name\">%s and %s</div><div class=\"tt-badges\"><span class=\"pill\" style=\"background:%s;color:#fff\">%s</span></div><div class=\"tt-notes\">%s</div><div class=\"tt-refs\"><b>Read:</b> %s</div></div>",
          esc(a), esc(b), PAL$tie[type], esc(type), esc(label), esc(ref))
}

# stack the nodes of one column top-down; returns top/bottom of each node
sankey_nodes <- function(ids, units, gap) {
  top <- numeric(length(ids)); cur <- 0
  for (i in seq_along(ids)) { top[i] <- cur; cur <- cur - units[i] - gap }
  data.frame(id = ids, units = units, top = top, bot = top - units)
}

plot_ties_sankey <- function(K, L, tips, ctx, scope = "between") {
  L <- L[L$scope == scope, ]
  keep <- K$id[king_keep(K, ctx)]
  L <- L[L$from_id %in% keep & L$to_id %in% keep, ]
  if (!nrow(L)) return(list(plot = ggplot2::ggplot() + ggplot2::theme_void() +
                              ggplot2::annotate("text", x = 0, y = 0, label = "No ties for the kings in this filter", family = F_BODY, fontface = "bold", size = 5, colour = PAL$ink), units = 3))
  kg <- function(id) as.character(K$kingdom[match(id, K$id)])
  if (scope == "between") {                       # left = Israel, right = Judah / United
    isr_from <- kg(L$from_id) == "Northern"
    L$l <- ifelse(isr_from, L$from_id, L$to_id); L$r <- ifelse(isr_from, L$to_id, L$from_id)
    heads <- c("ISRAEL (North)", "JUDAH (South)")
    hcols <- c(PAL$kingdom[["Northern"]], PAL$kingdom[["Southern"]])
  } else {                                        # left = earlier king, right = later king
    L$l <- pmin(L$from_id, L$to_id); L$r <- pmax(L$from_id, L$to_id)
    heads <- c("EARLIER KING", "LATER KING")
    hcols <- c(PAL$ink_soft, PAL$ink_soft)
  }
  gap <- 0.55
  ln <- sort(unique(L$l)); rn <- sort(unique(L$r))
  NL <- sankey_nodes(ln, as.numeric(table(factor(L$l, ln))), gap)
  NR <- sankey_nodes(rn, as.numeric(table(factor(L$r, rn))), gap)
  # centre the shorter column against the taller one
  hL <- -min(NL$bot); hR <- -min(NR$bot)
  if (hL < hR) { NL$top <- NL$top - (hR - hL) / 2; NL$bot <- NL$bot - (hR - hL) / 2 } else { NR$top <- NR$top - (hL - hR) / 2; NR$bot <- NR$bot - (hL - hR) / 2 }
  # slot each ribbon inside its two nodes, ordered by the position of the other end (fewer crossings)
  L$lpos <- match(L$l, NL$id); L$rpos <- match(L$r, NR$id)
  L <- L[order(L$lpos, L$rpos), ]
  L$ly <- NL$top[L$lpos] - (ave(L$lpos, L$lpos, FUN = seq_along) - 1)
  L <- L[order(L$rpos, L$lpos), ]
  L$ry <- NR$top[L$rpos] - (ave(L$rpos, L$rpos, FUN = seq_along) - 1)
  L$type <- factor(L$type, levels = names(PAL$tie))
  L$a <- if (is.null(ctx$sel)) 0.78 else ifelse(L$l == ctx$sel | L$r == ctx$sel, 0.9, 0.12)
  L$ln <- K$label[match(L$l, K$id)]; L$rn <- K$label[match(L$r, K$id)]
  L$tip <- tie_tip(as.character(L$type), L$label, L$ref, L$ln, L$rn)
  L$key <- paste0("t", L$link_id)

  # ribbons: smooth S-curves from the left node to the right node, one unit thick
  x0 <- 1.0; x1 <- 2.0; t <- seq(0, 1, length.out = 40); s <- (1 - cos(pi * t)) / 2
  R <- do.call(rbind, lapply(seq_len(nrow(L)), function(i) {
    top <- L$ly[i] + (L$ry[i] - L$ly[i]) * s
    data.frame(key = L$key[i], x = c(x0 + (x1 - x0) * t, rev(x0 + (x1 - x0) * t)), y = c(top, rev(top - 1)))
  }))
  R <- cbind(R, L[match(R$key, L$key), c("type", "a", "tip")])

  nd <- function(N, side) {
    N$kingdom <- kg(N$id); N$label <- K$label[match(N$id, K$id)]
    N$xmin <- if (side == "l") x0 - 0.05 else x1; N$xmax <- if (side == "l") x0 else x1 + 0.05
    N$lx <- if (side == "l") x0 - 0.075 else x1 + 0.075; N$hj <- if (side == "l") 1 else 0
    N$txt <- paste0(N$label, "  (", N$units, ")")
    N$a <- if (is.null(ctx$sel)) 1 else ifelse(N$id == ctx$sel, 1, 0.35)
    N
  }
  N <- rbind(nd(NL, "l"), nd(NR, "r"))
  N$tip <- tips[as.character(N$id)]; N$click <- js_pick(N$id); N$fill <- PAL$kingdom[N$kingdom]
  ytop <- max(N$top)

  p <- ggplot2::ggplot() +
    ggiraph::geom_polygon_interactive(data = R, ggplot2::aes(x = x, y = y, group = key, fill = type, alpha = a, tooltip = tip, data_id = key), colour = NA) +
    ggiraph::geom_rect_interactive(data = N, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = bot, ymax = top, alpha = a, tooltip = tip, data_id = id, onclick = click),
                                   fill = N$fill, colour = NA) +
    ggiraph::geom_text_interactive(data = N, ggplot2::aes(x = lx, y = (top + bot) / 2, label = txt, hjust = hj, alpha = a, tooltip = tip, data_id = id, onclick = click),
                                   family = F_BODY, fontface = "bold", size = 4, colour = PAL$ink) +
    ggplot2::annotate("text", x = c(x0 - 0.025, x1 + 0.025), y = ytop + 1.1, label = heads, hjust = c(1, 0), family = F_HEAD, fontface = "bold", size = 4.8, colour = hcols) +
    ggplot2::scale_fill_manual(values = PAL$tie, name = NULL) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_x_continuous(limits = c(0.42, 2.58), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(0.4, 0.3))) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 0.85))) +
    ggplot2::theme_void(base_family = F_BODY) +
    ggplot2::theme(legend.position = "top", legend.text = ggplot2::element_text(size = 13, face = "bold", colour = PAL$ink),
                   legend.key.size = ggplot2::unit(0.55, "cm"), legend.margin = ggplot2::margin(0, 0, 10, 0),
                   plot.background = ggplot2::element_rect(fill = PAL$card, colour = NA), plot.margin = ggplot2::margin(6, 6, 6, 6))
  list(plot = p, units = ytop - min(N$bot))
}

# how many kings had each kind of tie (a king counts once per kind, however many ties of that kind he had)
plot_tie_counts <- function(K, L, ctx) {
  keep <- K$id[king_keep(K, ctx)]
  E <- rbind(data.frame(id = L$from_id, type = L$type), data.frame(id = L$to_id, type = L$type))
  E <- unique(E[E$id %in% keep, ])
  types <- names(PAL$tie)
  T <- data.frame(type = factor(types, levels = rev(types)))
  T$n <- vapply(types, function(ty) sum(E$type == ty), 0)
  T$ties <- vapply(types, function(ty) sum(L$type == ty & (L$from_id %in% keep | L$to_id %in% keep)), 0)
  T$names <- vapply(types, function(ty) paste(K$label[match(sort(E$id[E$type == ty]), K$id)], collapse = ", "), "")
  T$y <- as.numeric(T$type)
  T$lab <- paste0(T$n, " kings  |  ", T$ties, ifelse(T$ties == 1, " tie", " ties"))
  T$tip <- sprintf("<div class=\"tt\"><div class=\"tt-name\">%s</div><div class=\"tt-badges\"><span class=\"pill\" style=\"background:%s;color:#fff\">%d kings</span></div><div class=\"tt-notes\">%s</div></div>",
                   esc(types), PAL$tie[types], T$n, esc(T$names))
  ggplot2::ggplot(T) +
    ggiraph::geom_rect_interactive(ggplot2::aes(xmin = 0, xmax = n, ymin = y - 0.36, ymax = y + 0.36, fill = type, tooltip = tip, data_id = type)) +
    ggplot2::geom_text(ggplot2::aes(x = n + 0.3, y = y, label = lab), hjust = 0, family = F_HEAD, fontface = "bold", size = 4.2, colour = PAL$ink) +
    ggplot2::scale_fill_manual(values = PAL$tie) +
    ggplot2::scale_y_continuous(breaks = T$y, labels = as.character(T$type), expand = ggplot2::expansion(add = 0.55)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 30, 2), limits = c(0, max(T$n) + 6), expand = c(0, 0)) +
    ggplot2::labs(x = "Number of kings with at least one tie of this kind", y = NULL) +
    theme_kings(13) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(family = F_HEAD, face = "bold", size = 13, colour = PAL$ink))
}
