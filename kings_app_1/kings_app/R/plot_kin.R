# ============================================================================
#  plot_kin.R -- "Houses & Kin": every dynasty as ONE row of bubbles
#  one bubble per king, in order of succession; bubble AREA = years reigned,
#  colour = how he came to the throne (family / not related / first king)
# ============================================================================
plot_dynasties <- function(K, tips, ctx) {
  D <- K[order(K$id), ]
  D$active <- king_active(D, ctx)
  D$a <- ifelse(D$active, 1, 0.25)
  D$tip <- tips[as.character(D$id)]; D$click <- js_pick(D$id)
  D$kl <- factor(D$kingdom, levels = KINGDOM_LEVELS, labels = KINGDOM_SHORT[KINGDOM_LEVELS])
  D$grp <- factor(rel_group(D), levels = names(PAL$rel))
  key <- paste(D$kingdom, D$house)
  D$rowkey <- factor(key, levels = rev(unique(key)))
  D <- D[order(D$rowkey, D$id), ]
  D$pos <- ave(D$id, D$rowkey, FUN = seq_along)
  D$y <- as.numeric(D$rowkey)
  D$sz <- pmax(D$years, 0.6)                                   # 7-day reigns still get a visible dot
  n_row <- tapply(D$id, D$rowkey, length)
  # names alternate below / above the bubbles so neighbours never collide
  D$ly <- ifelse(D$pos %% 2 == 0, D$y + 0.37, D$y - 0.37)
  D$lv <- ifelse(D$ly > D$y, 0, 1)
  D$inlab <- ifelse(D$years >= 20, format(D$years, trim = TRUE, drop0trailing = TRUE), "")

  xr <- max(D$pos)
  tot <- tapply(D$years, D$rowkey, sum)
  tl <- data.frame(rowkey = names(tot), n = as.numeric(n_row[names(tot)]), yrs = round(as.numeric(tot)))
  tl$y <- match(tl$rowkey, levels(D$rowkey)); tl$x <- tl$n + 0.75     # totals sit just after the last bubble
  tl$lab <- paste0(tl$n, ifelse(tl$n == 1, " king", " kings"), " | ", ifelse(as.numeric(tot) < 1, fmt_reign(as.numeric(tot)), paste0(tl$yrs, ifelse(tl$yrs == 1, " yr", " yrs"))))
  tl$kl <- D$kl[match(tl$rowkey, as.character(D$rowkey))]
  ln <- do.call(rbind, lapply(split(D, D$rowkey, drop = TRUE), function(d) data.frame(y = d$y[1], x0 = 1, x1 = max(d$pos), kl = d$kl[1])))
  ylab <- stats::setNames(D$house[match(levels(D$rowkey), as.character(D$rowkey))], levels(D$rowkey))

  ggplot2::ggplot(D) +
    ggplot2::geom_segment(data = ln, ggplot2::aes(x = x0, xend = x1, y = y, yend = y), colour = PAL$rule, linewidth = 1.2) +
    ggiraph::geom_point_interactive(ggplot2::aes(x = pos, y = y, size = sz, fill = grp, alpha = a, tooltip = tip, data_id = id, onclick = click),
                                    shape = 21, colour = "#FFFFFF", stroke = 1.1) +
    ggplot2::geom_text(ggplot2::aes(x = pos, y = y, label = inlab, alpha = a), family = F_BODY, fontface = "bold", size = 3.2,
                       colour = ifelse(D$grp == "Family", "#FFFFFF", PAL$ink)) +
    ggplot2::geom_text(ggplot2::aes(x = pos, y = ly, label = short, vjust = lv, alpha = a), family = F_BODY, fontface = "bold", size = 3.1, colour = PAL$ink) +
    ggplot2::geom_text(data = tl, ggplot2::aes(x = x, y = y, label = lab), hjust = 0, family = F_BODY, fontface = "bold", size = 3.8, colour = PAL$ink) +
    ggplot2::facet_grid(kl ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_fill_manual(values = PAL$rel, name = NULL, drop = FALSE) +
    ggplot2::scale_size_area(max_size = 14.5, guide = "none") +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_y_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2])), labels = function(b) unname(ylab[levels(D$rowkey)][b]), expand = ggplot2::expansion(add = 0.45)) +
    ggplot2::scale_x_continuous(limits = c(-0.3, xr + 4.2), breaks = NULL, expand = c(0, 0)) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 7))) +
    ggplot2::labs(x = "Kings in order of succession  |  bubble size = years on the throne (number shown for 20+ years)", y = NULL) +
    theme_kings(13) +
    ggplot2::theme(legend.position = "top", legend.text = ggplot2::element_text(size = 13, face = "bold", colour = PAL$ink),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(size = 12, face = "bold", margin = ggplot2::margin(t = 6)),
                   axis.text.y = ggplot2::element_text(family = F_HEAD, face = "bold", size = 12, colour = PAL$ink),
                   strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 90, face = "bold", family = F_HEAD, size = 12, colour = "#FFFFFF"),
                   strip.background = ggplot2::element_rect(fill = PAL$ink_soft, colour = NA),
                   panel.spacing = ggplot2::unit(0.5, "lines"))
}
