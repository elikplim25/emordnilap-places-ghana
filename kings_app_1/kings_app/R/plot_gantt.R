# ============================================================================
#  plot_gantt.R -- "The Split": one row per king, one bar per reign, one shared BC axis
#  three stacked panels (United -> Israel -> Judah) so you can SEE the split and who reigned at the same time
# ============================================================================
rel_group <- function(K) ifelse(K$rel_class == "First king", "First king", ifelse(K$related, "Family", "Not related"))

plot_gantt <- function(K, tips, ctx, color_by = c("verdict", "relationship", "dynasty")) {
  color_by <- match.arg(color_by)
  D <- K[order(K$id), ]
  D$active <- king_active(D, ctx)
  D$a <- ifelse(D$active, 1, 0.22)
  D$tip <- tips[as.character(D$id)]; D$click <- js_pick(D$id)
  D$kl <- factor(D$kingdom, levels = KINGDOM_LEVELS, labels = KINGDOM_SHORT[KINGDOM_LEVELS])
  D$row <- factor(D$label, levels = rev(D$label))
  D$e <- pmin(D$end_bc, D$start_bc - 0.5)                      # a reign under 6 months is drawn half a year wide so it stays visible and clickable
  D$grp <- switch(color_by,
                  verdict = ifelse(as.character(D$verdict) == "N/A", "Not rated", as.character(D$verdict)),
                  relationship = rel_group(D),
                  dynasty = D$house)
  vals <- switch(color_by,
                 verdict = c(Good = PAL$verdict[["Good"]], Mixed = PAL$verdict[["Mixed"]], Evil = PAL$verdict[["Evil"]], `Not rated` = PAL$verdict[["N/A"]]),
                 relationship = PAL$rel,
                 dynasty = stats::setNames(DYN_COLS[seq_along(unique(D$house))], unique(D$house)))
  D$grp <- factor(D$grp, levels = names(vals))
  D$len <- reign_short(D$years)

  ev <- data.frame(x = c(931, 722, 586), hj = c(0, 1, 1), nudge = c(-3, 3, 3),
                   kl = factor(KINGDOM_SHORT[KINGDOM_LEVELS], levels = levels(D$kl)),
                   lab = c("931 BC  The kingdom splits", "722 BC  Samaria falls", "586 BC  Jerusalem falls"))
  p <- ggplot2::ggplot()
  if (!is.null(ctx$sel) && length(ctx$sel) == 1) {
    r <- D[D$id == ctx$sel, ]
    p <- p + ggplot2::geom_rect(data = data.frame(xmin = r$start_bc, xmax = r$e), ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                                fill = PAL$verdict[["Mixed"]], alpha = 0.18)
  }
  p <- p +
    ggplot2::geom_vline(data = data.frame(x = c(931, 722, 586)), ggplot2::aes(xintercept = x), colour = PAL$ink, linetype = "22", linewidth = 0.5) +
    ggiraph::geom_rect_interactive(data = D, ggplot2::aes(xmin = start_bc, xmax = e, ymin = as.numeric(row) - 0.36, ymax = as.numeric(row) + 0.36,
                                                           fill = grp, alpha = a, tooltip = tip, data_id = id, onclick = click), colour = "#FFFFFF", linewidth = 0.3) +
    ggplot2::geom_text(data = D, ggplot2::aes(x = e - 3, y = as.numeric(row), label = len, alpha = a), hjust = 0, family = F_BODY, fontface = "bold",
                       size = 3.6, colour = PAL$ink) +
    ggplot2::geom_text(data = ev, ggplot2::aes(x = x + nudge, y = Inf, label = lab, hjust = hj), vjust = 1.4, family = F_HEAD, fontface = "bold", size = 3.7, colour = PAL$ink) +
    ggplot2::facet_grid(kl ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_fill_manual(values = vals, name = NULL, drop = FALSE) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_y_continuous(breaks = seq_along(levels(D$row)), labels = levels(D$row), expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_x_reverse(limits = c(1065, 520), breaks = seq(1000, 600, -100), labels = paste(seq(1000, 600, -100), "BC"), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_kings(13) +
    ggplot2::theme(legend.position = "top", legend.text = ggplot2::element_text(size = 13, colour = PAL$ink, face = "bold"),
                   legend.key.size = ggplot2::unit(0.5, "cm"),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(0.5, "lines"),
                   axis.text.y = ggplot2::element_text(size = 12, face = "bold", colour = PAL$ink),
                   axis.text.x = ggplot2::element_text(size = 12, face = "bold"),
                   strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 90, face = "bold", family = F_HEAD, size = 12, colour = "#FFFFFF"),
                   strip.background = ggplot2::element_rect(fill = PAL$ink_soft, colour = NA))
  list(plot = p, n = nrow(D))
}
