# ============================================================================
#  plot_lives.R -- "Lives & Reigns"
#    * dumbbell : age when crowned -> age at death (only where the Bible says)
#    * lollipop : years on the throne, for every one of the 43 rulers
# ============================================================================
plot_dumbbell <- function(K, tips, ctx, show_partial = TRUE) {
  D <- K[!is.na(K$age_start) | !is.na(K$age_end), ]
  if (!show_partial) D <- D[!is.na(D$age_start) & !is.na(D$age_end), ]
  D <- D[order(D$id), ]
  D$row <- rev(seq_len(nrow(D)))
  D$has_end <- !is.na(D$age_end)
  D$end_est <- ifelse(D$has_end, D$age_end, D$age_start + ceiling(D$years))
  D$active <- king_active(D, ctx)
  D$a <- ifelse(D$active, 1, 0.22)
  D$col <- PAL$verdict[as.character(D$verdict)]
  D$tip <- tips[as.character(D$id)]
  D$click <- js_pick(D$id)
  full <- D[D$has_end, ]; part <- D[!D$has_end, ]
  ymax <- nrow(D) + 0.6

  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = c(18, 30, 50), colour = "#D8CDA6", linewidth = 0.5)
  if (nrow(full)) p <- p + ggiraph::geom_segment_interactive(
      data = full, ggplot2::aes(x = age_start, xend = age_end, y = row, yend = row, tooltip = tip, data_id = id, onclick = click),
      colour = full$col, alpha = full$a * 0.9, linewidth = 4.4, lineend = "round")
  if (nrow(part)) p <- p + ggiraph::geom_segment_interactive(
      data = part, ggplot2::aes(x = age_start, xend = end_est, y = row, yend = row, tooltip = tip, data_id = id, onclick = click),
      colour = part$col, alpha = part$a * 0.7, linewidth = 1.8, linetype = "22")
  st <- D[!is.na(D$age_start), ]
  p <- p +
    ggiraph::geom_point_interactive(data = st, ggplot2::aes(x = age_start, y = row, tooltip = tip, data_id = id, onclick = click),
                                    shape = 21, size = 9, stroke = 1.8, fill = "#FFFFFF", colour = st$col, alpha = st$a) +
    ggiraph::geom_text_interactive(data = st, ggplot2::aes(x = age_start, y = row, label = age_start, tooltip = tip, data_id = id, onclick = click),
                                   family = F_HEAD, fontface = "bold", size = 3.5, colour = PAL$ink, alpha = st$a)
  if (nrow(full)) p <- p +
    ggiraph::geom_point_interactive(data = full, ggplot2::aes(x = age_end, y = row, tooltip = tip, data_id = id, onclick = click),
                                    shape = 21, size = 9, stroke = 1.8, fill = full$col, colour = full$col, alpha = full$a) +
    ggiraph::geom_text_interactive(data = full, ggplot2::aes(x = age_end, y = row, label = age_end, tooltip = tip, data_id = id, onclick = click),
                                   family = F_HEAD, fontface = "bold", size = 3.5, colour = ifelse(full$verdict == "Mixed", PAL$ink, "#FFFFFF"), alpha = full$a)
  if (nrow(part)) p <- p + ggiraph::geom_point_interactive(
      data = part, ggplot2::aes(x = end_est, y = row, tooltip = tip, data_id = id, onclick = click),
      shape = 21, size = 3.6, stroke = 1.3, fill = "#FFFFFF", colour = part$col, alpha = part$a)

  D$rx <- D$end_est + 2.6
  D$rtxt <- ifelse(D$has_end, paste0("reigned ", D$reign_txt), paste0("reigned ", D$reign_txt, " (death age not recorded)"))
  p <- p +
    ggplot2::geom_text(data = D, ggplot2::aes(x = rx, y = row, label = rtxt), hjust = 0, family = F_BODY, fontface = "bold",
                       size = 3.8, colour = PAL$ink, alpha = D$a) +
    ggplot2::scale_y_continuous(breaks = D$row, labels = D$label, expand = ggplot2::expansion(add = c(0.7, 0.9))) +
    ggplot2::scale_x_continuous(limits = c(0, 100), breaks = seq(0, 80, 10), expand = c(0, 0)) +
    ggplot2::labs(x = "Age (years)", y = NULL) +
    theme_kings(13) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(family = F_HEAD, face = "bold", size = 12.5, colour = PAL$ink))
  list(plot = p, n = nrow(D), n_full = nrow(full))
}

plot_reigns <- function(K, tips, ctx, order_by = c("time", "length")) {
  order_by <- match.arg(order_by)
  D <- K
  D$active <- king_active(D, ctx)
  D$a <- ifelse(D$active, 1, 0.22)
  D$col <- PAL$verdict[as.character(D$verdict)]
  D$tip <- tips[as.character(D$id)]
  D$click <- js_pick(D$id)
  D$kfac <- factor(D$kingdom, levels = KINGDOM_LEVELS, labels = KINGDOM_SHORT[KINGDOM_LEVELS])
  D <- if (order_by == "time") D[order(D$kfac, D$id), ] else D[order(D$kfac, -D$years, D$id), ]
  D$lab <- factor(D$label, levels = rev(unique(D$label)))
  D$x <- pmax(D$years, 0.35)
  ggplot2::ggplot(D, ggplot2::aes(y = lab)) +
    ggiraph::geom_segment_interactive(ggplot2::aes(x = 0, xend = x, yend = lab, tooltip = tip, data_id = id, onclick = click),
                                      colour = D$col, alpha = D$a * 0.85, linewidth = 2, lineend = "round") +
    ggiraph::geom_point_interactive(ggplot2::aes(x = x, tooltip = tip, data_id = id, onclick = click),
                                    colour = D$col, fill = D$col, shape = 21, size = 4.2, alpha = D$a) +
    ggplot2::geom_text(ggplot2::aes(x = x + 1.2, label = reign_txt), hjust = 0, family = F_BODY, fontface = "bold",
                       size = 3.8, colour = PAL$ink, alpha = D$a) +
    ggplot2::facet_grid(kfac ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.16)), breaks = seq(0, 50, 10)) +
    ggplot2::labs(x = "Years on the throne", y = NULL) +
    theme_kings(13) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(family = F_HEAD, face = "bold", size = 11, colour = PAL$ink),
                   strip.text.y = ggplot2::element_text(family = F_HEAD, face = "bold", angle = -90, size = 12, colour = "#FFFFFF"),
                   strip.background = ggplot2::element_rect(fill = PAL$ink_soft, colour = NA),
                   panel.spacing = ggplot2::unit(0.5, "lines"))
}
