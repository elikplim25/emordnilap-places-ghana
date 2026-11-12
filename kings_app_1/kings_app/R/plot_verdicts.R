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
    ggiraph::geom_point_interactive(ggplot2::aes(size = years, tooltip = tip, data_id = id, onclick = click), shape = 21, stroke = 1.2,
                                    fill = D$col, colour = "#FFFFFF", alpha = D$a) +
    ggplot2::geom_text(ggplot2::aes(x = vx + 0.28, label = reign_txt), hjust = 0, family = F_BODY, fontface = "bold", size = 3.5, colour = PAL$ink) +
    ggplot2::facet_grid(hs ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_size_continuous(range = c(3.2, 9.5)) +
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
