# ============================================================================
#  plot_ties.R -- "Ties & Alliances": how many marriages / alliances / wars / murders touched each king
# ============================================================================
plot_ties <- function(K, L, tips, ctx, scope = c("between", "within")) {
  L <- L[L$scope %in% scope, ]
  E <- rbind(data.frame(id = L$from_id, other = L$to_id, type = L$type, label = L$label, ref = L$ref),
             data.frame(id = L$to_id, other = L$from_id, type = L$type, label = L$label, ref = L$ref))
  E <- E[E$id %in% K$id[king_keep(K, ctx)], ]
  if (!nrow(E)) return(list(plot = ggplot2::ggplot() + ggplot2::theme_void(), n = 0))
  tot <- table(E$id)
  ids <- as.integer(names(tot)); ids <- ids[order(-as.numeric(tot), ids)]
  S <- as.data.frame(table(id = E$id, type = E$type), stringsAsFactors = FALSE); S <- S[S$Freq > 0, ]
  S$id <- as.integer(S$id); S$n <- S$Freq
  S$king <- K$label[match(S$id, K$id)]
  S$bar <- factor(S$king, levels = rev(K$label[match(ids, K$id)]))
  S$seg <- factor(S$type, levels = names(PAL$tie))
  S <- stack_rects(S)
  S$tip <- vapply(seq_len(nrow(S)), function(i) {
    e <- E[E$id == S$id[i] & E$type == S$type[i], ]
    sprintf("<div class=\"tt\"><div class=\"tt-name\">%s</div><div class=\"tt-badges\"><span class=\"pill\" style=\"background:%s;color:#fff\">%s (%d)</span></div>%s<div class=\"tt-hint\">Click for the full profile</div></div>",
            esc(S$king[i]), PAL$tie[[S$type[i]]], esc(S$type[i]), nrow(e),
            paste(sprintf("<div class=\"tt-notes\">%s <b>(%s)</b></div>", esc(e$label), esc(e$ref)), collapse = ""))
  }, "")
  S$click <- js_pick(S$id)
  S$a <- if (is.null(ctx$sel)) 1 else ifelse(S$id == ctx$sel, 1, 0.3)
  T <- data.frame(y = seq_along(levels(S$bar)), x = as.numeric(tapply(S$x1, S$bar, max)))
  p <- ggplot2::ggplot(S) +
    ggiraph::geom_rect_interactive(ggplot2::aes(xmin = x0, xmax = x1, ymin = y - 0.35, ymax = y + 0.35, fill = seg, alpha = a,
                                                 tooltip = tip, data_id = paste(id, type), onclick = click), colour = "#FFFFFF", linewidth = 0.8) +
    ggplot2::geom_text(data = T, ggplot2::aes(x = x + 0.12, y = y, label = x), hjust = 0, family = F_HEAD, fontface = "bold", size = 3.9, colour = PAL$ink) +
    ggplot2::scale_fill_manual(values = PAL$tie, name = NULL, drop = FALSE) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_y_continuous(breaks = seq_along(levels(S$bar)), labels = levels(S$bar), expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_x_continuous(breaks = 0:12, expand = ggplot2::expansion(mult = c(0, 0.06))) +
    ggplot2::labs(x = "Number of ties", y = NULL) +
    theme_kings(13) +
    ggplot2::theme(legend.position = "top", legend.text = ggplot2::element_text(size = 13, face = "bold", colour = PAL$ink),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(family = F_HEAD, face = "bold", size = 12, colour = PAL$ink))
  list(plot = p, n = length(ids))
}
