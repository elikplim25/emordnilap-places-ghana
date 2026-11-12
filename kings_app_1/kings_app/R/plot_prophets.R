# ============================================================================
#  plot_prophets.R -- "Prophets during the reigns of kings"
#    view 1: one bar per king, bar length = number of named prophets in his reign
#    view 2: one bar per prophet, bar length = number of kings he served beside (coloured by verdict)
#  Bars are drawn with geom_rect_interactive (geom_col_interactive breaks with ggplot2 >= 4.0).
# ============================================================================
plot_prophet_bars <- function(K, tips, ctx) {
  D <- K[king_keep(K, ctx), ]
  if (!nrow(D)) return(ggplot2::ggplot() + ggplot2::theme_void())
  D <- D[order(D$id), ]
  D$kl <- factor(D$kingdom, levels = KINGDOM_LEVELS, labels = KINGDOM_SHORT[KINGDOM_LEVELS])
  D$row <- factor(D$label, levels = rev(D$label))
  D$y <- as.numeric(D$row)
  D$a <- if (is.null(ctx$sel)) 1 else ifelse(D$id == ctx$sel, 1, 0.3)
  D$click <- js_pick(D$id)
  D$verdict <- factor(D$verdict, levels = VERDICT_LEVELS)
  D$tip <- vapply(seq_len(nrow(D)), function(i) {
    r <- D[i, ]
    pr <- unlist(r$prophets)
    list_html <- if (length(pr)) paste0("<ul class=\"tt-list\">", paste0("<li>", esc(pr), "</li>", collapse = ""), "</ul>") else "<div class=\"tt-notes\">No named prophet recorded.</div>"
    sprintf("<div class=\"tt\"><div class=\"tt-name\">%s</div><div class=\"tt-badges\">%s <span class=\"tt-reign\">%s | %s</span></div><div class=\"tt-sec\">Prophets in his reign (%d)</div>%s%s<div class=\"tt-hint\">Click for the full profile</div></div>",
            esc(r$name), pill(r$verdict), esc(r$reign_txt), esc(r$span_txt), r$n_prophets, list_html,
            if (r$unnamed) "<div class=\"tt-notes\">+ one or more unnamed prophets</div>" else "")
  }, "")
  # prophet names written beside the bar, so the chart reads on its own (and in a downloaded image)
  D$lab <- vapply(seq_len(nrow(D)), function(i) {
    pr <- unlist(D$prophets[[i]])
    txt <- if (length(pr)) paste(pr, collapse = ", ") else if (D$unnamed[i]) "" else "none recorded"
    if (D$unnamed[i]) txt <- paste0(txt, if (nzchar(txt)) " + unnamed" else "unnamed prophet(s)")
    txt
  }, "")
  xmax <- max(D$n_prophets, 1)
  ggplot2::ggplot(D) +
    ggiraph::geom_rect_interactive(ggplot2::aes(xmin = 0, xmax = pmax(n_prophets, 0.04), ymin = y - 0.36, ymax = y + 0.36, fill = verdict, alpha = a,
                                                 tooltip = tip, data_id = id, onclick = click)) +
    ggplot2::geom_text(ggplot2::aes(x = n_prophets + 0.1, y = y, label = lab, alpha = a), hjust = 0, family = F_BODY, fontface = "bold", size = 3.5, colour = PAL$ink) +
    ggplot2::facet_grid(kl ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::scale_fill_manual(values = PAL$verdict, breaks = VERDICT_LEVELS, labels = VERDICT_LAB[VERDICT_LEVELS], name = NULL, drop = FALSE) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_y_continuous(breaks = seq_along(levels(D$row)), labels = levels(D$row), expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_x_continuous(breaks = 0:8, limits = c(0, xmax + 4.5), expand = c(0, 0)) +
    ggplot2::labs(x = "Named prophets who ministered during the reign (bar colour = verdict on the king)", y = NULL) +
    theme_kings(13) +
    ggplot2::theme(legend.position = "top", legend.text = ggplot2::element_text(size = 13, face = "bold", colour = PAL$ink),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(family = F_HEAD, face = "bold", size = 12, colour = PAL$ink),
                   strip.placement = "outside", panel.spacing = ggplot2::unit(0.5, "lines"),
                   strip.text.y.left = ggplot2::element_text(angle = 90, face = "bold", family = F_HEAD, size = 12, colour = "#FFFFFF"),
                   strip.background = ggplot2::element_rect(fill = PAL$ink_soft, colour = NA))
}

plot_prophet_ranks <- function(K, Pt, ctx) {
  Pt <- Pt[Pt$king_id %in% K$id[king_keep(K, ctx)], ]
  if (!nrow(Pt)) return(ggplot2::ggplot() + ggplot2::theme_void())
  Pt$verdict <- as.character(K$verdict[match(Pt$king_id, K$id)])
  Pt$king <- K$label[match(Pt$king_id, K$id)]
  tot <- table(Pt$prophet)
  first <- tapply(Pt$king_id, Pt$prophet, min)
  ord <- names(tot)[order(-as.numeric(tot), as.numeric(first[names(tot)]))]
  S <- as.data.frame(table(prophet = Pt$prophet, verdict = Pt$verdict), stringsAsFactors = FALSE); S <- S[S$Freq > 0, ]
  S$n <- S$Freq
  S$bar <- factor(S$prophet, levels = rev(ord))
  S$seg <- factor(S$verdict, levels = VERDICT_LEVELS)
  S <- stack_rects(S)
  S$tip <- vapply(seq_len(nrow(S)), function(i) {
    e <- Pt[Pt$prophet == S$prophet[i] & Pt$verdict == S$verdict[i], ]
    sprintf("<div class=\"tt\"><div class=\"tt-name\">%s</div><div class=\"tt-badges\"><span class=\"pill\" style=\"background:%s;color:%s\">%d %s king%s</span></div><div class=\"tt-notes\">%s</div></div>",
            esc(S$prophet[i]), PAL$verdict[[S$verdict[i]]], PAL$verdict_txt[[S$verdict[i]]], S$n[i], VERDICT_LAB[[S$verdict[i]]],
            ifelse(S$n[i] == 1, "", "s"), esc(paste(e$king, collapse = ", ")))
  }, "")
  T <- data.frame(y = seq_along(levels(S$bar)), x = as.numeric(tapply(S$x1, S$bar, max)))
  ggplot2::ggplot(S) +
    ggiraph::geom_rect_interactive(ggplot2::aes(xmin = x0, xmax = x1, ymin = y - 0.36, ymax = y + 0.36, fill = seg,
                                                 tooltip = tip, data_id = paste(prophet, verdict)), colour = "#FFFFFF", linewidth = 0.6) +
    ggplot2::geom_text(data = T, ggplot2::aes(x = x + 0.1, y = y, label = x), hjust = 0, family = F_HEAD, fontface = "bold", size = 3.9, colour = PAL$ink) +
    ggplot2::scale_fill_manual(values = PAL$verdict, breaks = intersect(VERDICT_LEVELS, as.character(S$seg)), labels = VERDICT_LAB[intersect(VERDICT_LEVELS, as.character(S$seg))], name = NULL) +
    ggplot2::scale_y_continuous(breaks = seq_along(levels(S$bar)), labels = levels(S$bar), expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_x_continuous(breaks = 0:12, expand = ggplot2::expansion(mult = c(0, 0.06))) +
    ggplot2::labs(x = "Number of kings he served beside (colour = verdict on those kings)", y = NULL) +
    theme_kings(13) +
    ggplot2::theme(legend.position = "top", legend.text = ggplot2::element_text(size = 13, face = "bold", colour = PAL$ink),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(family = F_HEAD, face = "bold", size = 12, colour = PAL$ink))
}
