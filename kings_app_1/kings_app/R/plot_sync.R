# ============================================================================
#  plot_sync.R -- "Houses & Kin": the synchronisms between Israel and Judah
#  A two-column chronicle: time runs DOWN (931 BC at the top, 586 BC at the bottom),
#  Israel's kings on the left, Judah's on the right. Every line is one synchronism in the text:
#  "X began to reign in the Nth year of Y" joins the START of X's reign to year N of Y's reign.
#  Solid line = fits the reference chronology; dashed = differs by more than 3 years (a known puzzle).
#  The narrative boxes mark the two exiles: Israel to Assyria (722 BC), Judah to Babylon (586 BC).
# ============================================================================

# push labels apart so none overlap (positions in years BC, top = largest)
spread_labels <- function(y, gap) {
  o <- order(y, decreasing = TRUE); p <- y[o]
  for (i in seq_along(p)[-1]) if (p[i] > p[i - 1] - gap) p[i] <- p[i - 1] - gap
  out <- numeric(length(y)); out[o] <- p; out
}

plot_sync <- function(K, S, tips, ctx) {
  # each column: the main lane holds a king's SOLE reign; a thin inner strip holds years he shared the throne
  # (co-regency or rival reign, from data/chronology_reference.csv). Links attach to the strip's inner edge.
  X <- list(Northern = c(m0 = 3.10, m1 = 3.80, s0 = 3.84, s1 = 3.98, lab = 2.98, edge = 3.98, hj = 1),
            Southern = c(m0 = 6.20, m1 = 6.90, s0 = 6.02, s1 = 6.16, lab = 7.02, edge = 6.02, hj = 0))
  D <- K[K$kingdom != "United", ]; D <- D[order(D$id), ]
  D$active <- king_active(D, ctx)
  D$a <- ifelse(D$active, 1, 0.25)
  D$tip <- tips[as.character(D$id)]; D$click <- js_pick(D$id)
  kg <- as.character(D$kingdom)
  xv <- function(nm) vapply(kg, function(k) X[[k]][[nm]], 0)
  D$x0 <- xv("m0"); D$x1 <- xv("m1")
  D$sole <- ifelse(is.na(D$sole_start_bc), D$start_bc, D$sole_start_bc)
  D$y0 <- D$sole; D$y1 <- pmin(D$end_bc, D$sole - 1.2)                  # short reigns stay visible
  D$shade <- ave(D$id, D$kingdom, FUN = function(v) rank(v) %% 2)        # alternate two tints down a column
  D$fill <- ifelse(D$shade == 1, PAL$kingdom[kg], vapply(PAL$kingdom[kg], darker, "", amt = 0.35))
  C <- D[D$start_bc > D$sole, ]                                          # the shared years
  C$tip <- vapply(seq_len(nrow(C)), function(i) sub("<div class=\"tt-hint\">", paste0("<div class=\"tt-notes\"><b>Shared throne ", C$start_bc[i], "-", C$sole[i],
                   " BC</b> (co-regency or rival reign)</div><div class=\"tt-hint\">"), C$tip[i], fixed = TRUE), "")
  # labels beside each column, nudged apart, with a leader line back to the reign
  D$ly <- 0
  for (k in c("Northern", "Southern")) { i <- kg == k; D$ly[i] <- spread_labels((D$start_bc[i] + D$y1[i]) / 2, gap = 4.6) }
  D$lx <- xv("lab"); D$hj <- xv("hj")
  D$ex <- ifelse(kg == "Northern", D$x0 - 0.03, D$x1 + 0.03)
  D$lab <- paste0(D$label, "  ", reign_short(D$years))

  # synchronism links ---------------------------------------------------------------------------------------
  rev <- S$id == 5 & S$kind == "primary"                               # "Asa began in Jeroboam I's 20th year"
  L <- data.frame(begin = ifelse(rev, S$sync_partner_id, S$id), other = ifelse(rev, S$id, S$sync_partner_id),
                  yb = ifelse(rev, S$partner_start, S$own_start), yo = S$claimed_bc, n = S$sync_year,
                  agrees = S$agrees, gap = S$gap_years, ref = S$ref, kind = S$kind)
  L$bk <- as.character(K$kingdom[match(L$begin, K$id)])
  L$xb <- ifelse(L$bk == "Northern", X$Northern[["edge"]], X$Southern[["edge"]])
  L$xo <- ifelse(L$bk == "Northern", X$Southern[["edge"]], X$Northern[["edge"]])
  L$bn <- K$label[match(L$begin, K$id)]; L$on <- K$label[match(L$other, K$id)]
  L$bkl <- KINGDOM_LABELS[L$bk]; L$okl <- KINGDOM_LABELS[as.character(K$kingdom[match(L$other, K$id)])]
  on_ids <- K$id[king_keep(K, ctx)]
  L$a <- ifelse(L$begin %in% on_ids & L$other %in% on_ids, 1, 0.15)
  if (!is.null(ctx$sel)) L$a <- ifelse(L$begin == ctx$sel | L$other == ctx$sel, 1, 0.12)
  L$col <- ifelse(L$agrees, PAL$ink, PAL$verdict[["Mixed"]])
  L$lt <- ifelse(L$agrees, "solid", "22")
  L$tip <- sprintf(paste0("<div class=\"tt\"><div class=\"tt-name\">%s began to reign</div>",
                          "<div class=\"tt-meta\">%s</div>",
                          "<div class=\"tt-notes\">in the <b>%s year</b> of %s (%s).</div>",
                          "<div class=\"tt-notes\">%s</div>%s</div>"),
                   esc(L$bn), esc(L$bkl), ord(L$n), esc(L$on), esc(L$okl),
                   ifelse(L$agrees, "This fits the reference chronology.",
                          paste0("The reference chronology differs by about ", abs(round(L$gap)), " years here, usually explained by co-regencies or different ways of counting a king's first year.")),
                   ifelse(is.na(L$ref), "", paste0("<div class=\"tt-refs\"><b>Read:</b> ", esc(L$ref), "</div>")))
  L$nx <- L$xo + ifelse(L$xo < L$xb, 0.1, -0.1)                       # the year number sits by the "Nth year" end
  L$ny <- L$yo; for (sd in unique(L$xo)) { i <- L$xo == sd; L$ny[i] <- spread_labels(L$yo[i], gap = 3.3) }

  # narrative -----------------------------------------------------------------------------------------------
  vi <- table(factor(K$verdict[K$kingdom == "Northern"], VERDICT_LEVELS)); vj <- table(factor(K$verdict[K$kingdom == "Southern"], VERDICT_LEVELS))
  isr <- paste0("722 BC  ISRAEL IS TAKEN BY ASSYRIA\n",
                "Shalmaneser V of Assyria besieged Samaria for three years.\n",
                "It fell in the 9th year of Hoshea, and Israel was carried\n",
                "away to Assyria and scattered (2 Kings 17:3-6; 18:9-11).\n",
                "The northern kingdom lasted about 209 years under ", sum(K$kingdom == "Northern"), " kings,\n",
                vi[["Evil"]], " of them judged evil. Judah carried on alone.")
  jud <- paste0("586 BC  JUDAH IS TAKEN BY BABYLON\n",
                "Nebuchadnezzar of Babylon besieged Jerusalem in the reign of Zedekiah. The city and the temple were burned,\n",
                "Zedekiah was blinded and the people were carried to Babylon (2 Kings 25:1-21). Judah outlived Israel by 136 years:\n",
                "about 345 years under ", sum(K$kingdom == "Southern"), " rulers, ", vj[["Good"]], " judged good and ", vj[["Evil"]], " evil.")
  notes <- data.frame(y = c(705, 601), x = 5.92,
                      lab = c("701 BC  Sennacherib of Assyria\ninvades Judah; Jerusalem is\nspared (2 Kings 18:13-19:36)",
                              "597 BC  Jehoiachin is\ncarried to Babylon\n(2 Kings 24:10-16)"))
  yrs <- seq(900, 600, -50)

  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = yrs, colour = "#EFE8CC", linewidth = 0.4) +
    ggplot2::annotate("text", x = -0.75, y = yrs, label = paste(yrs, "BC"), hjust = 0, vjust = -0.4, family = F_BODY, fontface = "bold", size = 3.2, colour = PAL$ink_soft) +
    # Israel in exile: the empty column after 722
    ggplot2::annotate("rect", xmin = 3.10, xmax = 3.98, ymin = 586, ymax = 722, fill = "#E9E6F2", colour = NA) +
    ggplot2::annotate("text", x = 3.54, y = 612, label = "In exile in Assyria", angle = 90, family = F_HEAD, fontface = "bold", size = 3.6, colour = PAL$kingdom[["Northern"]]) +
    ggplot2::annotate("segment", x = 1.2, xend = 3.98, y = 722, yend = 722, colour = PAL$verdict[["Evil"]], linewidth = 1.1) +
    ggplot2::annotate("segment", x = -0.6, xend = 9.45, y = 586, yend = 586, colour = PAL$verdict[["Evil"]], linewidth = 1.1) +
    # links
    ggiraph::geom_segment_interactive(data = L, ggplot2::aes(x = xb, xend = xo, y = yb, yend = yo, alpha = a, tooltip = tip, data_id = paste0("s", seq_len(nrow(L)))),
                                      colour = L$col, linetype = L$lt, linewidth = 0.9) +
    ggplot2::geom_point(data = L, ggplot2::aes(x = xb, y = yb, alpha = a), size = 2.4, colour = PAL$ink) +
    ggplot2::geom_point(data = L, ggplot2::aes(x = xo, y = yo, alpha = a), size = 2.6, shape = 21, fill = "#FFFFFF", colour = L$col, stroke = 1.2) +
    ggplot2::geom_label(data = L, ggplot2::aes(x = nx, y = ny, label = n, alpha = a, hjust = ifelse(xo < xb, 0, 1)), family = F_HEAD, fontface = "bold", size = 3, colour = L$col,
                        fill = "#FFFFFF", linewidth = 0, label.padding = ggplot2::unit(0.08, "lines")) +
    # reigns
    ggiraph::geom_rect_interactive(data = C, ggplot2::aes(xmin = ifelse(kingdom == "Northern", X$Northern[["s0"]], X$Southern[["s0"]]), xmax = ifelse(kingdom == "Northern", X$Northern[["s1"]], X$Southern[["s1"]]),
                                                           ymin = sole, ymax = start_bc, alpha = a, tooltip = tip, data_id = id, onclick = click),
                                   fill = ifelse(C$kingdom == "Northern", "#8E93D6", "#6CCB9B"), colour = NA) +
    ggiraph::geom_rect_interactive(data = D, ggplot2::aes(xmin = x0, xmax = x1, ymin = y1, ymax = y0, alpha = a, tooltip = tip, data_id = id, onclick = click),
                                   fill = D$fill, colour = "#FFFFFF", linewidth = 0.35) +
    ggplot2::geom_segment(data = D, ggplot2::aes(x = lx + ifelse(hj == 1, 0.03, -0.03), xend = ex, y = ly, yend = (y0 + y1) / 2, alpha = a), colour = PAL$rule, linewidth = 0.4) +
    ggiraph::geom_text_interactive(data = D, ggplot2::aes(x = lx, y = ly, label = lab, hjust = hj, alpha = a, tooltip = tip, data_id = id, onclick = click),
                                   family = F_BODY, fontface = "bold", size = 3.3, colour = PAL$ink) +
    # headings and narrative
    ggplot2::annotate("text", x = c(3.45, 6.55), y = 944, label = c("ISRAEL (North)", "JUDAH (South)"), family = F_HEAD, fontface = "bold", size = 5,
                      colour = c(PAL$kingdom[["Northern"]], PAL$kingdom[["Southern"]])) +
    ggplot2::annotate("text", x = 5, y = 936.5, label = "931 BC  the kingdom splits", family = F_HEAD, fontface = "bold", size = 3.6, colour = PAL$ink) +
    ggplot2::geom_text(data = notes, ggplot2::aes(x = x, y = y, label = lab), hjust = 1, lineheight = 0.95, family = F_BODY, fontface = "bold", size = 3.1, colour = PAL$ink) +
    ggplot2::annotate("label", x = -0.6, y = 689, label = isr, hjust = 0, vjust = 1, lineheight = 1.05, family = F_BODY, fontface = "bold", size = 3.45,
                      colour = PAL$ink, fill = "#FFFFFF", linewidth = 0.8, label.padding = ggplot2::unit(0.6, "lines"), label.r = ggplot2::unit(0.08, "lines")) +
    ggplot2::annotate("label", x = -0.6, y = 580, label = jud, hjust = 0, vjust = 1, lineheight = 1.05, family = F_BODY, fontface = "bold", size = 3.45,
                      colour = PAL$ink, fill = "#FFFFFF", linewidth = 0.8, label.padding = ggplot2::unit(0.6, "lines"), label.r = ggplot2::unit(0.08, "lines")) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_x_continuous(limits = c(-0.8, 9.6), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(538, 950), expand = c(0, 0)) +
    ggplot2::theme_void(base_family = F_BODY) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = PAL$card, colour = NA), plot.margin = ggplot2::margin(4, 4, 4, 4))
}
