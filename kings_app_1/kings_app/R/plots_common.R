# ============================================================================
#  plots_common.R -- theme, tooltips and helpers shared by every chart
#  Font: Archivo everywhere (loaded in the browser from Google Fonts). No italics anywhere.
# ============================================================================
F_HEAD <- "Archivo"
F_BODY <- "Archivo"
F_DISPLAY <- F_HEAD; F_NUM <- F_BODY

theme_kings <- function(base = 13) {
  ggplot2::theme_minimal(base_size = base, base_family = F_BODY) +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = PAL$card, colour = NA),
      panel.background  = ggplot2::element_rect(fill = PAL$card, colour = NA),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_line(colour = "#E6DEBF", linewidth = 0.4),
      text              = ggplot2::element_text(colour = PAL$ink),
      axis.text         = ggplot2::element_text(colour = PAL$ink, size = base),
      axis.title        = ggplot2::element_text(colour = PAL$ink, size = base, face = "bold"),
      legend.position   = "none",
      plot.margin       = ggplot2::margin(6, 12, 6, 6)
    )
}

js_pick    <- function(id) sprintf('Shiny.setInputValue("king_pick", %d, {priority: "event"});', as.integer(id))
js_house   <- function(h)  sprintf('Shiny.setInputValue("house_pick", "%s", {priority: "event"});', gsub("\"", "", h))
js_prophet <- function(p)  sprintf('Shiny.setInputValue("prophet_pick", "%s", {priority: "event"});', gsub("\"", "", p))

hover_css <- function() {
  ggiraph::girafe_css(
    css  = "stroke:#001219;stroke-width:2.4px;cursor:pointer;",
    text = "stroke:none;cursor:pointer;",
    line = "stroke:#001219;stroke-width:2.6px;cursor:pointer;",
    point = "stroke:#001219;stroke-width:2px;cursor:pointer;"
  )
}

make_girafe <- function(p, w, h, hover = TRUE) {
  opts <- list(
    ggiraph::opts_sizing(rescale = TRUE, width = 1),
    ggiraph::opts_tooltip(css = "", opacity = 1, delay_mouseover = 60, delay_mouseout = 60, use_fill = FALSE),
    ggiraph::opts_hover(css = hover_css()),
    ggiraph::opts_hover_inv(css = "opacity:0.35;"),
    ggiraph::opts_toolbar(position = "topright", saveaspng = FALSE,
                          hidden = c("lasso_select", "lasso_deselect", "zoom_onoff", "zoom_rect", "zoom_reset", "selection"))
  )
  if (!hover) opts <- opts[-c(3, 4)]
  ggiraph::girafe(ggobj = p, width_svg = w, height_svg = h, options = opts)
}

esc <- function(x) htmltools::htmlEscape(x, attribute = TRUE)

pill <- function(v) {
  v <- as.character(v)
  sprintf('<span class="pill" style="background:%s;color:%s">%s</span>', PAL$verdict[[v]], PAL$verdict_txt[[v]], ifelse(v == "N/A", "Not rated", v))
}
trunc_txt <- function(x, n = 250) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "..."), x)

build_tips <- function(K, S) {
  tips <- vapply(seq_len(nrow(K)), function(i) {
    r <- K[i, ]
    rows <- character()
    add <- function(lab, val) if (!is.na(val) && nzchar(val)) rows <<- c(rows, sprintf("<tr><td>%s</td><td>%s</td></tr>", lab, esc(val)))
    if (!is.na(r$age_start)) add("Crowned at", paste("age", r$age_start))
    if (!is.na(r$age_end))   add("Died at", paste("age", r$age_end))
    add("Related to previous king", ifelse(r$rel_prev == "None", "First king", r$rel_prev))
    s <- S[S$id == r$id & S$kind == "primary", ]
    if (nrow(s)) add("Synchronism", s$text[1])
    add(paste0("Prophets (", r$n_prophets, ")"),
        ifelse(r$n_prophets > 0, paste0(paste(unlist(r$prophets), collapse = ", "), ifelse(r$unnamed, " + unnamed", "")),
               ifelse(r$unnamed, "unnamed prophet(s)", "none recorded")))
    sprintf(paste0(
      "<div class=\"tt\"><div class=\"tt-name\">%s</div>",
      "<div class=\"tt-meta\">%s | %s</div>",
      "<div class=\"tt-badges\">%s <span class=\"tt-reign\">%s | %s</span></div>",
      "<table class=\"tt-grid\">%s</table>",
      "<div class=\"tt-refs\"><b>Read:</b> %s</div>",
      "<div class=\"tt-notes\">%s</div>",
      "<div class=\"tt-hint\">Click for the full profile</div></div>"),
      esc(r$name), esc(KINGDOM_LABELS[[as.character(r$kingdom)]]), esc(r$house),
      pill(r$verdict), esc(r$reign_txt), esc(r$span_txt),
      paste(rows, collapse = ""), esc(r$refs), esc(trunc_txt(r$notes)))
  }, character(1))
  stats::setNames(tips, K$id)
}

# ---------------------------------------------------------------------------
# context shared by every chart: which verdicts are on, which dynasty, which king is picked
#   ctx = list(verdicts = , dyn = (house names or NULL), sel = (id or NULL), rel = (ids))
# ---------------------------------------------------------------------------
make_ctx <- function(verdicts = VERDICT_LEVELS, dyn = NULL, sel = NULL, rel = integer()) list(verdicts = verdicts, dyn = dyn, sel = sel, rel = rel)

king_keep <- function(K, ctx) {            # kings that pass the verdict + dynasty filters
  ok <- as.character(K$verdict) %in% ctx$verdicts
  if (length(ctx$dyn)) ok <- ok & K$house %in% ctx$dyn
  ok
}
king_active <- function(K, ctx) {          # ... and, when a king is picked, only he and his contemporaries
  on <- king_keep(K, ctx)
  if (!is.null(ctx$sel)) on <- on & (K$id %in% c(ctx$sel, ctx$rel))
  on
}

darker <- function(hex, amt = 0.35) {
  m <- grDevices::col2rgb(hex); grDevices::rgb(t(m * (1 - amt)), maxColorValue = 255)
}

# horizontal stacked bars drawn as rectangles. (geom_col_interactive breaks with ggplot2 >= 4.0 + ggiraph 0.8.x:
# "unused arguments (lineend, linejoin)", which Shiny shows as "Error: [object Object]".)
#   S: one row per (bar, segment); `bar` = factor (bottom-to-top order), `seg` = factor (left-to-right order), `n` = length
stack_rects <- function(S) {
  S <- S[order(S$bar, S$seg), ]
  S$x1 <- ave(S$n, S$bar, FUN = cumsum); S$x0 <- S$x1 - S$n
  S$y <- as.numeric(S$bar)
  S
}
