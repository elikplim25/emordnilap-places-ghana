# ============================================================================
#  THE KINGS OF ISRAEL  --  an explorer for every ruler from Saul to Zedekiah
#  Run:  shiny::runApp()   (in RStudio: open this file and press "Run App")
#  Needs: shiny, ggplot2, ggiraph, dplyr, readxl, htmltools
#  Colours: {ltc} palettes "heatmap0" and "hat" (hex values are stored in R/data.R, so the {ltc} package is not required)
# ============================================================================
need <- c("shiny", "ggplot2", "ggiraph", "dplyr", "readxl", "htmltools")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) stop("Please install: install.packages(c(", paste0('"', miss, '"', collapse = ", "), "))", call. = FALSE)
library(shiny); library(ggplot2); library(ggiraph); library(dplyr)
# everything in R/ is sourced automatically by Shiny (>= 1.5)

KINGS <- load_kings("data")
SYNC  <- build_sync(KINGS, "data")
OVER  <- build_overlaps(KINGS)
LINKS <- load_links(KINGS, "data")
PROPH <- build_prophets(KINGS)
TIPS  <- build_tips(KINGS, SYNC)
N_K <- table(KINGS$kingdom)
HOUSES <- unique(KINGS$house[order(KINGS$id)])
HOUSE_CHOICES <- c(list(`All dynasties` = ""),
                   split(HOUSES, factor(KINGDOM_LABELS[as.character(KINGS$kingdom[match(HOUSES, KINGS$house)])], levels = unname(KINGDOM_LABELS))))

bg_links <- function(refs) {
  parts <- trimws(strsplit(refs, ";")[[1]]); book <- ""
  full <- character(length(parts))
  for (i in seq_along(parts)) {                       # "8:16, 25-29" inherits the book of the previous reference
    b <- sub("\\s*[0-9]+([:.-].*)?$", "", parts[i])
    if (grepl("[A-Za-z]", b)) book <- b
    full[i] <- if (grepl("[A-Za-z]", parts[i])) parts[i] else paste(book, parts[i])
  }
  tagList(lapply(seq_along(parts), function(i) tags$a(parts[i], href = paste0("https://www.biblegateway.com/passage/?search=", URLencode(full[i], reserved = TRUE), "&version=ESV"),
                                          target = "_blank", class = "ref")))
}

stat <- function(num, lab, sub = NULL, col = PAL$ink)
  div(class = "stat", div(class = "stat-num", style = paste0("color:", col), num), div(class = "stat-lab", lab), if (!is.null(sub)) div(class = "stat-sub", sub))

# every chart, tile and card sits in a "shot": a block with its own "Download PNG" button (see www/shot.js).
# In the saved image the button and any .no-shot controls are hidden and a source line is added at the bottom.
SHOT_SOURCE <- "The Kings of Israel | Saul to Zedekiah, about 1051-586 BC"
shot <- function(name, ..., title = NULL, sub = NULL, cls = "")
  div(class = paste("shot", cls), `data-name` = name,
      tags$button(type = "button", class = "dl-btn", onclick = "kingsShot(this)", title = "Download this as a PNG image to share", HTML("&#8681; PNG")),
      if (!is.null(title)) div(class = "shot-title", title),
      if (!is.null(sub)) div(class = "lede", sub),
      ...,
      div(class = "shot-foot", SHOT_SOURCE))

vchip <- function(v, col, txt = "#FFFFFF") tags$span(class = "chip", style = paste0("--c:", col, ";--t:", txt), v)
vchips <- function() list(vchip("Good", PAL$verdict[["Good"]], "#FFFFFF"), vchip("Mixed", PAL$verdict[["Mixed"]], PAL$ink),
                          vchip("Evil", PAL$verdict[["Evil"]], "#FFFFFF"), vchip("Not rated", PAL$verdict[["N/A"]], "#FFFFFF"))

# the 43 rulers as a row of tiles in the header (colour = verdict, click = open the king)
hero_strip <- function() {
  row <- function(kg, lab, cls = "") {
    k <- KINGS[KINGS$kingdom == kg, ]
    div(class = paste("srow", cls), div(class = "sgrp-lab", lab, span(paste0(" (", nrow(k), ")"))),
        div(class = "tiles", lapply(seq_len(nrow(k)), function(i)
          tags$div(class = "tile", style = paste0("background:", PAL$verdict[[as.character(k$verdict[i])]]),
                   title = paste0(k$name[i], " - ", k$reign_txt[i], " - ", k$verdict[i]), onclick = js_pick(k$id[i])))))
  }
  div(class = "strip", row("United", "United Kingdom"), div(class = "split-mark", "931 BC - the kingdom splits"),
      row("Northern", "Israel (North)", "indent"), row("Southern", "Judah (South)", "indent"))
}

ui <- fluidPage(
  title = "The Kings of Israel",
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", crossorigin = "anonymous", href = "https://fonts.googleapis.com/css2?family=Archivo:wght@400;500;600;700;800;900&display=swap"),
    tags$link(rel = "stylesheet", href = paste0("app.css?v=", as.integer(file.mtime("www/app.css")))),   # version tag: browsers never use a stale copy
    tags$script(src = "https://cdn.jsdelivr.net/npm/html-to-image@1.11.13/dist/html-to-image.js"),
    tags$script(src = paste0("shot.js?v=", as.integer(file.mtime("www/shot.js"))))
  ),
  shot("kings-of-israel-header", cls = "shot-hero",
    div(class = "hero",
        div(class = "hero-inner",
            div(class = "hero-l",
                div(class = "hero-eyebrow", "Saul to Zedekiah | 43 rulers | about 1051 - 586 BC"),
                h1("The Kings of Israel"),
                div(class = "hero-sub", "One kingdom, one split, two roads to exile. Every ruler, how long he reigned, who he was related to, what the LORD said of him, and which prophets spoke in his day.")),
            shot("all-43-rulers-tiles", cls = "hero-r shot-strip",
                 div(class = "strip-title", "All 43 rulers - click a tile"), hero_strip(),
                 div(class = "hero-key", vchips()))))),
  div(class = "bar",
      div(class = "bar-g", span(class = "bar-lab", "Verdict"),
          checkboxGroupInput("verdicts", NULL, inline = TRUE, selected = VERDICT_LEVELS, choiceValues = VERDICT_LEVELS,
                             choiceNames = vchips())),
      div(class = "bar-g", span(class = "bar-lab", "Dynasty"),
          selectizeInput("dyn", NULL, width = "230px", choices = HOUSE_CHOICES, options = list(placeholder = "All dynasties"))),
      div(class = "bar-g bar-r", selectizeInput("jump", NULL, width = "260px",
                         choices = c("Jump to a king..." = "", stats::setNames(KINGS$id, paste0(KINGS$id, ". ", KINGS$name))),
                         options = list(placeholder = "Jump to a king...")))),
  div(class = "stats",
      shot("stat-united-kingdom", cls = "shot-card", stat(N_K[["United"]], "United Kingdom", "Saul, Ish-bosheth, David, Solomon", PAL$kingdom[["United"]])),
      shot("stat-kings-of-israel", cls = "shot-card", stat(N_K[["Northern"]], "Kings of Israel (North)", "ended in 722 BC", PAL$kingdom[["Northern"]])),
      shot("stat-kings-of-judah", cls = "shot-card", stat(N_K[["Southern"]], "Kings of Judah (South)", "ended in 586 BC", PAL$kingdom[["Southern"]])),
      shot("stat-succeeded-by-family", cls = "shot-card", uiOutput("stat_related")),
      shot("stat-verdicts", cls = "shot-card", uiOutput("stat_verdict")),
      shot("stat-named-prophets", cls = "shot-card", stat(length(unique(PROPH$prophet)), "Named prophets", "who served beside the kings", PAL$ink_soft))),
  div(class = "main",
      div(class = "main-l",
          tabsetPanel(id = "tab", type = "tabs",
            tabPanel("The Split",
                     shot("the-split-timeline", title = "The Split: every reign on one timeline",
                          sub = "Every row is a king and every bar is his reign, on one shared timeline. The kingdom is one until 931 BC, then it splits in two. Read across to see who reigned at the same time; click a king to light up his contemporaries in the other kingdom.",
                          div(class = "optrow no-shot", span(class = "opt-lab", "Colour the bars by"),
                              radioButtons("color_by", NULL, inline = TRUE, c("Verdict" = "verdict", "How he came to the throne" = "relationship", "Dynasty" = "dynasty"))),
                          girafeOutput("gantt", width = "100%", height = "auto"))),
            tabPanel("Lives & Reigns",
                     shot("lives-age-crowned-to-death", title = "From the crown to the grave",
                          sub = "Age when crowned to age at death, where the text records them. A ring is the day he was crowned, a solid disc is his death; a dotted run-out means his age at death is not recorded.",
                          div(class = "optrow no-shot", checkboxInput("partial", "Include kings with only one age recorded", TRUE, width = "auto")),
                          girafeOutput("dumbbell", width = "100%", height = "auto")),
                     shot("lives-years-on-the-throne", title = "Years on the throne",
                          sub = "How long every one of the 43 rulers reigned, including the many kings whose age when crowned or at death the Bible does not record. Colour = verdict.",
                          div(class = "optrow no-shot", radioButtons("order_by", NULL, inline = TRUE, c("In order" = "time", "Longest first" = "length"))),
                          girafeOutput("reigns", width = "100%", height = "auto")),
                     shot("dynasties-kings-and-years", title = "The dynasties: how many kings, and how long they lasted",
                          sub = "One bar per dynasty, in the order they came to power, split by kingdom. Switch between the number of kings and the years each dynasty held the throne (from the reference chronology, with co-regencies counted once). Click a bar to focus the whole app on that dynasty; click it again to show all.",
                          div(class = "optrow no-shot", span(class = "opt-lab", "Show"),
                              radioButtons("dmeasure", NULL, inline = TRUE, c("Number of kings" = "kings", "Years on the throne" = "years"))),
                          girafeOutput("dynbars", width = "100%", height = "auto"))),
            tabPanel("Houses & Kin",
                     div(class = "lede", "How did each king get the throne? Below, how many kings in each kingdom were the son, brother or other relative of the one before, and how many were not related at all."),
                     uiOutput("kin_cards")),
            tabPanel("Ties & Alliances",
                     shot("ties-sankey", title = "Who was bound to whom",
                          sub = "Every ribbon is one tie between two kings, coloured by kind: kin or marriage, alliance, war or rivalry, killed or overthrown. A thicker block means a king had more ties. Hover a ribbon to read the tie and where it is in the Bible; click a king to open his profile.",
                          div(class = "optrow no-shot", span(class = "opt-lab", "Show ties"),
                              radioButtons("scope", NULL, inline = TRUE, c("Between Israel and Judah" = "between", "Inside one kingdom" = "within"))),
                          uiOutput("ties_note"),
                          girafeOutput("ties", width = "100%", height = "auto")),
                     shot("ties-by-kind", title = "How many kings were bound by each kind of tie",
                          sub = "Counting both views together: each bar is the number of kings with at least one tie of that kind. A king with two wars counts once. Hover a bar for the names.",
                          girafeOutput("tie_counts", width = "100%", height = "auto")),
                     div(class = "foot", "Some ties come from your Notes; others I added from the biblical text. See data/cross_links.csv (column 'origin').")),
            tabPanel("Verdicts",
                     shot("verdicts-by-reign-length", title = "What the LORD said of each king",
                          sub = "Each bubble is one king, in the column of his verdict. The bigger the bubble, the longer he reigned. Kings are grouped by dynasty; use the Dynasty box at the top to focus on one.",
                          girafeOutput("vdots", width = "100%", height = "auto"))),
            tabPanel("Prophets during the reigns of kings",
                     shot("prophets-during-the-reigns-of-kings", title = "Prophets during the reigns of kings",
                          sub = "Which prophets ministered while each king reigned? The bar shows how many, the names are beside it, and the colour is the verdict on the king. Use the Dynasty box at the top to look at one dynasty.",
                          div(class = "optrow no-shot", span(class = "opt-lab", "View"),
                              radioButtons("pview", NULL, inline = TRUE, c("One bar per king" = "king", "One bar per prophet" = "prophet"))),
                          girafeOutput("prophets", width = "100%", height = "auto")))
          )),
      div(class = "main-r", uiOutput("dossier")))
)

server <- function(input, output, session) {
  selected <- reactiveVal(NULL)
  observeEvent(input$king_pick, { if (identical(selected(), input$king_pick)) selected(NULL) else selected(input$king_pick) })
  observeEvent(input$jump, { if (nzchar(input$jump)) selected(as.integer(input$jump)) })
  observeEvent(input$clear_sel, { selected(NULL) })
  observeEvent(input$nav, { i <- selected(); if (!is.null(i)) selected(((i - 1 + input$nav$d) %% nrow(KINGS)) + 1) })
  observeEvent(input$house_pick, {                      # click a dynasty bar = focus the app on it (click again to clear)
    h <- input$house_pick
    updateSelectizeInput(session, "dyn", selected = if (identical(input$dyn, h)) "" else h)
  })

  verd <- reactive({ v <- input$verdicts; if (is.null(v)) character() else v })
  dyn  <- reactive({ if (is.null(input$dyn) || !nzchar(input$dyn)) NULL else input$dyn })
  rel  <- reactive({ s <- selected(); if (is.null(s)) integer() else { o <- OVER$other[OVER$id == s]; unique(o[!is.na(o)]) } })
  ctx  <- reactive(make_ctx(verd(), dyn(), selected(), rel()))
  kept <- reactive(KINGS[king_keep(KINGS, ctx()), ])

  output$gantt <- renderGirafe({ r <- plot_gantt(KINGS, TIPS, ctx(), input$color_by %||% "verdict"); make_girafe(r$plot, 10, 0.3 * r$n + 3.2) })
  output$dumbbell <- renderGirafe({
    r <- plot_dumbbell(KINGS, TIPS, ctx(), show_partial = isTRUE(input$partial))
    make_girafe(r$plot, 10, 0.42 * r$n + 1.1)
  })
  output$reigns <- renderGirafe(make_girafe(plot_reigns(KINGS, TIPS, ctx(), input$order_by %||% "time"), 10, 11.5))
  output$ties <- renderGirafe({
    r <- plot_ties_sankey(KINGS, LINKS, TIPS, ctx(), input$scope %||% "between")
    make_girafe(r$plot, 10, 0.3 * r$units + 1.6)
  })
  output$ties_note <- renderUI({
    div(class = "lede", strong(if ((input$scope %||% "between") == "between")
      "Israel's kings are on the left, Judah's on the right. Solomon of the United Kingdom sits with Judah, his own line."
    else "Each tie runs from the earlier king (left) to the later one (right), so the chains of coups read across: Elah to Zimri, Zimri to Omri."))
  })
  output$tie_counts <- renderGirafe(make_girafe(plot_tie_counts(KINGS, LINKS, ctx()), 10, 3.6))
  output$dynbars <- renderGirafe(make_girafe(plot_dynasty_bars(KINGS, ctx(), input$dmeasure %||% "kings"), 10, 6.4))
  output$vdots <- renderGirafe({
    k <- kept(); nh <- length(unique(k$house))
    make_girafe(plot_verdict_dots(KINGS, TIPS, ctx()), 10, max(2.5, 0.3 * nrow(k) + 0.28 * nh + 0.9))
  })
  output$prophets <- renderGirafe({
    k <- kept()
    if ((input$pview %||% "king") == "king") {
      nh <- length(unique(k$kingdom))
      make_girafe(plot_prophet_bars(KINGS, TIPS, ctx()), 10, max(2.5, 0.3 * nrow(k) + 0.45 * nh + 1.3))
    } else {
      np <- length(unique(PROPH$prophet[PROPH$king_id %in% k$id]))
      make_girafe(plot_prophet_ranks(KINGS, PROPH, ctx()), 10, max(2.5, 0.3 * np + 1.5))
    }
  })

  # ---- header stat cards ------------------------------------------------------------------------------------------
  output$stat_related <- renderUI({
    d <- KINGS[KINGS$kingdom != "United" & KINGS$rel_class != "First king", ]
    stat(sum(d$related), "Succeeded by family", paste0("of ", nrow(d), " later kings (", nrow(d) - sum(d$related), " were not related)"))
  })
  output$stat_verdict <- renderUI({
    t <- table(factor(KINGS$verdict, VERDICT_LEVELS))
    div(class = "stat", div(class = "stat-num stat-verdict",
        span(style = paste0("color:", PAL$verdict[["Good"]]), t[["Good"]]), "/", span(style = paste0("color:", PAL$verdict[["Mixed"]]), t[["Mixed"]]), "/",
        span(style = paste0("color:", PAL$verdict[["Evil"]]), t[["Evil"]])), div(class = "stat-lab", "Good / Mixed / Evil"),
        div(class = "stat-sub", paste0(t[["N/A"]], " not rated")))
  })
  output$kin_cards <- renderUI({
    d <- KINGS[KINGS$kingdom != "United" & KINGS$rel_class != "First king", ]
    card <- function(kg, lab) {
      t <- sort(table(d$rel_prev[d$kingdom == kg]), decreasing = TRUE); n <- sum(t)
      fam <- sum(t[names(t) %in% c("Son", "Brother", "Grandson", "Mother", "Uncle")])
      shot(paste0("kin-", tolower(gsub("[^A-Za-z]+", "-", lab))), cls = "shot-card", div(class = "kincard", style = paste0("border-top-color:", PAL$kingdom[[kg]]), div(class = "kc-t", lab),
          div(class = "kc-n", fam, span(class = "kc-of", paste0(" of ", n, " were family of the king before"))),
          div(class = "kc-b", lapply(names(t), function(nm) div(class = "kc-row", span(class = "kc-l", nm),
                                    div(class = "kc-bar", style = paste0("width:", 100 * t[[nm]] / max(t), "%;background:", PAL$kingdom[[kg]])), span(class = "kc-v", t[[nm]]))))))
    }
    div(class = "kincards", card("Northern", "Israel (North)"), card("Southern", "Judah (South)"))
  })

  # ---- the dossier ------------------------------------------------------------------------------------------------
  output$dossier <- renderUI({
    s <- selected()
    if (is.null(s)) return(div(class = "dossier empty",
      h3("Choose a king"),
      p("Click any king on any chart, any tile in the header, or use the box at the top. His profile opens here: reign, relatives, who reigned beside him, his prophets and the passages to read.")))
    r <- KINGS[KINGS$id == s, ]
    ov <- OVER[OVER$id == s, ]; ov <- ov[order(-ov$overlap), ]
    sy <- SYNC[SYNC$id == s & SYNC$kind == "primary", ]
    row <- function(l, v) if (!is.null(v) && !is.na(v) && nzchar(v)) tags$tr(tags$td(l), tags$td(v))
    shot(paste0("king-", tolower(gsub("[^A-Za-z]+", "-", r$label))), cls = "shot-card shot-dossier", div(class = "dossier", style = paste0("border-top-color:", PAL$kingdom[[as.character(r$kingdom)]]),
      div(class = "d-nav no-shot", actionLink("prev", "< Previous", onclick = "Shiny.setInputValue('nav', {d:-1}, {priority:'event'}); return false;"),
          actionLink("clr", "Close", onclick = "Shiny.setInputValue('clear_sel', Math.random(), {priority:'event'}); return false;"),
          actionLink("next", "Next >", onclick = "Shiny.setInputValue('nav', {d:1}, {priority:'event'}); return false;")),
      div(class = "d-k", paste(KINGDOM_LABELS[[as.character(r$kingdom)]], "|", r$house)),
      h2(r$name), div(class = "d-badges", HTML(pill(r$verdict)), span(class = "d-reign", paste(r$reign_txt, "|", r$span_txt))),
      tags$table(class = "d-tab",
        row("Crowned at", if (!is.na(r$age_start)) paste("age", r$age_start)),
        row("Died at", if (!is.na(r$age_end)) paste("age", r$age_end)),
        row("Related to previous king", ifelse(r$rel_prev == "None", "First king", r$rel_prev)),
        if (nrow(sy)) row("Synchronism", sy$text[1]),
        if (nrow(sy)) row("Reference chronology", if (sy$agrees[1]) "agrees" else paste0("differs by about ", abs(round(sy$gap_years[1])), " years (a known puzzle)"))),
      if (nrow(ov)) div(class = "d-sec", div(class = "d-h", "Reigned alongside"),
          lapply(seq_len(nrow(ov)), function(i) { o <- KINGS[KINGS$id == ov$other[i], ]
            tags$a(href = "#", class = "kinglink", onclick = paste0(js_pick(o$id), " return false;"), paste0(o$label, " (", ov$overlap[i], " yrs)")) })),
      div(class = "d-sec", div(class = "d-h", paste0("Prophets (", r$n_prophets, ")")),
          if (r$n_prophets) lapply(unlist(r$prophets), function(p) span(class = "pchip", p)) else span(class = "muted", "none recorded"),
          if (r$unnamed) span(class = "muted", " + unnamed prophet(s)")),
      div(class = "d-sec", div(class = "d-h", "Read"), bg_links(r$refs)),
      div(class = "d-sec", div(class = "d-h", "Notes"), p(class = "d-notes", r$notes))))
  })
}

shinyApp(ui, server)
