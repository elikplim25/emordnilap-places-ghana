# The Kings of Israel (Shiny)
Open `app.R` in RStudio and press Run App. Needs: shiny, ggplot2, ggiraph, dplyr, readxl, htmltools (R 4.1+).
Colours are the LTC palettes "heatmap0" (verdicts, ink, page) and "hat" (kingdoms, ties); hex values live in R/data.R so the ltc package is optional.
- data/Kings data__CLAUDE.xlsx : your sheet (source of truth)
- data/chronology_reference.csv : approximate BC dates (mine - review/edit), used only to place kings on The Split timeline
- data/secondary_synchronisms.csv : the 3 second synchronisms from your Notes
- data/cross_links.csv : alliances/wars/marriages/coups; column `origin` = sheet / derived / added (added = mine, review)
