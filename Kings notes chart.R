library(tidyverse)

#### loading the data set ##########
Monarchs <- read_csv("IsraelMonarchs_v2.csv")

#### tagging themes in each king's notes ##########
kingdom_cols <- c("United Kingdom" = "#2a78d6",
                  "Israel (North)" = "#eb6834",
                  "Judah (South)"  = "#1baf7a")

# theme -> keywords searched for in the Notes column (edit these to change the tagging)
themes <- c("Idolatry"              = "Baal|idol|golden calves|sins?\\b|astral|gods|sorcery|pass through the fire",
           "Took throne by force"  = "(^|\\. )Assassinated [A-Z]|Killed Shallum|Murdered his brothers|Seized the throne|by coup",
           "Died by violence"      = "while drunk|assassinated by|Killed in a conspiracy|Killed at Megiddo|killed by Jehu|burned the palace over himself|Assassinated by his own servants",
           "Religious reform"      = "Removed|restored Temple|reform|Sent teachers|destroyed Baal|Sought God|Trusted in the LORD|asked God for wisdom",
           "Repented"              = "repent",
           "Clashed with prophet"  = "opposed by Elijah|Condemned by the prophet|rebuked|imprisoned the seer|Jeremiah.s scroll|stoned",
           "Foreign powers"        = "Assyria|Babylon|Nebuchadnezzar|Pharaoh|Egypt|Aram",
           "Text or date puzzle"   = "scribal|Masoretic|numeral gap|chronologically|second synchronism|co-regen")

king_themes <- Monarchs %>%
  mutate(Kingdom = recode(Kingdom,
                          "United Kingdom of Israel"   = "United Kingdom",
                          "Northern Kingdom of Israel" = "Israel (North)",
                          "Southern Kingdom (Judah)"   = "Judah (South)"),
         Kingdom = factor(Kingdom, levels = names(kingdom_cols)),
         King = fct_reorder(`Name of King`, `#`, .desc = TRUE)) %>%
  select(King, Kingdom, Notes) %>%
  crossing(Theme = names(themes)) %>%
  filter(str_detect(Notes, themes[Theme])) %>%
  mutate(Theme = factor(Theme, levels = names(themes)))

#### chart ##########
notes_chart <- ggplot(king_themes, aes(x = Theme, y = King, colour = Kingdom)) +
  geom_point(size = 3.2) +
  facet_grid(Kingdom ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = kingdom_cols, guide = "none") +
  scale_x_discrete(position = "top", drop = FALSE,
                   labels = function(x) str_replace(x, " ", "\n")) +
  labs(title = "Idolatry is the most common theme in the notes on the kings",
       subtitle = "Themes found in the notes on each king, in order of reign",
       caption = "Themes are tagged by keywords in the Notes column, so a theme the notes do not mention is not shown.",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.background    = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid.minor   = element_blank(),
        panel.grid.major   = element_line(colour = "#e6e5e1", linewidth = 0.3),
        panel.spacing.y    = unit(10, "pt"),
        strip.text.y       = element_text(angle = 0, hjust = 0, face = "bold", colour = "#0b0b0b"),
        plot.title         = element_text(face = "bold", colour = "#0b0b0b"),
        plot.subtitle      = element_text(colour = "#52514e"),
        plot.caption       = element_text(colour = "#52514e", hjust = 0, size = 9),
        axis.text          = element_text(colour = "#52514e"),
        axis.text.x.top    = element_text(vjust = 0, size = 9))

notes_chart

ggsave("Kings_by_notes.png", notes_chart, width = 9, height = 10, dpi = 200)
