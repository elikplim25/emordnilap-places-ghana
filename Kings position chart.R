library(tidyverse)

#### loading the data set ##########
Monarchs <- read_csv("IsraelMonarchs_v2.csv")

#### getting each king's position in his kingdom ##########
kingdom_cols <- c("United Kingdom" = "#2a78d6",
                  "Israel (North)" = "#eb6834",
                  "Judah (South)"  = "#1baf7a")

positions <- Monarchs %>%
  mutate(Kingdom = recode(Kingdom,
                          "United Kingdom of Israel"   = "United Kingdom",
                          "Northern Kingdom of Israel" = "Israel (North)",
                          "Southern Kingdom (Judah)"   = "Judah (South)"),
         Kingdom = factor(Kingdom, levels = names(kingdom_cols)),
         Position = as.numeric(str_extract(`Position in Kingdom`, "^[0-9]+")))

#### chart ##########
position_chart <- ggplot(positions, aes(x = Kingdom, y = Position, fill = Kingdom)) +
  geom_tile(width = 0.95, height = 0.9, colour = "#fcfcfb", linewidth = 0.7) +
  geom_text(aes(label = `Name of King`), colour = "#0b0b0b", size = 3.2) +
  scale_fill_manual(values = kingdom_cols, guide = "none") +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks = 1:20, expand = expansion(add = 0.3)) +
  labs(title = "The order of succession in each kingdom",
       subtitle = "Each king's position in the line of kings of his kingdom",
       x = NULL, y = "Position") +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid      = element_blank(),
        plot.title      = element_text(face = "bold", colour = "#0b0b0b"),
        plot.subtitle   = element_text(colour = "#52514e"),
        axis.text.x.top = element_text(face = "bold", colour = "#0b0b0b", size = 11),
        axis.text.y     = element_text(colour = "#52514e"),
        axis.title.y    = element_text(colour = "#52514e"))

position_chart

ggsave("Kings_by_position.png", position_chart, width = 8, height = 9, dpi = 200)
