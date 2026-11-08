library(tidyverse)

#### loading the data set ##########
Monarchs <- read_csv("IsraelMonarchs_v2.csv")

#### counting kings per kingdom ##########
kingdom_cols <- c("United Kingdom" = "#2a78d6",
                  "Israel (North)" = "#eb6834",
                  "Judah (South)"  = "#1baf7a")

kingdom_counts <- Monarchs %>%
  mutate(Kingdom = recode(Kingdom,
                          "United Kingdom of Israel"   = "United Kingdom",
                          "Northern Kingdom of Israel" = "Israel (North)",
                          "Southern Kingdom (Judah)"   = "Judah (South)"),
         Kingdom = factor(Kingdom, levels = rev(names(kingdom_cols)))) %>%
  count(Kingdom)

#### chart ##########
kingdom_chart <- ggplot(kingdom_counts, aes(x = n, y = Kingdom, fill = Kingdom)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), hjust = -0.4, colour = "#0b0b0b",
            size = 4.2, fontface = "bold") +
  scale_fill_manual(values = kingdom_cols, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Judah and Israel each had about 20 kings after the split",
       subtitle = "Number of kings in each kingdom",
       x = "Number of kings", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.background    = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#e6e5e1", linewidth = 0.3),
        plot.title         = element_text(face = "bold", colour = "#0b0b0b"),
        plot.subtitle      = element_text(colour = "#52514e"),
        axis.text          = element_text(colour = "#52514e"),
        axis.title.x       = element_text(colour = "#52514e"))

kingdom_chart

ggsave("Kings_by_kingdom.png", kingdom_chart, width = 8, height = 3.5, dpi = 200)
