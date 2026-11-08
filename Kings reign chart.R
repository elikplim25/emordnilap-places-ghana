library(tidyverse)

#### loading the data set ##########
Monarchs <- read_csv("IsraelMonarchs_v2.csv")

#### preparing reign lengths ##########
kingdom_cols <- c("United Kingdom" = "#2a78d6",
                  "Israel (North)" = "#eb6834",
                  "Judah (South)"  = "#1baf7a")

reigns <- Monarchs %>%
  mutate(Kingdom = recode(Kingdom,
                          "United Kingdom of Israel"   = "United Kingdom",
                          "Northern Kingdom of Israel" = "Israel (North)",
                          "Southern Kingdom (Judah)"   = "Judah (South)"),
         Kingdom = factor(Kingdom, levels = names(kingdom_cols)),
         King = fct_reorder(`Name of King`, `#`, .desc = TRUE),
         # label reigns under a year (their bars are too short to see) and the 3 longest
         label = case_when(`Years Reigned` < 0.05 ~ "7 days",
                           `Years Reigned` < 0.1  ~ "1 month",
                           `Years Reigned` < 1    ~ paste(round(`Years Reigned` * 12), "months"),
                           min_rank(desc(`Years Reigned`)) <= 3 ~ paste(`Years Reigned`, "years"),
                           TRUE ~ NA_character_))

#### chart ##########
reign_chart <- ggplot(reigns, aes(x = `Years Reigned`, y = King, fill = Kingdom)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label), hjust = -0.15, colour = "#52514e", size = 3, na.rm = TRUE) +
  facet_grid(Kingdom ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = kingdom_cols, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "How long each king of Israel and Judah reigned",
       subtitle = "Years on the throne, in order of reign within each kingdom",
       x = "Years reigned", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.background    = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#e6e5e1", linewidth = 0.3),
        panel.spacing.y    = unit(10, "pt"),
        strip.text.y       = element_text(angle = 0, hjust = 0, face = "bold", colour = "#0b0b0b"),
        plot.title         = element_text(face = "bold", colour = "#0b0b0b"),
        plot.subtitle      = element_text(colour = "#52514e"),
        axis.text          = element_text(colour = "#52514e"),
        axis.title.x       = element_text(colour = "#52514e"))

reign_chart

ggsave("Kings_by_years_reigned.png", reign_chart, width = 8, height = 10, dpi = 200)
