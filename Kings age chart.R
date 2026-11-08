library(tidyverse)

#### loading the data set ##########
Monarchs <- read_csv("IsraelMonarchs_v2.csv")

#### keeping kings with a recorded age ##########
kingdom_cols <- c("United Kingdom" = "#2a78d6",
                  "Israel (North)" = "#eb6834",
                  "Judah (South)"  = "#1baf7a")

ages <- Monarchs %>%
  mutate(Kingdom = recode(Kingdom,
                          "United Kingdom of Israel"   = "United Kingdom",
                          "Northern Kingdom of Israel" = "Israel (North)",
                          "Southern Kingdom (Judah)"   = "Judah (South)"),
         Kingdom = factor(Kingdom, levels = names(kingdom_cols)),
         King = fct_reorder(`Name of King`, `#`, .desc = TRUE)) %>%
  filter(!is.na(`Age at Start`))

no_age <- nrow(Monarchs) - nrow(ages)

#### chart ##########
age_chart <- ggplot(ages, aes(x = `Age at Start`, y = King, fill = Kingdom)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = `Age at Start`), hjust = -0.3, colour = "#52514e", size = 3.2) +
  facet_grid(Kingdom ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = kingdom_cols, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Two kings of Judah took the throne as young boys",
       subtitle = "Age when each king began to reign, in order of reign",
       caption = paste0(no_age, " kings, including every king of the northern kingdom, ",
                        "have no recorded age and are not shown."),
       x = "Age at start of reign", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.background    = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#e6e5e1", linewidth = 0.3),
        panel.spacing.y    = unit(10, "pt"),
        strip.text.y       = element_text(angle = 0, hjust = 0, face = "bold", colour = "#0b0b0b"),
        plot.title         = element_text(face = "bold", colour = "#0b0b0b"),
        plot.subtitle      = element_text(colour = "#52514e"),
        plot.caption       = element_text(colour = "#52514e", hjust = 0, size = 9),
        axis.text          = element_text(colour = "#52514e"),
        axis.title.x       = element_text(colour = "#52514e"))

age_chart

ggsave("Kings_by_age_at_start.png", age_chart, width = 8, height = 6, dpi = 200)
