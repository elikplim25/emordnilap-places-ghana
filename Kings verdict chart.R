library(tidyverse)

#### loading the data set ##########
Monarchs <- read_csv("IsraelMonarchs_v2.csv")

#### counting kings by verdict and kingdom ##########
kingdom_cols <- c("United Kingdom" = "#2a78d6",
                  "Israel (North)" = "#eb6834",
                  "Judah (South)"  = "#1baf7a")

verdict_counts <- Monarchs %>%
  mutate(Kingdom = recode(Kingdom,
                          "United Kingdom of Israel"   = "United Kingdom",
                          "Northern Kingdom of Israel" = "Israel (North)",
                          "Southern Kingdom (Judah)"   = "Judah (South)"),
         Kingdom = factor(Kingdom, levels = names(kingdom_cols)),
         Verdict = factor(Verdict, levels = rev(c("Good", "Mixed", "Evil", "N/A")))) %>%
  count(Verdict, Kingdom)

verdict_totals <- verdict_counts %>%
  group_by(Verdict) %>%
  summarise(total = sum(n))

#### chart ##########
kings_chart <- ggplot(verdict_counts, aes(x = n, y = Verdict, fill = Kingdom)) +
  geom_col(width = 0.6, colour = "#fcfcfb", linewidth = 0.7,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5, reverse = TRUE),
            colour = "#0b0b0b", size = 3.5) +
  geom_text(data = verdict_totals, aes(x = total, y = Verdict, label = total),
            inherit.aes = FALSE, hjust = -0.4, colour = "#0b0b0b",
            size = 4, fontface = "bold") +
  scale_fill_manual(values = kingdom_cols) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Most kings of Israel and Judah did evil",
       subtitle = "Number of kings by God's verdict, split by kingdom",
       x = "Number of kings", y = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.background    = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#e6e5e1", linewidth = 0.3),
        plot.title         = element_text(face = "bold", colour = "#0b0b0b"),
        plot.subtitle      = element_text(colour = "#52514e"),
        axis.text          = element_text(colour = "#52514e"),
        axis.title.x       = element_text(colour = "#52514e"),
        legend.position    = "top",
        legend.justification = "left",
        legend.text        = element_text(colour = "#52514e"))

kings_chart

ggsave("Kings_by_verdict.png", kings_chart, width = 8, height = 4.5, dpi = 200)
