library(tidyverse)

#### loading the data set ##########
Monarchs <- read_csv("IsraelMonarchs_v2.csv")

#### cleaning the relationship column ##########
kingdom_cols <- c("United Kingdom" = "#2a78d6",
                  "Israel (North)" = "#eb6834",
                  "Judah (South)"  = "#1baf7a")

relationships <- Monarchs %>%
  mutate(Kingdom = recode(Kingdom,
                          "United Kingdom of Israel"   = "United Kingdom",
                          "Northern Kingdom of Israel" = "Israel (North)",
                          "Southern Kingdom (Judah)"   = "Judah (South)"),
         Kingdom = factor(Kingdom, levels = names(kingdom_cols)),
         Relationship = recode(`Relationship to Previous King`,
                               "None" = "First king",
                               "Servant (Not related by blood)" = "Servant (not related)",
                               "Mother" = "Mother of previous king"))

#### counting kings per relationship ##########
relationship_counts <- relationships %>%
  count(Relationship, Kingdom) %>%
  group_by(Relationship) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(Relationship = fct_reorder(Relationship, total))

relationship_totals <- relationship_counts %>% distinct(Relationship, total)

#### chart ##########
relationship_chart <- ggplot(relationship_counts, aes(x = n, y = Relationship, fill = Kingdom)) +
  geom_col(width = 0.6, colour = "#fcfcfb", linewidth = 0.7,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5, reverse = TRUE),
            colour = "#0b0b0b", size = 3.3) +
  geom_text(data = relationship_totals, aes(x = total, y = Relationship, label = total),
            inherit.aes = FALSE, hjust = -0.4, colour = "#0b0b0b",
            size = 3.8, fontface = "bold") +
  scale_fill_manual(values = kingdom_cols) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(title = "Most kings took the throne from their father",
       subtitle = "How each king was related to the king before him, by kingdom",
       caption = "Not related: the new king was not family of the king before him, e.g. David after Ish-bosheth.",
       x = "Number of kings", y = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.background    = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#e6e5e1", linewidth = 0.3),
        plot.title         = element_text(face = "bold", colour = "#0b0b0b"),
        plot.subtitle      = element_text(colour = "#52514e"),
        plot.caption       = element_text(colour = "#52514e", hjust = 0, size = 9),
        axis.text          = element_text(colour = "#52514e"),
        axis.title.x       = element_text(colour = "#52514e"),
        legend.position    = "top",
        legend.justification = "left",
        legend.text        = element_text(colour = "#52514e"))

relationship_chart

ggsave("Kings_by_relationship.png", relationship_chart, width = 8, height = 5, dpi = 200)
