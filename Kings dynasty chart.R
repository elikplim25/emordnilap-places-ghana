library(tidyverse)

#### loading the data set ##########
Monarchs <- read_csv("IsraelMonarchs_v2.csv")

#### cleaning the dynasty column ##########
kingdom_cols <- c("United Kingdom" = "#2a78d6",
                  "Israel (North)" = "#eb6834",
                  "Judah (South)"  = "#1baf7a")

dynasties <- Monarchs %>%
  mutate(Kingdom = recode(Kingdom,
                          "United Kingdom of Israel"   = "United Kingdom",
                          "Northern Kingdom of Israel" = "Israel (North)",
                          "Southern Kingdom (Judah)"   = "Judah (South)"),
         Kingdom = factor(Kingdom, levels = names(kingdom_cols)),
         # founders belong to the house they started, e.g. Omri -> House of Omri
         Dynasty = case_when(`Dynasty/House` == "Founder" ~
                               paste("House of", word(`Name of King`, 1)),
                             `Dynasty/House` == "Usurper" ~ "No dynasty (usurpers)",
                             TRUE ~ `Dynasty/House`))

#### counting kings per dynasty ##########
dynasty_counts <- dynasties %>%
  count(Dynasty, Kingdom) %>%
  group_by(Dynasty) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(Dynasty = fct_reorder(Dynasty, total))

dynasty_totals <- dynasty_counts %>% distinct(Dynasty, total)

#### chart ##########
dynasty_chart <- ggplot(dynasty_counts, aes(x = n, y = Dynasty, fill = Kingdom)) +
  geom_col(width = 0.6, colour = "#fcfcfb", linewidth = 0.7,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5, reverse = TRUE),
            colour = "#0b0b0b", size = 3.3) +
  geom_text(data = dynasty_totals, aes(x = total, y = Dynasty, label = total),
            inherit.aes = FALSE, hjust = -0.4, colour = "#0b0b0b",
            size = 3.8, fontface = "bold") +
  scale_fill_manual(values = kingdom_cols) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(title = "The House of David produced half of all the kings",
       subtitle = "Number of kings from each dynasty, by kingdom",
       caption = "Founders are counted in the house they started. Usurpers who founded no dynasty are grouped together.",
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

dynasty_chart

ggsave("Kings_by_dynasty.png", dynasty_chart, width = 8, height = 5, dpi = 200)
