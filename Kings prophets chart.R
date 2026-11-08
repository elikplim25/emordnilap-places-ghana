library(tidyverse)

#### loading the data set ##########
Monarchs <- read_csv("IsraelMonarchs_v2.csv")

#### one row per king-prophet pair ##########
kingdom_cols <- c("United Kingdom" = "#2a78d6",
                  "Israel (North)" = "#eb6834",
                  "Judah (South)"  = "#1baf7a")

king_prophets <- Monarchs %>%
  mutate(Kingdom = recode(Kingdom,
                          "United Kingdom of Israel"   = "United Kingdom",
                          "Northern Kingdom of Israel" = "Israel (North)",
                          "Southern Kingdom (Judah)"   = "Judah (South)"),
         Kingdom = factor(Kingdom, levels = names(kingdom_cols))) %>%
  separate_longer_delim(`Prophet(s)`, delim = ", ") %>%
  rename(Prophet = `Prophet(s)`) %>%
  mutate(Prophet = str_trim(Prophet),
         Prophet = recode(Prophet,
                          "Unnamed prophet"  = "Unnamed prophet(s)",
                          "Unnamed prophets" = "Unnamed prophet(s)")) %>%
  filter(Prophet != "None recorded")

#### counting kings per prophet ##########
prophet_counts <- king_prophets %>%
  count(Prophet, Kingdom) %>%
  group_by(Prophet) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(Prophet = fct_reorder(Prophet, total))

prophet_totals <- prophet_counts %>% distinct(Prophet, total)

no_prophet <- sum(Monarchs$`Prophet(s)` == "None recorded")

#### chart ##########
prophet_chart <- ggplot(prophet_counts, aes(x = n, y = Prophet, fill = Kingdom)) +
  geom_col(width = 0.7, colour = "#fcfcfb", linewidth = 0.5,
           position = position_stack(reverse = TRUE)) +
  geom_text(data = prophet_totals, aes(x = total, y = Prophet, label = total),
            inherit.aes = FALSE, hjust = -0.4, colour = "#0b0b0b", size = 3.2) +
  scale_fill_manual(values = kingdom_cols) +
  scale_x_continuous(breaks = seq(0, 10, 2), expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Hosea prophesied under more kings than any other prophet",
       subtitle = "Number of kings each prophet is recorded as serving under, by kingdom",
       caption = paste(no_prophet, "kings have no prophet recorded and are not shown."),
       x = "Number of kings", y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.background    = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(colour = "#e6e5e1", linewidth = 0.3),
        plot.title         = element_text(face = "bold", colour = "#0b0b0b"),
        plot.subtitle      = element_text(colour = "#52514e"),
        plot.caption       = element_text(colour = "#52514e", hjust = 0),
        axis.text          = element_text(colour = "#52514e"),
        axis.title.x       = element_text(colour = "#52514e"),
        legend.position    = "top",
        legend.justification = "left",
        legend.text        = element_text(colour = "#52514e"))

prophet_chart

ggsave("Kings_by_prophets.png", prophet_chart, width = 8, height = 9, dpi = 200)
