library(tidyverse)

#### loading the data set ##########
Monarchs <- read_csv("IsraelMonarchs_v2.csv")

#### keeping kings dated by a king of the other kingdom ##########
kingdom_cols <- c("United Kingdom" = "#2a78d6",
                  "Israel (North)" = "#eb6834",
                  "Judah (South)"  = "#1baf7a")

synced <- Monarchs %>%
  mutate(Kingdom = recode(Kingdom,
                          "United Kingdom of Israel"   = "United Kingdom",
                          "Northern Kingdom of Israel" = "Israel (North)",
                          "Southern Kingdom (Judah)"   = "Judah (South)"),
         Kingdom = factor(Kingdom, levels = names(kingdom_cols)),
         Synced = `Synchronized With`) %>%
  filter(!is.na(Synced))

no_sync <- sum(is.na(Monarchs$`Synchronized With`))

#### counting kings per synchronizing king ##########
sync_counts <- synced %>%
  count(Synced, Kingdom) %>%
  group_by(Synced) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(Synced = fct_reorder(Synced, total))

sync_totals <- sync_counts %>% distinct(Synced, total)

#### chart ##########
sync_chart <- ggplot(sync_counts, aes(x = n, y = Synced, fill = Kingdom)) +
  geom_col(width = 0.6, colour = "#fcfcfb", linewidth = 0.7,
           position = position_stack(reverse = TRUE)) +
  geom_text(data = sync_totals, aes(x = total, y = Synced, label = total),
            inherit.aes = FALSE, hjust = -0.4, colour = "#0b0b0b",
            size = 3.8, fontface = "bold") +
  scale_fill_manual(values = kingdom_cols) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(title = "Asa of Judah is the reference point for 7 northern kings",
       subtitle = "Number of kings whose start is dated by the reign of each king in the other kingdom",
       caption = paste(no_sync, "kings have no synchronism recorded and are not shown."),
       x = "Number of kings", y = "Synchronized with", fill = "Kingdom of the king being dated:") +
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
        legend.text        = element_text(colour = "#52514e"),
        legend.title       = element_text(colour = "#52514e"))

sync_chart

ggsave("Kings_by_synchronism.png", sync_chart, width = 8, height = 5, dpi = 200)
