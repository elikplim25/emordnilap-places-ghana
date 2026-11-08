library(tidyverse)

#### loading the data set ##########
Monarchs <- read_csv("IsraelMonarchs_v2.csv")

#### finding which books each king appears in ##########
kingdom_cols <- c("United Kingdom" = "#2a78d6",
                  "Israel (North)" = "#eb6834",
                  "Judah (South)"  = "#1baf7a")

# book name -> pattern in the Bible References column ("1 & 2 Samuel" counts for both)
books <- c("1 Samuel"     = "1 (& 2 )?Samuel",
           "2 Samuel"     = "2 Samuel",
           "1 Kings"      = "1 Kings",
           "2 Kings"      = "2 Kings",
           "1 Chronicles" = "1 Chronicles",
           "2 Chronicles" = "2 Chronicles",
           "Isaiah"       = "Isaiah",
           "Jeremiah"     = "Jeremiah",
           "Ezekiel"      = "Ezekiel",
           "Hosea"        = "Hosea",
           "Amos"         = "Amos",
           "Acts"         = "Acts")

king_books <- Monarchs %>%
  mutate(Kingdom = recode(Kingdom,
                          "United Kingdom of Israel"   = "United Kingdom",
                          "Northern Kingdom of Israel" = "Israel (North)",
                          "Southern Kingdom (Judah)"   = "Judah (South)"),
         Kingdom = factor(Kingdom, levels = names(kingdom_cols)),
         King = fct_reorder(`Name of King`, `#`, .desc = TRUE)) %>%
  select(King, Kingdom, `Bible References`) %>%
  crossing(Book = names(books)) %>%
  filter(str_detect(`Bible References`, books[Book])) %>%
  mutate(Book = factor(Book, levels = names(books)))

#### chart ##########
references_chart <- ggplot(king_books, aes(x = Book, y = King, colour = Kingdom)) +
  geom_point(size = 3.2) +
  facet_grid(Kingdom ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = kingdom_cols, guide = "none") +
  scale_x_discrete(position = "top", drop = FALSE,
                   labels = function(x) str_replace(x, " ", "\n")) +
  labs(title = "Chronicles skips almost every king of the northern kingdom",
       subtitle = "Books of the Bible where each king's story is told, in order of reign",
       caption = "Based on the Bible References column; brief mentions elsewhere are not included.",
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

references_chart

ggsave("Kings_by_bible_references.png", references_chart, width = 9, height = 10, dpi = 200)
