library(tidyverse)
library(readxl)

#### loading the data sets ##########
Kings_data_claude <- read_excel("C:/Users/XPS/Downloads/Kings data__CLAUDE.xlsx",
                                sheet = "Kings Data")
kings_position <- read_excel("C:/Users/XPS/Downloads/kings_position.xlsx")

###Removing labels from the King columns ######
kings_position1 <- kings_position%>%
  mutate(King = str_remove(King, "^\\d+\\.\\s"))

#### merging the 2 data sets ##########
# Both files list the 43 kings in the same order, so join on row number.
# (Joining on King name drops 13 kings, e.g. "Ahaziah (Israel)" vs "Ahaziah".)
kings_position1 <- kings_position1 %>%
  mutate(ID = row_number()) %>%
  select(ID, `Position in Kingdom`)

Kings_merged2 <- merge(Kings_data_claude, kings_position1, by.x = "#", by.y = "ID")
dim(Kings_merged2)



Monarchs <-Kings_merged2 %>% distinct(`Name of King`, Kingdom, `Relationship to Previous King`, .keep_all = TRUE)

Monarchs2 <- Monarchs%>%
  arrange(`#`)


write.csv(Monarchs2, "IsraelMonarchs_v2.csv")
