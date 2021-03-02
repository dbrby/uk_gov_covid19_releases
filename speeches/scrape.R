library(rvest)
library(tidyverse)
library(lubridate)

url <- "https://www.gov.uk/government/collections/slides-and-datasets-to-accompany-coronavirus-press-conferences"

pg <- read_html(url)



links_back <- pg %>% 
  html_nodes(".gem-c-document-list__item-link") %>%
  html_attr('href')



titles_uk <- pg %>%
  html_nodes(".gem-c-document-list__item-title") %>%
  html_text()

links_uk <- paste0("https://www.gov.uk/", links_back)
links_uk <- links_uk[grepl("speeches", links_uk)]

titles_uk <- titles_uk[grepl("statement", titles_uk)]


uk_covid <- data.frame(titles_uk, links_uk)

uk_covid$speaker <- stringr::word(uk_covid$titles_uk, 1, sep = "'")

uk_covid$date <- stringr::word(uk_covid$titles, -1, sep = ":") %>%
  dmy()

uk_covid$date[is.na(uk_covid$date)] <- as.Date("2020-05-16")

uk_covid$text <-
  lapply(uk_covid$links_uk, function(text) {
    text %>% read_html() %>%
      html_nodes(".content-bottom-margin") %>%
      html_text() %>%
      as.character()
  })

uk_covid$clean_text <- stringr::word(uk_covid$text, -1, 
                                     sep = "(Transcript of the speech, exactly as it was delivered)"
                                     )

uk_covid$clean_text <- stringr::word(uk_covid$clean_text, 1, 
                                     sep = "Published"
)

uk_covid$clean_text <- gsub(")\n    \n\n\n      \n    \n          \n", "", uk_covid$clean_text, fixed = T)




uk_covid$clean_text <- str_remove_all(uk_covid$clean_text, "[\r\n]") %>% trimws()

uk_covid$clean_text[1]

write_csv(uk_covid, "uk_covid19_press_statements.csv")

