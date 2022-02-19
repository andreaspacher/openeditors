library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "Longdom") %>%
  distinct()

journals$url <- stringr::str_remove(journals$url, ".html$")
journals$url <- paste0(journals$url, "/editorial-board.html")

# prepare the scraping process
EdList <- list()

for(i in 1:nrow(journals)) {
  
  printtext <- paste(i, journals$url[i], sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(rvest::session(
    journals$url[i],
    httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  ), silent = TRUE)
  
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  journalname <- rvest::html_node(wholepage, css = "h1")
  journalname <- rvest::html_text(journalname)
  if (grepl("Open Access$", journalname)) {
    journalname <- stringr::str_remove(journalname, "Open Access")
  }
  
  webpage <- rvest::html_nodes(wholepage, css = ".card-text.font-size-3")
  
  people <- rvest::html_nodes(webpage, "strong")
  people <- rvest::html_text(people)
  
  affiliations <- rvest::html_text(webpage)
  affiliations <- trimws(affiliations)
  affiliations <- stringr::str_remove(affiliations, ".*")
  affiliations <- trimws(affiliations)
  
  EdB <- data.frame(
    "editor" = people,
    "role" = rep("Editorial Board", length(people)),
    "affiliation" = affiliations,
    "journal" = journalname,
    "publisher" = "Longdom",
    "issn" = NA,
    "url" = journals$url[i],
    "date" = Sys.Date()
  )
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(6)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\Longdom-", Sys.Date(), ".tsv"))
