LONGDOM <- read.csv("Data\\Longdom\\longdom.csv")

# modify URL
LONGDOM$url <- stringr::str_remove(LONGDOM$url, ".html$")
LONGDOM$url <- paste0(LONGDOM$url, "/editorial-board.html")

# set scraping function
get_editors <- function(journal_url) {
  if (grepl("longdom.org", journal_url)) {

    wholepage <- xml2::read_html(url(journal_url))

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
      "url" = journal_url,
      "date" = Sys.Date()
    )


    return(EdB)
    Sys.sleep(8)
  }
}

# execute scraping
EdList <- list()

for(i in 1:nrow(LONGDOM)) {
  
  printtext <- paste(i, LONGDOM$url[i], sep=": ")
  print(printtext)
  
  EdList[[i]] <- get_editors(LONGDOM$url[i])
  
  Sys.sleep(2)
}
library(tidyverse)
DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, role, editor, affiliation, url, date)

write.csv(DF, file = paste0("Output\\Longdom\\Longdom-",i,".csv"), fileEncoding = "UTF-8")
