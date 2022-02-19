library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "Allied Academies") %>%
  distinct()

journals$url <- paste0(journals$url, "editors.php")
journals$url <- stringr::str_remove(journals$url, "^NA")

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
  
  webpage <- rvest::html_nodes(wholepage, css = "div.card-body")
  
  roles <- rvest::html_nodes(webpage, css = "strong")
  roles <- rvest::html_text(roles)
  
  webpage <- stringr::str_extract_all(webpage, "(?<=</strong>)[\\S\\s]*?(?=(<div|</ul))")
  
  people <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=<b>)[\\S\\s]*?(?=</b>)"))
  names(people[[1]]) <- roles
  
  affiliations <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=</b>)[\\S\\s]*?(?=</li>)"))
  affiliations <- unlist(affiliations)
  affiliations <- ifelse(grepl(",[^a-zA-Z]", affiliations), sub("..", "", affiliations), affiliations)
  
  EdB <- stack(people[[1]])
  names(EdB) <- c("editor", "role")
  EdB$affiliation <- affiliations
  EdB$journal <- journalname
  EdB$publisher <- "Allied Academies"
  EdB$issn <- NA
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(8)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

#DF$affiliation <- gsub("<.*?>", "", DF$affiliation)

write_tsv(DF, paste0("Output\\2022-Scraping\\AlliedAcademies-", Sys.Date(), ".tsv"))
