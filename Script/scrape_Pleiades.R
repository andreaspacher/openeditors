library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "Pleiades") %>%
  distinct()

journals$url <- gsub("search/?name=", "", journals$url, fixed = TRUE)
journals$url <- paste0(journals$url, "/board/")
journals$url <- gsub("journals", "journal", journals$url, fixed = TRUE)


# prepare the scraping process
EdList <- list()

for(i in 1:nrow(journals)) {
  
  printtext <- paste(i, journals$url[i], sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(rvest::session(journals$url[i],
                                  httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20"))
                   , silent = TRUE)
  
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  journalname <- rvest::html_node(wholepage, css = "h1")
  journalname <- rvest::html_text(journalname)
  
  issn <- rvest::html_node(wholepage, xpath = "//*[contains(text(),'ISSN PRINT')]")
  issn <- rvest::html_text(issn)
  issn <- stringr::str_extract(issn, "(?<=: ).*(?=ISSN|$)")
  issn <- substr(issn, start = 1, stop = 9)
  
  
  webpage <- rvest::html_nodes(wholepage, css = "div.tab-content")
  webpage <- rvest::html_nodes(webpage, css = ".collapse")
  
  people <- lapply(webpage, function(x) rvest::html_nodes(x, css = "td:nth-child(1)"))
  people <- lapply(people, function(x) rvest::html_text(x))
  
  affiliations <- lapply(webpage, function(x) rvest::html_nodes(x, css = "td:nth-child(2)"))
  affiliations <- lapply(affiliations, function(x) rvest::html_text(x))
  affiliations <- unlist(affiliations)
  
  roles <- rvest::html_nodes(wholepage, css = "h4")
  roles <- rvest::html_text(roles)
  roles <- trimws(roles)
  
  names(people) <- roles
  
  EdB <- stack(people)
  names(EdB) <- c("editor", "role")
  EdB$affiliation <- NA_character_
  EdB$affiliation[seq_along(affiliations)] <- affiliations
  EdB$journal <- journalname
  EdB$publisher <- "Pleiades"
  EdB$issn <- issn
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\Pleiades-", Sys.Date(), ".tsv"))
