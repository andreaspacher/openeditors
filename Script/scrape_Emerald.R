library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "Emerald") %>%
  distinct()

# prepare the scraping process
EdList <- list()

for(i in 1:nrow(journals)) {
  
  printtext <- paste(i, journals$url[i], sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(rvest::html_session(
    journals$url[i],
    httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  ), silent = TRUE)
  
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  journalname <- rvest::html_nodes(wholepage, css = ".journal-header__title")
  journalname <- rvest::html_text(journalname)
  journalname <- trimws(journalname)
  
  issn <- rvest::html_nodes(wholepage, css = ".journal-header__issn")
  issn <- toString(issn)
  issn <- stringr::str_extract(issn, "(?<=ISSN:\\s{0,40}).*(?=<)")
  issn <- trimws(issn)
  
  webpage <- rvest::html_nodes(wholepage, css = ".editorial-team")
  
  people <- rvest::html_nodes(webpage, css = "ul")
  people[1] <- NULL
  people <- lapply(people, function(x) rvest::html_nodes(x, css = "li"))
  people <- sapply(people, function(x) stringr::str_extract(x, "(?<=<li>\\s{1,50})(.|\\s)*?(?=<)"))
  people <- sapply(people, function(x) trimws(x))
  people <- sapply(people, function(x) stringr::str_squish(x))
  
  roles <- rvest::html_nodes(webpage, css = "h4")
  roles <- rvest::html_text(roles)
  
  # NOT WORKING YET (disentangling affiliation and country)
  affiliations <- rvest::html_nodes(webpage, css = "ul")
  affiliations[1] <- NULL
  affiliations <- lapply(affiliations, function(x) stringr::str_extract_all(x, "(?<=<em>)[\\s\\S]*?(?=<li>|$)"))
  affiliations <- lapply(affiliations, function(x) {
    lapply(x, function(y) stringr::str_extract(y, "^[\\s\\S]+(?=</em>)"))
  })
  affiliations <- lapply(affiliations, function(x) {
    lapply(x, function(y) stringr::str_replace_all(y, "[\t|\n]|(</em>)", ""))
  })
  affiliations <- lapply(affiliations, function(x) {
    lapply(x, function(y) stringr::str_replace_all(y, "(-<em>)", ", "))
  })
  
  affiliations <- lapply(affiliations, function(x) unlist(x))
  affiliations <- lapply(affiliations, function(x) if (identical(x, character(0))) NA_character_ else x)
  
  
  EdB <- do.call(rbind, Map(data.frame, people = people, affiliation = affiliations, roles = roles))
  EdB$journalname <- journalname
  EdB$issn <- issn
  EdB$url <- journals$url[i]
  EdB$publisher <- "Emerald"
  EdB$date <- Sys.Date()
  colnames(EdB) <- c("editor", "affiliation", "role", "journal", "issn", "url", "publisher", "date")
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\Emerald-", Sys.Date(), ".tsv"))
