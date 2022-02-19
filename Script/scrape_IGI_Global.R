library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "IGI Global Publishing") %>%
  distinct()

# prepare the scraping process
EdList <- list()

for(i in 1:nrow(journals)) {
  
  printtext <- paste(i, journals$url[i], sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(xml2::read_html(url(journals$url[i])), silent = TRUE)
  
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  journalname <- rvest::html_nodes(wholepage, css = "#lblTitle")
  journalname <- rvest::html_text(journalname)
  
  issn <- rvest::html_node(wholepage, css = "div.isbn-doi-inner > div > span > span:nth-child(2)")
  issn <- rvest::html_text(issn)
  
  webpage <- rvest::html_nodes(wholepage, css = "div.editorial-board-container")
  webpage <- rvest::html_nodes(webpage, xpath = '//div[contains(@id, "divEditorialBoard")]')
  
  people <- lapply(webpage, function(x) rvest::html_nodes(x, css = "dd"))
  people <- purrr::compact(people)
  people <- lapply(people, function(x) rvest::html_text(x))
  people <- lapply(people, function(x) trimws(x))
  people <- lapply(people, function(x) stringr::str_extract(x, ".*?(?=,)"))
  
  affiliations <- lapply(webpage, function(x) rvest::html_nodes(x, css = "dd"))
  affiliations <- purrr::compact(affiliations)
  affiliations <- lapply(affiliations, function(x) rvest::html_text(x))
  affiliations <- lapply(affiliations, function(x) stringr::str_extract(x, "(?<=, ).*"))
  affiliations <- lapply(affiliations, function(x) trimws(x))
  
  orcid <- lapply(webpage, function(x) rvest::html_nodes(x, css = "dd"))
  orcid <- purrr::compact(orcid)
  orcid <- lapply(orcid, function(x) rvest::html_text(x))
  orcid <- lapply(orcid, function(x) stringr::str_extract(x, "(?=https://orcid.org).*$"))
  
  roles <- rvest::html_nodes(wholepage, css = "dt")
  roles <- rvest::html_text(roles)
  
  EdB <- do.call(rbind, Map(data.frame, editor = people, affiliation = affiliations, role = roles))
  EdB$orcid <- unlist(orcid)
  EdB$journal <- journalname
  EdB$publisher <- "IGI Global"
  EdB$issn <- issn
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, orcid, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\IGI_Global-", Sys.Date(), ".tsv"))
