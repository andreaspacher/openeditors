library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "Hindawi") %>%
  distinct()

journals$url <- paste0(journals$url, "editors/")

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
  
  journalname <- rvest::html_nodes(wholepage, css = ".article__SubtitleStyled-sc-10qhnp8-2")
  journalname <- rvest::html_text(journalname)
  
  #issn <- toString(wholepage)
  #issn <- stringr::str_extract(issn, "(?<=\"journal_issn\":\").*?(?=\",)")
  
  webpage <- rvest::html_nodes(wholepage, css = "#__next > div > div > main > div > div.threeColumn__ThreeColumnWrapper-sc-1bf4078-0.dDyxFF.threeColumnWrapper > div.threeColumn__ContentWrapper-sc-1bf4078-3.bbzREB.contentWrapper > div > div > div")
  
  people <- rvest::html_nodes(webpage, css = "ul")
  people <- lapply(people, function(x) rvest::html_nodes(x, css = "li"))
  if (grepl("<strong>", people[[1]][1], fixed = T)) {
    people <- lapply(people, function(x) rvest::html_nodes(x, css = "strong"))
  } else {
    people <- lapply(people, function(x) rvest::html_nodes(x, css = "span"))
  }
  people <- sapply(people, function(x) stringr::str_extract(x, "(?<=>)(.|\\s)*?(?=<)"))
  people <- purrr::compact(people)
  
  affiliations <- rvest::html_nodes(webpage, css = "ul")
  affiliations <- lapply(affiliations, function(x) rvest::html_nodes(x, css = "li"))
  affiliations <- sapply(affiliations, function(x) stringr::str_extract(x, "(?<=>, )(.|\\s)*?(?=<)"))
  affiliations <- purrr::compact(affiliations)
  
  orcid <- rvest::html_nodes(webpage, css = "ul")
  orcid <- lapply(orcid, function(x) rvest::html_nodes(x, css = "li"))
  orcid <- lapply(orcid, function(x) ifelse(grepl("orcid", x), stringr::str_extract_all(x, "(?<=orcid.org/)(.|\\s)*?(?=\")"), NA))
  orcid <- purrr::compact(orcid)
  orcid <- unlist(orcid)
  
  roles <- rvest::html_nodes(webpage, css = "h2")
  roles <- rvest::html_text(roles)
  
  EdB <- do.call(rbind, Map(data.frame, editor = people, affiliation = affiliations, role = roles))
  EdB$orcid <- orcid
  EdB$journal <- journalname
  EdB$publisher <- "Hindawi"
  #EdB$issn <- issn
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, role, editor, orcid, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\Hindawi-", Sys.Date(), ".tsv"))
