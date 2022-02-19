library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "John Benjamins") %>%
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
  
  journalname <- rvest::html_node(wholepage, css = "h1")
  journalname <- rvest::html_text(journalname)
  journalname <- stringr::str_squish(journalname)
  
  issn <- rvest::html_node(wholepage, css = "div.booktext:nth-child(5)")
  issn <- rvest::html_text(issn)
  issn <- stringr::str_extract(issn, "(?<=ISSN ).*?(?= )")
  
  webpage <- rvest::html_nodes(wholepage, css = "#board")
  webpage <- rvest::html_nodes(webpage, css = ".contribgroup")
  
  people <- lapply(webpage, function(x) rvest::html_nodes(x, css = "span.name"))
  people <- lapply(people, function(x) rvest::html_text(x))
  people <- lapply(people, function(x) stringr::str_squish(x))
  
  affiliations <- lapply(webpage, function(x) rvest::html_nodes(x, css = ".member"))
  affiliations <- lapply(affiliations, function(x) stringr::str_extract(x, "(?<=ISSN pipe\"> | </span>).*?(?=(<span|</div>))"))
  affiliations <- unlist(affiliations)
  
  
  orcid <- lapply(webpage, function(x) rvest::html_nodes(x, css = ".member"))
  orcid <- lapply(orcid, function(x) ifelse(grepl("orcid", x), stringr::str_extract_all(x, "(?<=orcid.org/)(.|\\s)*?(?=\")"), NA))
  orcid <- purrr::compact(orcid)
  orcid <- unlist(orcid)
  
  roles <- rvest::html_nodes(wholepage, css = ".contribgroup_heading")
  roles <- rvest::html_text(roles)
  
  names(people) <- roles
  
  EdB <- stack(people)
  colnames(EdB) <- c("editor", "role")
  EdB$affiliation <- affiliations
  EdB$orcid <- orcid
  EdB$journal <- journalname
  EdB$publisher <- "John Benjamins"
  EdB$issn <- issn
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(6)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, orcid, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\johnbenjamins-", Sys.Date(), ".tsv"))
