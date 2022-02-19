library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "InderScience") %>%
  distinct()

journals <- journals[journals$journal != "Forgotten?", ]

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
  
  journalname <- rvest::html_nodes(wholepage, css = "h2.display-4")
  journalname <- rvest::html_text(journalname)
  
  issn <- rvest::html_node(wholepage, css = "div.row:nth-child(4) > div:nth-child(2) > dl:nth-child(1)")
  issn <- rvest::html_text(issn)
  issn_online <- stringr::str_extract(issn, "(?<=ISSN online).*?(?=ISSN)")
  issn_print <- stringr::str_extract(issn, "(?<=ISSN print).........")
  
  webpage <- rvest::html_nodes(wholepage, css = "#edboard-content")
  
  roles <- rvest::html_nodes(webpage, css = "h3")
  roles <- rvest::html_text(roles)
  
  peoples <- rvest::html_nodes(webpage, css = "ul")
  peoples <- lapply(peoples, function(x) rvest::html_nodes(x, css = "li"))
  peoples <- sapply(peoples, function(x) rvest::html_text(x))
  
  people <- sapply(peoples, function(x) stringr::str_extract(x, "^(.*?,.*?)(?=,)"))
  
  affiliations <- sapply(peoples, function(x) stringr::str_extract(x, "(?<=(.{1,70},.{1,70}?,)).*"))
  affiliations <- sapply(affiliations, function(x) trimws(x))
  
  EdB <- do.call(rbind, Map(data.frame, editor = people, affiliation = affiliations, role = roles))
  EdB$journal <- journalname
  EdB$issn <- issn_online
  EdB$url <- journals$url[i]
  EdB$publisher <- "Inderscience"
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\Inderscience-", Sys.Date(), ".tsv"))
