library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "APA") %>%
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
  
  journalname <- rvest::html_nodes(wholepage, css = "h1")
  journalname <- rvest::html_text(journalname)
  journalname <- trimws(journalname)
  
  issn <- rvest::html_node(wholepage, css = "#topcontent > div > section.info > div.prod_detail > div:nth-child(1) > span")
  issn <- rvest::html_text(issn)
  
  webpage <- rvest::html_nodes(wholepage, css = "#tab_item-1")
  
  roles <- rvest::html_nodes(webpage, css = "h2")
  roles <- rvest::html_text(roles)
  
  webpage <- stringr::str_extract_all(webpage, "(?<=<h2>)[\\S\\s]*?(?=(<h2>|</section>))")
  
  people <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=<p>)[\\S\\s]*?(?=<br>)"))
  names(people[[1]]) <- roles
  
  affiliations <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=<em>)[\\S\\s]*?(?=</em>)"))
  affiliations <- unlist(affiliations)
  
  EdB <- stack(people[[1]])
  names(EdB) <- c("editor", "role")
  EdB$affiliation <- affiliations
  EdB$journal <- journalname
  EdB$publisher <- "American Psychological Association"
  EdB$issn <- issn
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\APA-", Sys.Date(), ".tsv"))
