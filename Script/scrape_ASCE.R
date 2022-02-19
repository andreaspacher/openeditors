library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "ASCE") %>%
  distinct()

journals$url <- paste0(journals$url, "/editorialboard")
journals$url <- gsub("/loi/", "/page/", journals$url)

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
  
  journalname <- rvest::html_nodes(wholepage, css = "h2")
  journalname <- rvest::html_text(journalname)
  
  webpage <- rvest::html_nodes(wholepage, css = "#snippet-Editorial_Boards > div:nth-child(1) > div:nth-child(1)")
  webpage <- stringr::str_extract_all(webpage, "(?<=<b>)[\\S\\s]*?(?=(<b>|</div>))")
  webpage[[1]] <- webpage[[1]][nchar(webpage[[1]]) > 30] # delete first part which does not contain any names
  
  roles <- lapply(webpage[[1]], function(x) stringr::str_extract_all(x, "^.*?(?=(:|</b>))"))
  roles <- unlist(roles)
  
  people <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=<br>.?\n).*?(?=,)"))
  names(people[[1]]) <- roles
  
  affiliations <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=<i>).*?(?=<br>)"))
  affiliations <- unlist(affiliations)
  
  EdB <- stack(people[[1]])
  names(EdB) <- c("editor", "role")
  EdB$affiliation <- affiliations
  EdB$affiliation <- (gsub("<.*?>", "", EdB$affiliation))
  EdB$journal <- journalname
  EdB$publisher <- "American Society of Civil Engineers"
  EdB$issn <- NA
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\ASCE-", Sys.Date(), ".tsv"))
