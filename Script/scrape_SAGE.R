library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "SAGE") %>%
  distinct()

journals$url <- stringr::str_replace(journals$url, "/home/", "/editorial-board/")

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
  
  journalname <- rvest::html_nodes(wholepage, xpath = '//*[@id="e3c018c7-8573-4acd-93ae-0ff4b1f3baf3"]/div/div')
  journalname <- rvest::html_text(journalname)
  
  issn <- rvest::html_nodes(wholepage, xpath = '//*[@id="4c8a6b02-2938-4d47-8324-a3ffe9c0abdd"]/div/div/div/div/div[5]/span[1]')
  issn <- toString(issn)
  issn <- stringr::str_extract(issn, "(?<=ISSN: ).*(?=<)")
  
  webpage <- rvest::html_nodes(wholepage, xpath = '//*[@id="5dfa7b11-3157-4585-b786-54aa88233446"]/div/div/div')
  
  people <- rvest::html_nodes(webpage, xpath = "//div[@class='editorial-board']/descendant::table")
  people <- rvest::html_table(people, fill = TRUE)
  
  roles <- rvest::html_nodes(webpage, xpath = "//div[@class='ed-board-name']")
  roles <- rvest::html_text(roles)
  
  if (length(roles) > 0) {
    EdB <- do.call(rbind, Map(data.frame, people = people, roles = roles))
    EdB$journalname <- journalname
    EdB$publisher <- "SAGE"
    EdB$url <- journal_url
    EdB$issn <- issn
    EdB$date <- Sys.Date()
    colnames(EdB) <- c("editor", "affiliation", "role", "journal", "publisher", "url", "issn", "date")
  } else {
    EdB <- data.frame(
      "editor" = NA,
      "affiliation" = NA,
      "role" = NA,
      "journal" = journalname,
      "publisher" = "SAGE",
      "url" = journals$url[i],
      "issn" = issn,
      "scrapedate" = Sys.Date()
    )
  }
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\SAGE-", Sys.Date(), ".tsv"))
