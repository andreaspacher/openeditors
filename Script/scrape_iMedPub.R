library(tidyverse)
library(rvest)
library(RSelenium)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "iMedPub") %>%
  distinct()

journals$url <- paste0(journals$url, "editors.php")

# not working... not sure why!

# prepare the scraping process
EdList <- list()

for(i in 1:nrow(journals)) {
  
  printtext <- paste(i, journals$url[i], sep=": ")
  print(printtext)
  
  rD <- RSelenium::rsDriver(browser="firefox", port=4546L, verbose=F)
  remDr <- rD[["client"]]
  remDr$maxWindowSize()
  remDr$navigate(journals$url[i])
  
  # start scraping
  # wholepage <- try(rvest::session(
  #   url(journals$url[i]),
  #   httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  # ), silent = TRUE)
  # wholepage <- try(
  #   xml2::read_html(url(journals$url[i]))
  # )
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  Sys.sleep(3.5)
  
  wholepage <- remDr$getPageSource()
  wholepage <- xml2::read_html(wholepage[[1]])
  
  journalname <- rvest::html_node(wholepage, css = "h1")
  journalname <- rvest::html_text(journalname)
  
  webpage <- rvest::html_nodes(wholepage, css = "div.wlcme_imdpub")
  
  roles <- rvest::html_nodes(webpage, css = ".editor_type")
  roles <- rvest::html_text(roles)
  
  webpage <- stringr::str_extract_all(webpage, "(?<=<div class=\"editor_type\">)[\\S\\s]*?(?=(<div|</table))")
  
  people <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=<b>)[\\S\\s]*?(?=</td>)"))
  names(people[[1]]) <- roles
  
  affiliations <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=</b>)[\\S\\s]*?(?=</td>)"))
  affiliations <- unlist(affiliations)
  affiliations <- ifelse(grepl(",[^a-zA-Z]", affiliations), sub("..", "", affiliations), affiliations)
  
  EdB <- stack(people[[1]])
  names(EdB) <- c("editor", "role")
  EdB$affiliation <- affiliations
  EdB$journal <- journalname
  EdB$publisher <- "iMedPub"
  EdB$issn <- NA
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  remDr$close()
  gc()
  rD$server$stop()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  
  Sys.sleep(5)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

DF$editor <- stringr::str_remove(DF$editor, "</b>[\\S\\s]*$")

write_tsv(DF, paste0("Output\\2022-Scraping\\iMedPub-", Sys.Date(), ".tsv"))
