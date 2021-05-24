IMEDPUB <- read.csv("Data\\iMedPub\\imedpub.csv")

# modify URL
IMEDPUB$url <- paste0(IMEDPUB$url, "editors.php")

# set scraping function
get_editors <- function(journal_url) {
  if (grepl("imedpub.com", journal_url)) {

    wholepage <- xml2::read_html(url(journal_url))

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
    EdB$url <- journal_url
    EdB$date <- Sys.Date()
    
    return(EdB)
    Sys.sleep


    return(EdB)
    Sys.sleep(8)
  }
}

# execute scraping
EdList <- list()

for(i in 1:nrow(IMEDPUB)) {
  
  printtext <- paste(i, IMEDPUB$url[i], sep=": ")
  print(printtext)
  
  EdList[[i]] <- get_editors(IMEDPUB$url[i])
  
  Sys.sleep(3)
}
library(tidyverse)
DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, role, editor, affiliation, url, date) %>%
  distinct()

write.csv(DF, file = paste0("Output\\iMedPub\\iMedPub-",i,".csv"), fileEncoding = "UTF-8")
