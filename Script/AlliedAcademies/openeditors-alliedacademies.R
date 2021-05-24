df <- read.csv("Data\\AlliedAcademies\\alliedacademies.csv")

# modify URL
df$url <- paste0(df$url, "editors.php")

# set scraping function
get_editors <- function(journal_url) {
  if (grepl("alliedacademies.org", journal_url)) {

    wholepage <- xml2::read_html(url(journal_url))

    journalname <- rvest::html_node(wholepage, css = "h1")
    journalname <- rvest::html_text(journalname)

    webpage <- rvest::html_nodes(wholepage, css = "div.card-body")
    
    roles <- rvest::html_nodes(webpage, css = "strong")
    roles <- rvest::html_text(roles)

    webpage <- stringr::str_extract_all(webpage, "(?<=</strong>)[\\S\\s]*?(?=(<div|</ul))")
    
    people <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=<b>)[\\S\\s]*?(?=</b>)"))
    names(people[[1]]) <- roles
    
    affiliations <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=</b>)[\\S\\s]*?(?=</li>)"))
    affiliations <- unlist(affiliations)
    affiliations <- ifelse(grepl(",[^a-zA-Z]", affiliations), sub("..", "", affiliations), affiliations)
    
    EdB <- stack(people[[1]])
    names(EdB) <- c("editor", "role")
    EdB$affiliation <- affiliations
    EdB$journal <- journalname
    EdB$publisher <- "Allied Academies"
    EdB$issn <- NA
    EdB$url <- journal_url
    EdB$date <- Sys.Date()


    return(EdB)
    Sys.sleep(8)
  }
}

# execute scraping
EdList <- list()

for(i in 105:nrow(df)) {
  
  printtext <- paste(i, df$url[i], sep=": ")
  print(printtext)
  
  EdList[[i]] <- get_editors(df$url[i])
  
  Sys.sleep(2)
}
library(tidyverse)
DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, role, editor, affiliation, url, date)

write.csv(DF, file = paste0("Output\\AlliedAcademies\\AlliedAcademies-",i,".csv"), fileEncoding = "UTF-8")
