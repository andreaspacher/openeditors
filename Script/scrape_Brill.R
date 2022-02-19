library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "Brill") %>%
  distinct()
journals$url <- stringr::str_remove(journals$url, "(?<=overview.xml).*")
journals$url <- paste0(journals$url, "?contents=editorialcontent-49636")

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
  
  journalname <- rvest::html_nodes(wholepage, css = "h1")
  journalname <- rvest::html_text(journalname)
  journalname <- trimws(journalname)
  journalname <- unique(journalname)
  
  issn <- rvest::html_node(wholepage, css = ".pr-4 > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > dl:nth-child(1) > dd:nth-child(2)")
  issn <- rvest::html_text(issn)
  issn <- trimws(issn)
  
  webpage <- rvest::html_nodes(wholepage, css = "#container-49624-item-49636 > div > div > div")
  
  webpage <- toString(webpage)
  
  editorsnodes <- stringr::str_extract_all(
    webpage,
    "(?<=<b>)[\\S\\s]*?(?=(<b>|</div>))"
  )
  
  if (length(editorsnodes[[1]]) > 0) {
    roles <- lapply(editorsnodes, function(x) stringr::str_extract(x, "^.+?(?=</b>)"))
    roles <- unlist(roles)
    
    peoplesnodes <- lapply(editorsnodes, function(x) stringr::str_extract_all(x, "(?<=(<br />|<br>))[\\S\\s]{15,180}?(?=(<br|$))"))
    peoplesnodes <- lapply(rapply(peoplesnodes, enquote, how = "unlist"), eval)
    
    people <- lapply(peoplesnodes, function(x) stringr::str_extract(x, "^.+?(?=(,|\\n<i>|$))"))
    
    affiliations <- lapply(peoplesnodes, function(x) stringr::str_extract(x, "(?<=(<i>|<br>.{0,40},)).{8,120}?(?=(</i>|\\n))"))
    
    EdB <- try(
      do.call(rbind, Map(data.frame, editor = people, affiliation = affiliations, role = roles)),
      silent = TRUE)
    if (inherits(EdB, "try-error")) {
      print("--- some formatting error?")
      next
    } else {
      EdB$journal <- journalname
      EdB$publisher <- "Brill"
      EdB$url <- journals$url[i]
      EdB$issn <- issn
      EdB$date <- Sys.Date()
    }
  } else {
    EdB <- data.frame(
      "editor" = NA,
      "affiliation" = NA,
      "role" = NA,
      "journal" = journalname,
      "publisher" = "Brill",
      "url" = journals$url[i],
      "issn" = issn,
      "date" = Sys.Date()
    )
  }
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(6)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\Brill-", Sys.Date(), ".tsv"))
