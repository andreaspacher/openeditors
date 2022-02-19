library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "Royal Society of Chemistry") %>%
  distinct()

# prepare the scraping process
EdList <- list()

for(i in 1:nrow(journals)) {
  
  editors_url <- journals$url[i]
  
  printtext <- paste(i, journals$url[i], sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(rvest::html_session(
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
  
  issn <- rvest::html_node(wholepage, css = "#subscription-information")
  issn <- toString(issn)
  issn <- stringr::str_extract(issn, "(?<=ISSN ).{8,10}(?=( |</p>))")
  issn <- substr(issn, 1, 9)
  
  webpage <- rvest::html_nodes(wholepage, css = "#boards-staff")
  
  roles <- rvest::html_nodes(webpage, "span.tab__heading")
  roles <- rvest::html_text(roles)
  
  editorsnodes <- rvest::html_nodes(webpage, ".content-block, hashtaglink") # or div.block tabsinnerblock
  
  editorialboard <- toString(editorsnodes[[1]])
  editorialboard2 <- unlist(stringr::str_extract_all(editorialboard, "(?<=<h4>)[\\S\\s]*?(?=(<h4>|$))"))
  if (length(editorialboard2) > 0) {
    editorialboard <- editorialboard2
  } else {
    editorialboard <- toString(editorsnodes[[1]])
  }
  
  edboard_roles <- unlist(stringr::str_extract_all(editorialboard, "^.*?(?=</h4>)"))
  
  edboard_ppl <- lapply(editorialboard, function(x) stringr::str_extract_all(x, "(?<=<strong>).*?(?=(</strong>|</a>))"))
  edboard_ppl <- lapply(rapply(edboard_ppl, enquote, how = "unlist"), eval)
  edboard_ppl <- lapply(edboard_ppl, function(x) x[nchar(x) > 4])
  if (length(editorialboard2) > 0) {
    names(edboard_ppl) <- paste0("Editorial Board: ", edboard_roles)
  } else {
    names(edboard_ppl) <- "Editorial Board"
  }
  
  edboard_affiliations <- unlist(lapply(editorialboard, function(x) stringr::str_extract_all(x, "(?<=,).*(?=</p>)")))
  edboard_affiliations <- unlist(lapply(edboard_affiliations, function(x) x[nchar(x) > 4]))
  
  editorsnodes[[1]] <- NULL
  peoplesnodes <- lapply(editorsnodes, function(x) rvest::html_nodes(x, "p"))
  
  people <- lapply(peoplesnodes, function(x) rvest::html_nodes(x, "strong"))
  people <- lapply(peoplesnodes, function(x) rvest::html_text(x))
  people <- lapply(people, function(x) stringr::str_extract(x, "^.*?(?=,)"))
  
  affiliations <- lapply(peoplesnodes, function(x) stringr::str_extract(x, "(?<=,).*(?=</p>)"))
  
  # MERGE THE THREE CATEGORIES TO DATA FRAMES
  advisory_board <- data.frame("editor" = people[[1]], "role" = "Advisory Board", "affiliation" = affiliations[[1]])
  editorial_office <- data.frame("editor" = people[[2]], "role" = paste0("Editorial Office: ", affiliations[[2]]), "affiliation" = NA)
  edboard <- plyr::ldply(edboard_ppl, data.frame)
  names(edboard) <- c("role", "editor")
  edboard$affiliation <- edboard_affiliations
  
  EdB <- do.call("rbind", list(edboard, advisory_board, editorial_office))
  
  cleanHTML <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  
  EdB[1:3] <- lapply(EdB[1:3], cleanHTML)
  EdB <- as.data.frame(EdB)
  
  EdB$journal <- journalname
  EdB$publisher <- "Royal Society of Chemistry"
  EdB$url <- journals$url[i]
  EdB$issn <- issn
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\RSC-", Sys.Date(), ".tsv"))
