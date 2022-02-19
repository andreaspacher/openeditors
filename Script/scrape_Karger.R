library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "Karger") %>%
  distinct()

journals$url <- stringr::str_replace(journals$url, "//Journal", "/Journal")
journals$url <- stringr::str_replace(journals$url, "/Home/", "/EditorialBoard/")

# prepare the scraping process
EdList <- list()

for(i in 1:nrow(journals)) {
  
  editors_url <- journals$url[i]
  
  printtext <- paste(i, editors_url, sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(rvest::session(
    editors_url,
    httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  ),
  silent = TRUE)
  
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  # if so, fetch the data
  journalname <- rvest::html_node(wholepage, css = "h2")
  journalname <- rvest::html_text(journalname)
  
  issn <- rvest::html_nodes(wholepage, css = ".sidebar > div.search.bright")
  issn <- rvest::html_text(issn)
  issn <- stringr::str_extract(issn, "(?<=issn.).*?(?=w)")
  issn <-issn[!is.na(issn)]
  
  webpage <- rvest::html_node(wholepage, css = "#editorialboard")
  
  # get h3 headings
  headings <- rvest::html_nodes(webpage, "h3")
  headings <- rvest::html_text(headings)
  
  # get raw text
  raw.text <- rvest::html_text(webpage)
  
  # split raw text on h3 headings and put in a list
  list.members <- list()
  raw.text.2 <- raw.text
  for (h in headings) {
    # split on headings
    b <- strsplit(raw.text.2, h, fixed = TRUE)
    # split members using \n as separator
    c <- strsplit(b[[1]][1], "\n", fixed = TRUE)
    # clean empty elements from vector
    c <- list(c[[1]][c[[1]] != ""])
    # add vector of member to list
    list.members <- c(list.members, c)
    # update text
    raw.text.2 <- b[[1]][2]
  }
  # remove first element of main list
  list.members <- list.members[2:length(list.members)]
  # add final segment of raw.text to list
  c <- strsplit(raw.text.2, "\n", fixed = TRUE)
  c <- list(c[[1]][c[[1]] != ""])
  list.members <- c(list.members, c)
  # add names to list
  names(list.members) <- headings
  
  people <- lapply(list.members, function(x) stringr::str_extract(x, "^.*?(?= (-|–))"))
  
  affiliations <- lapply(list.members, function(x) stringr::str_extract(x, "(?<= (-|–)).*$"))
  affiliations <- unlist(unname(affiliations))
  affiliations <- trimws(affiliations)
  
  EdB <- stack(people)
  names(EdB) <- c("editor", "role")
  EdB$affiliation <- affiliations
  EdB$journal <- journalname
  EdB$publisher <- "Karger"
  EdB$issn <- issn
  EdB$url <- editors_url
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\karger-", Sys.Date(), ".tsv"))
