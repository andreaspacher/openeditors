library(tidyverse)
library(rvest)

# get all CUP journals
journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true"))
journals <- journals %>%
  filter(publisher == "Cambridge University Press") %>%
  distinct()

# get journal abbreviations from the URLs
Abbr <- stringr::str_extract_all(journals$url, "(?<=/journals/).*")
Abbr <- unlist(Abbr)

# prepare the scraping process
EdList <- list()

for(i in 366:nrow(journals)) {
  
  # get url
  editors_url <- paste0("https://www.cambridge.org/core/journals/"
                        , Abbr[i]
                        ,"/information/editorial-board")
  
  # notify about process
  printtext <- paste(i, editors_url, sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(xml2::read_html(editors_url),
                   silent = TRUE)
  
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  journalname <- rvest::html_nodes(wholepage, xpath = "/html/body/section[4]/div/div/div/h1")
  journalname <- rvest::html_text(journalname)
  journalname <- trimws(journalname)
  
  webpage <- rvest::html_nodes(wholepage, ".editorial-position")
  
  editors <- lapply(webpage, function(x) rvest::html_nodes(x, "p"))
  editors <- lapply(editors, function(x) rvest::html_text(x))
  editors <- lapply(editors, function(x) trimws(x))
  
  roles <- lapply(webpage, function(x) rvest::html_nodes(x, "h6"))
  roles <- lapply(roles, function(x) rvest::html_text(x))
  roles <- unlist(roles)
  names(editors) <- roles
  
  EdB <- plyr::ldply(editors, data.frame)
  if (length(EdB) > 0) {
    EdB$journal <- journalname
    EdB$publisher <- "Cambridge University Press"
    EdB$url <- editors_url
    EdB$date <- Sys.Date()
    colnames(EdB) <- c("role", "editor", "journal", "publisher", "url", "date")
    EdB <- dplyr::mutate(EdB, affiliation = stringr::str_extract(editor, "(?<=, ).*"))
    EdB$editor <- lapply(EdB$editor, function(x) stringr::str_extract(x, "^.*?(?=,|$)"))
    
    print(paste0("--- found ", nrow(EdB), " editors!"))
    
  } else {
    EdB <- data.frame(
      "role" = NA,
      "editor" = NA,
      "journal" = journalname,
      "publisher" = "Cambridge University Press",
      "url" = editors_url,
      "date" = Sys.Date()
    )
    
    print("--- Something went wrong")
  }
  
  EdList[[i]] <- EdB
  
  Sys.sleep(4.5)
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, role, editor, affiliation, url, date)
DF <- DF %>% mutate(editor = map_chr(editor, toString))

write_tsv(DF, paste0("Output\\2022-Scraping\\cup-", Sys.Date(), ".tsv"))
