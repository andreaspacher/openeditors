library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "SciTechnol") %>%
  distinct()

journals$url <- sub("scitechnol.com/", "scitechnol.com/editorialboard-", journals$url)

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
  
  journalname <- rvest::html_node(wholepage, css = "h2.font-size-30")
  journalname <- rvest::html_text(journalname)
  journalname <- stringr::str_remove(journalname, "\\..*")
  
  issn <- rvest::html_node(wholepage, ".font-size-20")
  issn <- rvest::html_text(issn)
  
  webpage <- rvest::html_nodes(wholepage, css = ".col-md-9")
  
  people <- rvest::html_nodes(webpage, "h4")
  people <- rvest::html_text(people)
  
  affiliations <- rvest::html_nodes(webpage, "p.team-v3-paragraph")
  affiliations <- rvest::html_text(affiliations)
  
  EdB <- data.frame(
    "editor" = people,
    "role" = rep("Editorial Board", length(people)),
    "affiliation" = affiliations,
    "journal" = journalname,
    "publisher" = "SciTechnol",
    "issn" = issn,
    "url" = journals$url[i],
    "date" = Sys.Date()
  )
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(8)
  
}

library(tidyverse)
DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date) %>%
  distinct()

journals2 <- journals[grep("scitechnol.com", journals$url),]
journals2 <- anti_join(journals,journals2)
journals2$url <- paste0("https://www.scitechnol.com/editorialboard-", journals2$url)

EdList2 <- list()

for(i in 1:nrow(journals2)) {
  
  printtext <- paste(i, journals2$url[i], sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(rvest::session(
    journals2$url[i],
    httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  ), silent = TRUE)
  
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  journalname <- rvest::html_node(wholepage, css = "h2.font-size-30")
  journalname <- rvest::html_text(journalname)
  journalname <- stringr::str_remove(journalname, "\\..*")
  
  issn <- rvest::html_node(wholepage, ".font-size-20")
  issn <- rvest::html_text(issn)
  
  webpage <- rvest::html_nodes(wholepage, css = ".col-md-9")
  
  people <- rvest::html_nodes(webpage, "h4")
  people <- rvest::html_text(people)
  
  affiliations <- rvest::html_nodes(webpage, "p.team-v3-paragraph")
  affiliations <- rvest::html_text(affiliations)
  
  EdB <- data.frame(
    "editor" = people,
    "role" = rep("Editorial Board", length(people)),
    "affiliation" = affiliations,
    "journal" = journalname,
    "publisher" = "SciTechnol",
    "issn" = issn,
    "url" = journals2$url[i],
    "date" = Sys.Date()
  )
  
  EdList2[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(8)
  
}

DF2 <- dplyr::bind_rows(EdList2) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date) %>%
  distinct()

DF3 <- rbind(DF, DF2)

DF3 <- DF3 %>%
  filter(!is.na(url)) %>%
  filter(!is.na(editor)) %>%
  mutate(journal = stringr::str_remove(journal, "ISSN: .*$")) %>%
  mutate(issn = stringr::str_remove(issn, "ISSN: "))

write_tsv(DF3, paste0("Output\\2022-Scraping\\SciTechnol-", Sys.Date(), ".tsv"))
