library(tidyverse)
library(rvest)

agent <- httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")

journals_page <- try(rvest::session("https://publications.copernicus.org/open-access_journals/journals_by_subject.html",
                                   agent)
                    , silent = TRUE)


journal_names <- rvest::html_elements(journals_page, css = ".m-0") %>%
  rvest::html_text()
journal_editors_urls <- rvest::html_elements(journals_page, css = "div.journals-container p:nth-child(2) a:nth-child(2)") %>%
  rvest::html_attr("href")

journals <- tibble(name = journal_names) %>%
  filter(!str_detect(`name`, pattern = "\\n"))
journals$url <- journal_editors_urls

# prepare the scraping process
EdList <- list()

for(i in 1:nrow(journals)) {
  
  printtext <- paste(i, journals$url[i], sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(rvest::session(journals$url[i], agent), silent = TRUE)
  
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  journalname <- journals$name[i]

  imprint_url <- httr::parse_url(journals$url[i])
  imprint_url$path <- "imprint.html"
  imprint_wholepage <- rvest::session(httr::build_url(imprint_url), agent)
  
  issn <- rvest::html_element(imprint_wholepage, xpath = "//*[contains(text(),'ISSN')]/span") %>%
    rvest::html_text(trim = TRUE)
  
  editor_div <- wholepage %>%
    rvest::html_elements(css = "div.editorial-board-info")
  
  people <- editor_div %>% rvest::html_elements("h2") %>% 
    rvest::html_text(trim = TRUE) %>%
    stringr::str_squish()
  
  affiliations <- list()
  for(j in 1:length(editor_div)) {
    affil <- editor_div[j] %>% rvest::html_elements(css = "div.editorial-board-affiliation div") %>%
      rvest::html_text(trim = TRUE)
    country <- editor_div[j] %>% rvest::html_elements(css = "div.editorial-board-country div") %>%
      rvest::html_text(trim = TRUE)
    sep <- ifelse(any(length(affil) < 1, length(country) < 1), "", ", ")
    affiliations <- c(affiliations, stringr::str_squish(paste0(affil, sep, country)))
  }

  roles_types <- wholepage %>%
    rvest::html_elements("div.content-container > :not(.editorial-board-reference) h1, :not(.editorial-board-reference) div[class='h1']") %>%
    rvest::html_text(trim = TRUE)
  
  role_containers <- wholepage %>% rvest::html_elements(css = "div.editorial-board")
  role_counts <- sapply(role_containers, function(x) { length(x %>% rvest::html_elements(css = "div.editorial-board-info")) })
  
  roles <- rep(roles_types, role_counts)
  names(people) <- roles
  
  EdB <- stack(people)
  names(EdB) <- c("editor", "role")
  EdB$affiliation <- NA_character_
  EdB$affiliation <- unlist(affiliations)
  EdB$journal <- journalname
  EdB$publisher <- "Copernicus Publications"
  EdB$issn <- issn
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors for ", journalname, "!"))
  
  Sys.sleep(1)
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, file.path("Output", "2022-Scraping", "by_publisher", paste0("copernicus-publishing-", Sys.Date(), ".tsv")))
