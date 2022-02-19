library(tidyverse)
library(rvest)

# get all MDPI journals
journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true"))
journals <- journals %>%
  filter(publisher == "MDPI") %>%
  distinct()
journals <- journals %>%
  mutate(url = paste0(url, "/editors"))

# prepare the scraping process
EdList <- list()

for(i in 1:nrow(journals)) {
  
  eCaps <- list(`mox:firefoxOptions` = list(general.useragent.override = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:84.0) Gecko/20100101 Firefox/84.0"))
  rD <- RSelenium::rsDriver(
    browser = "firefox", port = 4546L, verbose = F,
    extraCapabilities = eCaps
  )
  remDr <- rD[["client"]]
  remDr$navigate(journals$url[i])
  
  for (iii in 1:5) {
    remDr$executeScript(paste("scroll(0,", iii * 10000, ");"))
    Sys.sleep(3)
  }
  
  wholepage <- remDr$getPageSource()
  wholepage <- xml2::read_html(wholepage[[1]])
  
  journalname <- rvest::html_nodes(wholepage, css = "#js-main-top-container > a:nth-child(1) > img:nth-child(1)")
  journalname <- stringr::str_extract(journalname, "(?<= title=\").*?(?=\")")
  
  issn <- rvest::html_nodes(wholepage, css = ".journal-info > span:nth-child(1)")
  issn <- stringr::str_extract(issn, "(?<=EISSN ).*?(?=,)")
  
  webpage <- rvest::html_nodes(wholepage, css = ".middle-column__main")
  
  roles <- rvest::html_nodes(webpage, css = "h2")
  roles <- rvest::html_text(roles)
  rolesnumber <- unlist(lapply(roles, function(x) stringr::str_extract(x, "(?<=\\().*?(?=\\))")))
  rolesnumber <- unlist(lapply(rolesnumber, function(x) ifelse(is.na(x), 1, x)))
  roles <- unlist(lapply(roles, function(x) stringr::str_extract(x, "^(.|\\s)*?(?=( \\(|$))")))
  roles <- rep(roles, rolesnumber)
  
  peoples <- rvest::html_nodes(webpage, css = ".editor-div__content")
  
  people <- as.character(peoples[seq(1, length(peoples), 2)])
  people <- unlist(lapply(people, function(x) stringr::str_extract(x, "(?<=<b>).*?(?=</b>)")))
  
  affiliations <- as.character(peoples[seq(2, length(peoples), 2)])
  affiliations <- unlist(lapply(affiliations, function(x) stringr::str_extract(x, "(?<=>\\n).*?(?=<br>)")))
  
  EdB <- do.call(rbind, Map(data.frame, editor = people, affiliation = affiliations, role = roles))
  EdB$journal <- journalname
  EdB$issn <- issn
  EdB$publisher <- "MDPI"
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  remDr$close()
  gc()
  rD$server$stop()
  system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)
  
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  EdList[[i]] <- EdB
  
  Sys.sleep(7)
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\mdpi-", Sys.Date(), "b.tsv"))
