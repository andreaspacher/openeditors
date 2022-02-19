EdList <- list()
journal_url <- "https://elifesciences.org/about/people"

eCaps <- list(`mox:firefoxOptions` = list(general.useragent.override = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:84.0) Gecko/20100101 Firefox/84.0"))
rD <- RSelenium::rsDriver(
  browser = "firefox", port = 4546L, verbose = F,
  extraCapabilities = eCaps
)

remDr <- rD[["client"]]
remDr$navigate(journal_url)

element <- remDr$findElement("css", "body")
element$sendKeysToElement(list("key"="end"))

wholepage <- remDr$getPageSource()
wholepage <- xml2::read_html(wholepage[[1]])

alloptions <- rvest::html_nodes(wholepage, "div.form-item select option")
alloptions <- rvest::html_attr(alloptions, "value")

for(i in 1:length(alloptions)) {
  
  element <- remDr$findElement("css", "body")
  element$sendKeysToElement(list("key"="end"))
  
  wholepage <- remDr$getPageSource()
  wholepage <- xml2::read_html(wholepage[[1]])
  
  webpage <- rvest::html_nodes(wholepage, css = '#maincontent > div.wrapper.wrapper--listing > div > div')
  
  roles <- rvest::html_nodes(webpage, "h3.list-heading")
  roles <- rvest::html_text(roles)
  
  peoples_all <- rvest::html_nodes(webpage, "ol.about-profiles")
  
  #peoples_all <- rvest::html_nodes(webpage, css = "li.about-profiles__item")
  
  peoples <- lapply(peoples_all, function(x) {
    x <- rvest::html_nodes(x, css = "h4")
    x <- rvest::html_text(x)
    x <- stringr::str_squish(x)
  })
  # 
  # peoples <- rvest::html_nodes(peoples_all, css = "h4")
  # peoples <- rvest::html_text(peoples)
  # peoples <- stringr::str_squish(peoples)
  
  affiliations <- lapply(peoples_all, function(x) {
    x <- rvest::html_nodes(x, "p.about-profile__role")
    x <- rvest::html_text(x)
    x <- stringr::str_squish(x)
  })
  
  names(peoples) <- roles
  
  EdB <- stack(peoples)
  names(EdB) <- c("editor", "role")
  EdB$affiliation <- unlist(affiliations)
  EdB$journal <- "eLife"
  EdB$issn <- "2050-084X"
  EdB$publisher <- "eLife"
  EdB$url <- journal_url
  EdB$date <- Sys.Date()
  EdB$orcid <- NA
  
  EdList[[i]] <- EdB
  
  Sys.sleep(4)
  
  next_page<-remDr$findElement(using='xpath', paste0("//div[contains(@class, 'form-item')]/select/option[", i+1, "]"))
  next_page$clickElement()
  
}

remDr$close()
gc()
rD$server$stop()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

library(tidyverse)
DF <- dplyr::bind_rows(EdList) %>%
  dplyr::select(publisher, issn, journal, role, editor, affiliation, orcid, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\elife-", Sys.Date(), ".tsv"))
