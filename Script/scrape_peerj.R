EdList <- list()
journal_url <- "https://peerj.com/academic-boards/editors/peerj/"

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

lastpage <- 140

for(i in 3:lastpage) {
  
  element <- remDr$findElement("css", "body")
  element$sendKeysToElement(list("key"="end"))
  
  wholepage <- remDr$getPageSource()
  wholepage <- xml2::read_html(wholepage[[1]])
  
  webpage <- rvest::html_nodes(wholepage, css = '.container > div.row > div.span9')
  
  peoples_all <- rvest::html_nodes(webpage, css = "div.span5")
  
  peoples <- rvest::html_node(peoples_all, css = "h3 a")
  peoples <- rvest::html_text(peoples)
  peoples <- stringr::str_squish(peoples)
  
  affiliations <- rvest::html_nodes(peoples_all, "div")
  affiliations <- rvest::html_text(affiliations)
  affiliations <- stringr::str_squish(affiliations)
  
  EdB <- do.call(rbind, Map(data.frame, editor=peoples, affiliation=affiliations, role="Academic Editors"))
  EdB$journal <- "PeerJ"
  EdB$issn <- "2167-8359"
  EdB$publisher <- "PeerJ"
  EdB$url <- journal_url
  EdB$date <- Sys.Date()
  EdB$orcid <- NA
  
  EdList[[i]] <- EdB
  
  Sys.sleep(4)
  
  next_page<-remDr$findElement(using='xpath', "//a[contains(.,'Next')]")
  next_page$clickElement()
  
}

remDr$close()
gc()
rD$server$stop()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

library(tidyverse)
DF <- dplyr::bind_rows(EdList) %>%
  dplyr::select(publisher, issn, journal, role, editor, affiliation, orcid, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\peerj-", Sys.Date(), ".tsv"))
