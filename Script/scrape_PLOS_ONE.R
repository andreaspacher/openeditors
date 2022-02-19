# this script scrapes
# the editorial board members
# of PLOS ONE
# by clicking on the "next page" button until the end
# and scraping the editors (50 each page)
#
# note that PLOS ONE also has two other types of editors
# (1) staff editors
# (2) section editors

PLOS_Editors <- list()
journal_url <- "https://journals.plos.org/plosone/static/editorial-board"

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

lastpage <- rvest::html_node(wholepage, css = "#article-pagination > a:nth-child(8)")
lastpage <- rvest::html_attr(lastpage, "data-page")

for(i in 1:lastpage) {
  
    element <- remDr$findElement("css", "body")
    element$sendKeysToElement(list("key"="end"))
    
    wholepage <- remDr$getPageSource()
    wholepage <- xml2::read_html(wholepage[[1]])
    
    webpage <- rvest::html_nodes(wholepage, css = '#content > div.results > ul')
    
    peoples_all <- rvest::html_nodes(webpage, css = "li.item")
    
    peoples <- rvest::html_node(peoples_all, css = "strong")
    peoples <- rvest::html_text(peoples)
    peoples <- trimws(peoples)
    
    # orcid
    orcid <- sapply(peoples_all, function(x) {
      o <- rvest::html_node(x, "a")
      o <- rvest::html_text(o)
      o <- stringr::str_extract(o, "([^\\/]+$)")
    })
    
    peoples_string <- toString(peoples_all)
    peoples_string <- unlist(stringr::str_extract_all(peoples_string, "(?<=<li class=\\\"item\\\">)[\\S\\s]*?(?=</li>)"))
    
    affiliations <- stringr::str_extract(peoples_string, "(?<=(</strong><br>))[\\S\\s]*?(?=<br><b>)")
    affiliations <- unlist(lapply(affiliations, function(x) stringr::str_remove(x, "^.*?(!?</a></span><br>)")))
    affiliations <- gsub("[\r\n]", "", affiliations)
    affiliations <- trimws(affiliations)
    affiliations <- gsub("<br>          ", ", ", affiliations)
    
    EdB <- do.call(rbind, Map(data.frame, editor=peoples, affiliation=affiliations, orcid=orcid, role="Editorial Board"))
    EdB$journal <- "PLOS ONE"
    EdB$issn <- "1932-6203"
    EdB$publisher <- "PLOS"
    EdB$url <- journal_url
    EdB$date <- Sys.Date()
 
    PLOS_Editors[[i]] <- EdB
    
    Sys.sleep(4)
    
    
    next_page<-remDr$findElement(using='css selector', '#nextPageLink')
    next_page$clickElement()
    
}

remDr$close()
gc()
rD$server$stop()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

library(tidyverse)
DF <- dplyr::bind_rows(PLOS_Editors) %>%
  dplyr::select(publisher, issn, journal, role, editor, affiliation, orcid, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\plos-one-", Sys.Date(), ".tsv"))
