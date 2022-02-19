library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "SCIRP") %>%
  distinct()

journals$url <- stringr::str_remove(journals$url, ".html$")

EdList <- list()

for(i in 1:nrow(journals)) {
  
  printtext <- paste(i, journals$url[i], sep=": ")
  print(printtext)
  
  rD <- RSelenium::rsDriver(browser="firefox", port=4546L, verbose=F,
                            extraCapabilities=list(acceptInsecureCerts=TRUE,acceptUntrustedCerts=TRUE)) # <- add this on your own risk!
  remDr <- rD[["client"]]
  remDr$maxWindowSize()
  remDr$navigate(journals$url[i])
  
  wholepage <- remDr$getPageSource()
  wholepage <- xml2::read_html(wholepage[[1]])
  
  Sys.sleep(1.5)

  frameurl <- rvest::html_node(wholepage, "frame")
  frameurl <- rvest::html_attr(frameurl, "src")
  frameurl <- paste0("http:", frameurl)

  remDr$navigate(frameurl)
  Sys.sleep(1.5)
  
  clickthis <- try(remDr$findElement(using = "link text", "Editorial Board"), silent = TRUE)
  
  clickthis$clickElement()
  
  Sys.sleep(3)
  
  wholepage <- remDr$getPageSource()
  wholepage <- xml2::read_html(wholepage[[1]])

    # wholepage <- rvest::session(
  #   journals$url[i],
  #   httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  # )
  
  journalname <- rvest::html_node(wholepage, css = "h4 strong")
  journalname <- rvest::html_text(journalname)
  if (grepl("Open Access$", journalname)) {
    journalname <- stringr::str_remove(journalname, "Open Access")
    journalname <- trimws(journalname)
  }
  
  webpage <- rvest::html_nodes(wholepage, css = "#form1 > div.container > div > div.col-md-6.column > div:nth-child(3) > div table")
  
  issn <- rvest::html_node(wholepage, "#UserControl_Journalmag1_p_issn")
  issn <- rvest::html_text(issn)
  issn <- stringr::str_remove(issn, ".*: ")
  
  people <- lapply(webpage, function(x) {
    ppl <- rvest::html_nodes(x, "strong")
    ppl <- rvest::html_text(ppl)
    ppl
  }
  )
  
  roles <- rvest::html_nodes(wholepage, css = "#form1 > div.container > div > div.col-md-6.column > div:nth-child(3) > div h4")
  roles <- rvest::html_text(roles)
  names(people) <- roles

  affiliations <- rvest::html_nodes(webpage, "td")
  affiliations <- rvest::html_text(affiliations)
  affiliations <- trimws(affiliations)
  affiliations <- stringr::str_squish(affiliations)
  affiliations <- affiliations[c(FALSE, TRUE)] # keep only the even indices
  affiliations <- sub("^.*?,", "", affiliations)
  affiliations <- trimws(affiliations)
  
  EdB <- stack(people)
  names(EdB) <- c("editor", "role")
  EdB$affiliation <- affiliations
  EdB$journal <- journalname
  EdB$publisher <- "SCIRP"
  EdB$issn <- issn
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  remDr$close()
  gc()
  rD$server$stop()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  
  Sys.sleep(5)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\SCIRP-", Sys.Date(), ".tsv"))
