library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "Frontiers") %>%
  distinct()

journals$url <- paste0(journals$url, "/editors")

# prepare the scraping processW
EdList <- list()

for(i in 1:nrow(journals)) {
  
  editors_url <- journals$url[i]
  
  printtext <- paste(i, editors_url, sep=": ")
  print(printtext)
  
  rD <- RSelenium::rsDriver(browser="firefox", port=4546L, verbose=F)
  
  remDr <- rD[["client"]]
  remDr$maxWindowSize()
  remDr$navigate(editors_url)
  
  element <- remDr$findElement("css", "body")
  flag <- TRUE
  counter <- 0
  n <- 6
  while(flag){
    counter <- counter + 1
    #compare the pagesource every n(n=5) time, since sometimes one scroll down doesn't render new content
    for(ii in 1:n){
      
      element$sendKeysToElement(list("key"="end"))
      Sys.sleep(7.5)
    }
    if(exists("pagesource")){
      if(pagesource == remDr$getPageSource()[[1]]){
        flag <- FALSE
        writeLines(paste0("Scrolled down ",n*counter," times.\n"))
      } else {
        pagesource <- remDr$getPageSource()[[1]]
      }
    } else {
      pagesource <- remDr$getPageSource()[[1]]
    }
  }
  
  wholepage <- remDr$getPageSource()
  wholepage <- xml2::read_html(wholepage[[1]])
  
  journalname <- rvest::html_nodes(wholepage, css='h1')
  journalname <- rvest::html_text(journalname)
  journalname <- trimws(journalname)
  journalname <- journalname[[1]]
  journalname <- paste0("Frontiers in ", journalname)
  
  issn <- rvest::html_node(wholepage, xpath = "//*[@id=\"facts\"]/div/div[2]/div[2]/p[1]/text()")
  issn <- rvest::html_text(issn)
  issn <- trimws(issn)
  
  webpage <- rvest::html_nodes(wholepage, css = '.tab-editorial-board, editors')
  
  roles <- rvest::html_nodes(webpage, css="h4.teaser-role")
  roles <- rvest::html_text(roles)
  roles <- trimws(roles)
  
  peoples_all <- rvest::html_nodes(webpage, css = "div.teaser")
  
  peoples <- rvest::html_nodes(peoples_all, css = "a.editorial-board-full-name")
  peoples <- rvest::html_text(peoples)
  peoples <- trimws(peoples)
  peoples <- unlist(lapply(peoples, function(x) x[nzchar(x)]))
  
  affiliations <- rvest::html_nodes(peoples_all, css = "p.teaser-text")
  affiliations <- rvest::html_text(affiliations)
  affiliations <- trimws(affiliations)
  affiliations <- gsub("[\r\n]", ";", affiliations)
  affiliations <- gsub("\\s+", " ", affiliations)
  affiliations <- gsub("; ; ", "; ", affiliations)
  affiliations <- unlist(lapply(affiliations, function(x) x[nzchar(x)]))
  
  roles <- rvest::html_nodes(peoples_all, css = "p.role-value")
  roles <- rvest::html_node(roles, css = "strong")
  roles <- rvest::html_text(roles)
  roles <- trimws(roles)
  
  EdB <- try(
    do.call(rbind, Map(data.frame, editor=peoples, affiliation=affiliations, role=roles))
    , silent = TRUE)
  
  # for all Frontiers journals
  # that have a different format
  # (the so called "Frontiers Partnerships"?)
  if (inherits(EdB, "try-error")) {
    print("--- error?")
    
    clickthis <- try(remDr$findElement(using = "link text", "Editors and Reviewers"), silent = TRUE)
    if (inherits(clickthis, "try-error")) {
      clickthis <- try(remDr$findElement(using = "link text", "Editors"), silent = TRUE)
    }
    if (inherits(clickthis, "try-error")) {
      clickthis <- try(remDr$findElement(using = "link text", "Editors & Reviewers"), silent = TRUE)
    }
    if (inherits(clickthis, "try-error")) {
      clickthis <- try(remDr$findElement(using = "link text", "Editorial Board"), silent = TRUE)
    }
    clickthis$clickElement()
    
    Sys.sleep(10)
    
    element <- remDr$findElement("css", "div.Editors")
    flag <- TRUE
    counter <- 0
    n <- 6
    while(flag){
      counter <- counter + 1
      #compare the pagesource every n(n=5) time, since sometimes one scroll down doesn't render new content
      for(ii in 1:n){
        
        element$sendKeysToElement(list("key"="end"))
        Sys.sleep(6)
      }
      if(exists("pagesource")){
        if(pagesource == remDr$getPageSource()[[1]]){
          flag <- FALSE
          writeLines(paste0("Scrolled down ",n*counter," times.\n"))
        } else {
          pagesource <- remDr$getPageSource()[[1]]
        }
      } else {
        pagesource <- remDr$getPageSource()[[1]]
      }
    }
    
    wholepage <- remDr$getPageSource()
    wholepage <- xml2::read_html(wholepage[[1]])
    
    journalname <- rvest::html_nodes(wholepage, css='h1')
    journalname <- rvest::html_text(journalname)
    journalname <- trimws(journalname)
    journalname <- journalname[[1]]
    
    issn <- NA
    webpage <- rvest::html_nodes(wholepage, css = 'div.Editors')
    roles <- rvest::html_nodes(webpage, css="h3.Editors__role")
    roles <- rvest::html_text(roles)
    roles <- trimws(roles)
    
    peoples_all <- rvest::html_nodes(webpage, css = "editor-viewer")
    
    peoples <- rvest::html_nodes(peoples_all, css = "a.EditorsViewer__item__name")
    peoples <- rvest::html_text(peoples)
    peoples <- trimws(peoples)
    peoples <- unlist(lapply(peoples, function(x) x[nzchar(x)]))
    
    affiliations <- rvest::html_nodes(peoples_all, css = "div.EditorsViewer__item__affiliation")
    affiliations <- rvest::html_text(affiliations)
    affiliations <- trimws(affiliations)
    affiliations <- gsub("[\r\n]", ";", affiliations)
    affiliations <- gsub("\\s+", " ", affiliations)
    affiliations <- gsub("; ; ", "; ", affiliations)
    affiliations <- unlist(lapply(affiliations, function(x) x[nzchar(x)]))
    
    country <- rvest::html_nodes(peoples_all, css = "div.EditorsViewer__item__country")
    country <- rvest::html_text(country)
    country <- trimws(country)
    country <- gsub("[\r\n]", ";", country)
    country <- gsub("\\s+", " ", country)
    country <- gsub("; ; ", "; ", country)
    country <- unlist(lapply(country, function(x) x[nzchar(x)]))
    
    affiliations <- paste0(affiliations, ", ", country)
    
    roles <- rvest::html_nodes(peoples_all, css = "div.EditorsViewer__item__role")
    roles <- rvest::html_text(roles)
    roles <- trimws(roles)
    
    EdB <- try(
      do.call(rbind, Map(data.frame, editor=peoples, affiliation=affiliations, role=roles))
      , silent = TRUE)
    
    EdB$journal <- journalname
    EdB$issn <- issn
    EdB$publisher <- "Frontiers"
    EdB$url <- remDr$getCurrentUrl()
    EdB$date <- Sys.Date()
    
    remDr$close()
    gc()
    rD$server$stop()
    system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
    
    EdList[[i]] <- EdB
    print(paste0("--- found ", nrow(EdB), " editors!"))
    
    next
  }
  
  EdB$journal <- journalname
  EdB$issn <- issn
  EdB$publisher <- "Frontiers"
  EdB$url <- editors_url
  EdB$date <- Sys.Date()
  
  remDr$close()
  gc()
  rD$server$stop()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\frontiers-", Sys.Date(), ".tsv"))
