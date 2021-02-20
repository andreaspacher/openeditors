#' Webscrape Editors
#'
#' This function allows you to webscrape the editors of scholarly journals from the publishers Cambridge University Press, Elsevier and SAGE.
#' @param journal_url Link to the editorial board-webpage
#' @export
#' @examples
#' url <- https://www.cambridge.org/core/journals/modern-asian-studies/information/editorial-board
#' get_editors(url)
#' get_editors("https://www.journals.elsevier.com/electoral-studies/editorial-board")
#' get_editors("https://journals.sagepub.com/editorial-board/asr")
get_editors <- function(journal_url) {
    # =============== FRONTIERS =============== [WORKS PERFECTLY]
    if(grepl("frontiersin.org", journal_url)) {
    
    #wholepage <- rvest::html_session(journal_url,
    #                                 httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20"))
    #wholepage <- xml2::read_html(url(journal_url))
    
    # connect with RSelenium
    # library("RSelenium")
    
    #AT HOME
    
    eCaps <- list(chromeOptions = list(
      args = list('--user-agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko)"')
    ))
    
    rD <- RSelenium::rsDriver(browser="chrome", port=4546L, verbose=F, chromever="87.0.4280.20",
                              extraCapabilities = eCaps)
    
    #AT WORK
    #rD <- RSelenium::rsDriver(browser="chrome", port=4546L, verbose=F)
    remDr <- rD[["client"]]
    remDr$navigate(journal_url)
    
    element <- remDr$findElement("css", "body")
    flag <- TRUE
    counter <- 0
    n <- 7
    while(flag){
      counter <- counter + 1
      #compare the pagesource every n(n=5) time, since sometimes one scroll down doesn't render new content
      for(i in 1:n){
        element$sendKeysToElement(list("key"="end"))
        Sys.sleep(8)
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
    
    EdB <- do.call(rbind, Map(data.frame, editor=peoples, affiliation=affiliations, role=roles))
    EdB$journal <- journalname
    EdB$issn <- issn
    EdB$publisher <- "Frontiers Media"
    EdB$url <- journal_url
    EdB$date <- Sys.Date()
    
    remDr$close()
    gc()
    rD$server$stop()
    system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
    
    Sys.sleep(2)

    return(EdB)
    
    # =============== ELSE ===============
  } else {
    stop("Make sure to use a journal from either Cambridge University Press, Elsevier or SAGE. Use the whole URL to the website containing editorial board data.")
  }
}
