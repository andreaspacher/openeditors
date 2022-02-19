library(tidyverse)
library(rvest)

journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "Elsevier") %>%
  distinct()

journals$url <- paste0(journals$url, "/editorial-board")

# prepare the scraping process
EdList <- list()

for(i in 1:nrow(journals)) {
  
  printtext <- paste(i, journals$url[i], sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(xml2::read_html(url(journals$url[i])), silent = TRUE)
  
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  issn <- rvest::html_node(wholepage, "main > div.sc-orwwe2-1.dZFiMY > div > p")
  issn <- rvest::html_text(issn)
  issn <- stringr::str_extract(issn, "(?<=: ).*")
  
  journalname <- rvest::html_node(wholepage, "main > header > p.sc-orwwe2-5")
  journalname <- rvest::html_text(journalname)
  
  webpage <- rvest::html_nodes(wholepage, "div.sc-1k8yoys-3")
  
  editorsnodes <- rvest::html_children(webpage)
  
  titles <- rvest::html_nodes(editorsnodes, ".sc-1mur6on-4.ftpctw")
  titles <- rvest::html_text(titles)
  
  numcount <- stringr::str_count(editorsnodes, "biiCZY")
  titles <- rep(titles, numcount)
  
  editors <- rvest::html_nodes(editorsnodes, "ul li div.hWbBxq")
  names <- lapply(editors, function(x) {
    names <- rvest::html_node(x, "p.biiCZY")
    names <- rvest::html_text(names)
    names <- trimws(names)
  })
  names <- unlist(names)
  
  affiliations <- lapply(editors, function(x) {
    affiliations <- rvest::html_node(x, ".fziRAl")
    affiliations <- rvest::html_text(affiliations)
    affiliations <- trimws(affiliations)
  })
  affiliations <- unlist(affiliations)
  
  EdB <- data.frame("role" = titles,
                    "editor" = names,
                    "affiliation" = affiliations,
                    stringsAsFactors = FALSE)
  EdB$journal <- journalname
  EdB$issn <- issn
  EdB$publisher <- "Elsevier"
  EdB$url <- journals$url[i]
  EdB$date <- Sys.Date()
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, issn, role, editor, affiliation, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\Elsevier-", Sys.Date(), ".tsv"))
