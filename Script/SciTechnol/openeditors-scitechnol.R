df <- read.csv("Data\\SciTechnol\\scitechnol.csv")

# modify URL
df$url <- sub("scitechnol.com/", "scitechnol.com/editorialboard-", df$url)

# set scraping function
get_editors <- function(journal_url) {
  if (grepl("scitechnol.com", journal_url)) {

    wholepage <- xml2::read_html(url(journal_url))

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
      "url" = journal_url,
      "date" = Sys.Date()
    )


    return(EdB)
    Sys.sleep(8)
  }
}

# execute scraping
EdList <- list()

for(i in 92:nrow(df)) {
  
  printtext <- paste(i, df$url[i], sep=": ")
  print(printtext)
  
  EdList[[i]] <- get_editors(df$url[i])
  
  Sys.sleep(2)
}
library(tidyverse)
DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, role, editor, affiliation, url, date) %>%
  distinct()

df2 <- df[grep("scitechnol.com", df$url),]
df2 <- anti_join(df,df2)
df2$url <- paste0("https://www.scitechnol.com/editorialboard-", df2$url)

EdList2 <- list()

for(i in 1:nrow(df2)) {
  
  printtext <- paste(i, df$url[i], sep=": ")
  print(printtext)
  
  EdList2[[i]] <- get_editors(df$url[i])
  
  Sys.sleep(2)
}
library(tidyverse)
DF2 <- dplyr::bind_rows(EdList2) %>%
  select(publisher, journal, role, editor, affiliation, url, date) %>%
  distinct()

DF3 <- rbind(DF, DF2)

write.csv(DF3, file = paste0("Output\\SciTechnol\\SciTechnol-",i,".csv"), fileEncoding = "UTF-8")
