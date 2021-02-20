######
# STAFF EDITORS
######
journal_url <- "https://journals.plos.org/plosone/s/staff-editors"

wholepage <- xml2::read_html(url(journal_url))

webpage <- rvest::html_node(wholepage, css = 'body > main > div > article')

peoples_all <- rvest::html_nodes(webpage, css = "table")
  
peoples <- rvest::html_node(peoples_all, css = "h2")
peoples <- rvest::html_text(peoples)
peoples <- trimws(peoples)
peoples <- peoples[!is.na(peoples)]

peoples2 <- rvest::html_nodes(peoples_all, css = "h3")
peoples2 <- rvest::html_text(peoples2)
peoples2 <- trimws(peoples2)
peoples2 <- peoples2[!sapply(peoples2, identical, "")]

peoples <- c(peoples, peoples2)

roles <- rvest::html_node(peoples_all, css = "em")
roles <- rvest::html_text(roles)
roles <- roles[-25] #just for the scraping on 19 Dec 2020, b/c there seems to be a kind of error ...

EdB <- do.call(rbind, Map(data.frame, editor=peoples, affiliation=NA, role=roles))
EdB$journal <- "PLOS ONE"
EdB$issn <- "1932-6203"
EdB$publisher <- "PLOS"
EdB$url <- journal_url
EdB$date <- Sys.Date()


#filePath <- "C:\\Users\\pac\\Downloads\\openeditors\\2020-12-14 scrape-journals\\"
filePath <- "C:\\Users\\andre\\OneDrive\\2020 Grants\\2020 Fellowship Op Sci\\2020-12 SCRAPED-DATA\\"
write.csv(EdB, file=paste0(filePath, "2020-12-19_PLOS_StaffEditors.csv"))

rm(webpage, wholepage, peoples_all, peoples, roles, peoples2, journal_url)


######
# SECTION EDITORS
######
journal_url <- "https://journals.plos.org/plosone/s/section-editors"

wholepage <- xml2::read_html(url(journal_url))
webpage <- rvest::html_node(wholepage, css = 'body > main > div > article')

peoples_all <- rvest::html_nodes(webpage, css = "tr")
peoples <- rvest::html_node(peoples_all, css = "h4")
peoples <- rvest::html_text(peoples)
peoples <- trimws(peoples)
peoples <- peoples[!is.na(peoples)]

affiliations <- rvest::html_node(peoples_all, css = "p")
affiliations <- rvest::html_text(affiliations)
affiliations <- unlist(lapply(affiliations, function(x) stringr::str_extract(x, "^.*?(?=(Expertise|$))")))
affiliations <- gsub("orcid.org/.{19,20}", "", affiliations)
affiliations <- trimws(affiliations)
affiliations <- affiliations[!is.na(affiliations)]

EdB <- do.call(rbind, Map(data.frame, editor=peoples, affiliation=affiliations, role="Section Editor"))
EdB$journal <- "PLOS ONE"
EdB$issn <- "1932-6203"
EdB$publisher <- "PLOS"
EdB$url <- journal_url
EdB$date <- Sys.Date()

#filePath <- "C:\\Users\\pac\\Downloads\\openeditors\\2020-12-14 scrape-journals\\"
filePath <- "C:\\Users\\andre\\OneDrive\\2020 Grants\\2020 Fellowship Op Sci\\2020-12 SCRAPED-DATA\\"
write.csv(EdB, file=paste0(filePath, "2020-12-19_PLOS_SectionEditors.csv"))

rm(webpage, wholepage, peoples_all, peoples, journal_url, affiliations)
