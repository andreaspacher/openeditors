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

# add orcid
orcid <- rvest::html_nodes(peoples_all, xpath = "//a[contains(@href, 'orcid')]")
orcid <- rvest::html_text(orcid)
orcid <- unique(orcid)
orcid <- orcid[orcid != ""]
orcid <- stringr::str_extract(orcid, "([^\\/]+$)")

roles <- rvest::html_node(peoples_all, css = "em")
roles <- rvest::html_text(roles)
#roles <- roles[-25] #just for the scraping on 19 Dec 2020, b/c there seems to be a kind of error ...

EdB <- do.call(rbind, Map(data.frame, editor=peoples, affiliation=NA, role=roles, orcid = orcid))
EdB$journal <- "PLOS ONE"
EdB$issn <- "1932-6203"
EdB$publisher <- "PLOS"
EdB$url <- journal_url
EdB$date <- Sys.Date()

write.csv(EdB, paste0("Output\\2022-Scraping\\plos-one-staff-editors-", Sys.Date(), ".csv"))

rm(webpage, wholepage, peoples_all, peoples, roles, peoples2, journal_url)


######
# SECTION EDITORS
######
journal_url <- "https://journals.plos.org/plosone/s/section-editors"

wholepage <- xml2::read_html(url(journal_url))
webpage <- rvest::html_node(wholepage, css = 'body > main > div > article')

peoples_all <- rvest::html_nodes(webpage, css = "tr")
peoples <- sapply(peoples_all, function(x) {
  p <- rvest::html_node(x, css = "h4")
  p <- rvest::html_text(p)
  p <- trimws(p)
})
orcid <- sapply(peoples_all, function(x) {
  o <- rvest::html_node(x, "a")
  o <- rvest::html_text(o)
  o <- stringr::str_extract(o, "([^\\/]+$)")
})

affiliations <- sapply(peoples_all, function(x) {
  a <- rvest::html_node(x, "p")
  a <- rvest::html_text(a)
  a <- stringr::str_extract(a, "^.*?(?=(Expertise|$))")
  a <- gsub("orcid.org/.{19,20}", "", a)
  a <- trimws(a)
})

EdB <- do.call(rbind, Map(data.frame, editor=peoples, affiliation=affiliations, orcid = orcid, role="Section Editor"))
EdB$journal <- "PLOS ONE"
EdB$issn <- "1932-6203"
EdB$publisher <- "PLOS"
EdB$url <- journal_url
EdB$date <- Sys.Date()

EdB <- EdB[!is.na(EdB$editor),]


write.csv(EdB, paste0("Output\\2022-Scraping\\plos-one-section-editors-", Sys.Date(), ".csv"))

rm(webpage, wholepage, peoples_all, peoples, journal_url, affiliations)
