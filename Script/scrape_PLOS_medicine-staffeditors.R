######
# STAFF EDITORS
######
journal_url <- "https://journals.plos.org/plosmedicine/s/staff-editors"

wholepage <- xml2::read_html(url(journal_url))

webpage <- rvest::html_node(wholepage, css = 'body > main > div > article')

peoples_all <- rvest::html_nodes(webpage, css = "table")

peoples <- rvest::html_node(peoples_all, css = "h2")
peoples <- rvest::html_text(peoples)
peoples <- trimws(peoples)
peoples <- peoples[!is.na(peoples)]

# add orcid
orcid <- rvest::html_nodes(peoples_all, xpath = "//a[contains(@href, 'orcid')]")
orcid <- rvest::html_text(orcid)
orcid <- unique(orcid)
orcid <- orcid[orcid != ""]
orcid <- stringr::str_extract(orcid, "([^\\/]+$)")

roles <- rvest::html_node(peoples_all, css = "em")
roles <- rvest::html_text(roles)

EdB <- do.call(rbind, Map(data.frame, editor=peoples, orcid=orcid, role=roles, affiliation=NA))
EdB$journal <- "PLOS Medicine"
EdB$issn <- "1549-1277"
EdB$publisher <- "PLOS"
EdB$url <- journal_url
EdB$date <- Sys.Date()

write.csv(EdB, paste0("Output\\2022-Scraping\\plos-medicine-staff-editors-", Sys.Date(), ".csv"))

rm(webpage, wholepage, peoples_all, peoples, roles, journal_url, EdB)
