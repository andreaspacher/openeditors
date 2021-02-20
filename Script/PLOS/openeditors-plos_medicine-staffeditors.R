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

roles <- rvest::html_node(peoples_all, css = "em")
roles <- rvest::html_text(roles)

EdB <- do.call(rbind, Map(data.frame, editor=peoples, role=roles, affiliation=NA))
EdB$journal <- "PLOS Medicine"
EdB$issn <- "1549-1277"
EdB$publisher <- "PLOS"
EdB$url <- journal_url
EdB$date <- Sys.Date()


#filePath <- "C:\\Users\\pac\\Downloads\\openeditors\\2020-12-14 scrape-journals\\"
filePath <- "C:\\Your\\Folder\\"
write.csv(EdB, file = paste0(filePath, Sys.Date(), "_PLOS_Medicine_StaffEditors.csv"))

rm(webpage, wholepage, peoples_all, peoples, roles, journal_url, EdB)
