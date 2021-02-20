####################################
# STAFF EDITORS OF PLOS BIOLOGY
####################################
journal_url <- "https://journals.plos.org/plosbiology/s/staff-editors"

wholepage <- xml2::read_html(url(journal_url))

webpage <- rvest::html_node(wholepage, css = "body > main > div > article")

peoples_all <- rvest::html_nodes(webpage, css = "table")

peoples <- rvest::html_node(peoples_all, css = "h2")
peoples <- rvest::html_text(peoples)
peoples <- trimws(peoples)
peoples <- peoples[!is.na(peoples)]

peoples <- c(peoples, peoples2)

roles <- rvest::html_node(peoples_all, css = "em")
roles <- rvest::html_text(roles)

EdB <- do.call(rbind, Map(data.frame, editor = peoples, affiliation = NA, role = roles))
EdB$journal <- "PLOS Biology"
EdB$issn <- "1545-7885"
EdB$publisher <- "PLOS"
EdB$url <- journal_url
EdB$date <- Sys.Date()

filePath <- "C:\\Your\\Folder\\"
write.csv(EdB, file = paste0(filePath, Sys.Date(), "_PLOS_Biology_StaffEditors.csv"))