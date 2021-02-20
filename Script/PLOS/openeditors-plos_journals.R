######################################################################
#
# EDITORIAL BOARD OF PLOS JOURNALS
#
######################################################################

# pick one of these:
journal_url <- "https://journals.plos.org/plospathogens/s/editorial-board"
journal_url <- "https://journals.plos.org/plosntds/s/editorial-board"
journal_url <- "https://journals.plos.org/plosgenetics/s/editorial-board"
journal_url <- "https://journals.plos.org/plosmedicine/s/editorial-board"
journal_url <- "https://journals.plos.org/ploscompbiol/s/editorial-board"
journal_url <- "https://journals.plos.org/plosbiology/s/editorial-board"

# webscraping starts here
wholepage <- xml2::read_html(url(journal_url))

journalname <- rvest::html_node(wholepage, "title")
journalname <- rvest::html_text(journalname)
journalname <- stringr::str_extract(journalname, "^.*(?=: )")

webpage <- rvest::html_node(wholepage, css = "body > main > div > article")

peoples_all <- stringr::str_extract_all(webpage, "(?<=<h4.{0,30}?>)[\\S\\s]*?(?=</p>)")
peoples <- lapply(peoples_all, function(x) stringr::str_extract(x, "^.*?(?=</h4>)"))

affiliations <- sapply(peoples_all, function(x) stringr::str_extract_all(x, "(?<=</h4>)[\\S\\s]*?(?=(</p>|$))"))
affiliations <- lapply(affiliations, function(x) gsub("<.*?>", "", x))
affiliations <- unlist(affiliations)
affiliations <- gsub("orcid\\.org/.{19}", "", affiliations)
affiliations <- gsub("Expertise:.*$", "", affiliations)
affiliations <- gsub("Sections:.*$", "", affiliations)
affiliations <- trimws(affiliations)

EdB <- do.call(rbind, Map(data.frame, editor = peoples, role = "Editorial Board"))
EdB$affiliation <- affiliations
EdB$journal <- journalname
if (journalname == "PLOS Biology") {
  EdB$issn <- "1544-9173"
} else if (journalname == "PLOS Medicine") {
  EdB$issn <- "1549-1277"
} else if (journalname == "PLOS Computational Biology") {
  EdB$issn <- "1553-7358"
} else if (journalname == "PLOS Genetics") {
  EdB$issn <- "1553-7390"
} else if (journalname == "PLOS Neglected Tropical Diseases") {
  EdB$issn <- "1935-2727"
} else if (journalname == "PLOS Pathogens") {
  EdB$issn <- "1553-7366"
} else {
  EdB$issn <- NA
}
EdB$publisher <- "PLOS"
EdB$url <- journal_url
EdB$date <- Sys.Date()

EdB$editor <- gsub("<.*?>", "", EdB$editor)

filePath <- "C:\\Your\\Folder\\"
write.csv(EdB, file = paste0(filePath, Sys.Date(), "_", journalname, "_EditorialBoard.csv"))
