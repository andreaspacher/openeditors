library(tidyverse)
library(rvest)

# regex-function to clean strings from html codes
clean_html <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# get journals
journals <- read.csv(url("https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true")) %>%
  filter(publisher == "Springer" | publisher == "Springer Nature" | publisher == "BioMedCentral") %>%
  distinct() %>%
  select(journal, url)

# get links to editorial boards
JJ <- list.files(path = "Output\\2022-Springer-Links", pattern="springer.*.csv")
JJ <- lapply(paste0("Output\\2022-Springer-Links\\", JJ), read_csv)
JJ <- data.table::rbindlist(JJ)
JJ <- JJ %>%
  filter(!is.na(editors_url) & editors_url != "https://www.springer.com/gp/authors-editors" & editors_url != "/authors-editors")
JJ <- JJ %>%
  filter(grepl("Editorial ((B|b)oard|(S|s)taff)|About the (E|e)ditor", editors_linktext)
         & !is.na(editors_linktext))

# merge the files together
journals <- left_join(JJ, journals)
journals <- journals %>%
  mutate(url = ifelse(grepl("nature", url), "https://www.nature.com", url),
         editors_url = ifelse(editors_url == "/about/editorial-board" | grepl("nature|biomedc", url), paste0(url, editors_url),
                              paste0("https://www.springer.com", editors_url)),
         editors_url = stringr::str_extract(editors_url, "http(?![\\s\\S]*http).*")) %>%
  filter(!grepl("volumesAndIssues", editors_url)
         & editors_url != "https://www.springer.com/about-the-editors"
         & editors_url != "https://bmcserieseditors.biomedcentral.com/"
         & !grepl("javascript:|authors/editorial_policies", editors_url)
         & !grepl("Join|recruiting|Call for|Introducing", editors_linktext)) %>%
  distinct() %>%
  select(-url) %>%
  select(journal, "url" = editors_url, editors_linktext) %>%
  distinct()

# only select BMC journals
journals <- journals %>%
  filter(grepl("biomedc", journals$url) & !grepl("for-editorial-board", journals$url))

journals %>%
  select(journal) %>%
  distinct() %>% count()

# prepare the scraping process
EdList <- list()

for(i in 1:nrow(journals)) {
  
  printtext <- paste(i, journals$url[i], sep=": ")
  print(printtext)
  
  # start scraping
  wholepage <- try(xml2::read_html(journals$url[i]), silent = TRUE)
  
  # did it work?
  if (inherits(wholepage, "try-error")) {
    print("--- 404 error?")
    next
  }
  
  webpage <- html_node(wholepage, "main")
  xmltext <- trimws(webpage)
  
  #=====================
  # (1) parse roles
  #=====================
  roles <- html_nodes(webpage, "strong") %>%
    html_text()
  
  #=====================
  # (1b) count nr of editors per role
  #=====================
  groups <- strsplit(
    xmltext
    , '(<strong>)[^<]{8,}(?=</strong>|</div>|</p>)'
    , perl=TRUE
  )
  groups <- lapply(groups, function(x){x[!x == ""]})   # remove empty string (that always occurs due to strsplit)
  groups <- lapply(groups, function(x){x[grepl(",", x)]})   # remove empty string (that always occurs due to strsplit)
  
  italicnumbers <- unlist(lapply(groups,
                                 function(x)
                                   str_count(x, '(?<=<i>|<em>)[\\s\\S]+?(?=</i>|</em>)')
  )
  )
  italicnumbers <- italicnumbers[italicnumbers != 0]
  roles <- try(rep(roles, times=italicnumbers),
               silent = TRUE)
  if (inherits(roles, "try-error")) {
    roles <- NA
  }
  
  #=====================
  # (2) parse editors
  #=====================
  editors <- unlist(stringr::str_extract_all(webpage, '(?<=<br>|<p>)(?:(?!<strong>)[\\s\\S])*?(?=<br>|</p>)'))
  editors <- editors[nchar(editors) > 6]
  
  #=====================
  # (3) parse affiliations
  #=====================
  affiliations <- stringr::str_extract_all(editors, '(?<=<i>|<em>)(?:(?!</i>).)+?(?=</i>|</em>)')
  affiliations <- stringr::str_squish(affiliations)
  affiliations <- affiliations[nchar(affiliations) > 8]
  
  editors <- stringr::str_extract(editors, ".*?(?=(<em>|<i>))")
  
  #=====================
  # (4) remove empty parts from list & clean data
  #=====================
  roles <- roles[!sapply(roles, identical, character(0))]
  editors <- editors[!sapply(editors, identical, character(0))]
  affiliations <- affiliations[!sapply(affiliations, identical, character(0))]
  
  roles <- trimws(clean_html(roles))
  editors <- trimws(clean_html(editors))
  affiliations <- trimws(clean_html(affiliations))
  
  editors <- gsub(",$", "", editors)
  
  #=====================
  # (5) CHECK
  #=====================
  # they should be of equal length:
  len1 <- length(affiliations)
  len2 <- length(editors)
  len3 <- length(roles)
  
  if(is_empty(roles)) { roles <- NA }
  if(len1 == len2 & len3 != len1) {
    roles <- NA
  }
  if((is.na(roles) & len1 == len2 & len1 != 0) | all(sapply(list(len1,len2,len3), function(x) x == len3) & len3 != 0)) {
    EdB <- do.call(rbind, c(Map(data.frame,
                                editors = editors,
                                affiliations = affiliations,
                                roles = roles,
                                publisher = "BioMedCentral",
                                journal = journals$journal[i],
                                url = journals$url[i]
    ), make.row.names = FALSE))
    EdB$date <- Sys.Date()
  } else {
    print("-- some error")
    Sys.sleep(2.5)
    next
  }
  
  EdList[[i]] <- EdB
  print(paste0("--- found ", nrow(EdB), " editors!"))
  
  Sys.sleep(3)
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, "role" = roles, "editor" = editors, "affiliation" = affiliations, url, date)

write_tsv(DF, paste0("Output\\2022-Scraping\\BioMedCentral-", Sys.Date(), ".tsv"))
