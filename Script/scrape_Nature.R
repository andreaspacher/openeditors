library(tidyverse)
library(rvest)

# regex-function to clean strings from html codes
clean_html <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# get journals
journals <-
  read.csv(
    url(
      "https://github.com/andreaspacher/academic-publishers/blob/main/Output/allpublishers-PRELIMINARY-2021-12-09.csv?raw=true"
    )
  ) %>%
  filter(publisher == "Springer Nature") %>%
  distinct() %>%
  select(journal, url)

# get links to editorial boards
JJ <-
  list.files(path = "Output\\2022-Springer-Links", pattern = "springer.*.csv")
JJ <- lapply(paste0("Output\\2022-Springer-Links\\", JJ), read_csv)
JJ <- data.table::rbindlist(JJ)
JJ <- JJ %>%
  filter(
    !is.na(editors_url) &
      editors_url != "https://www.springer.com/gp/authors-editors" &
      editors_url != "/authors-editors"
  ) %>%
  filter(
    grepl(
      "Editorial ((B|b)oard|(S|s)taff)|About the (E|e)ditor",
      editors_linktext
    )
    & !is.na(editors_linktext)
  )

# merge the files together
journals <- left_join(journals, JJ)
journals <- journals %>%
  mutate(
    url = ifelse(grepl("nature", url), "https://www.nature.com", url),
    editors_url = ifelse(
      editors_url == "/about/editorial-board" |
        grepl("nature|biomedc", url),
      paste0(url, editors_url),
      paste0("https://www.springer.com", editors_url)
    ),
    editors_url = stringr::str_extract(editors_url, "http(?![\\s\\S]*http).*")
  ) %>%
  filter(
    !grepl("volumesAndIssues", editors_url)
    &
      editors_url != "https://www.springer.com/about-the-editors"
    &
      editors_url != "https://bmcserieseditors.biomedcentral.com/"
    &
      !grepl("javascript:|authors/editorial_policies", editors_url)
    &
      !grepl("Join|recruiting|Call for|Introducing", editors_linktext)
  ) %>%
  distinct() %>%
  select(-url) %>%
  select(journal, "url" = editors_url, editors_linktext) %>%
  distinct()

# only select Nature journals
journals <- journals %>%
  filter(!grepl("https://www.nature.comNA", journals$url))

journals <- journals %>%
  distinct(url, .keep_all = TRUE)

journals %>%
  select(journal) %>%
  distinct() %>% count()

# prepare the scraping process
EdList <- list()

for (i in 1:nrow(journals)) {
  ALL <- try(xml2::read_html(journals$url[i]), silent = TRUE)
  
  if (inherits(ALL, "try-error")) {
    print("SOME KIND OF ERROR?")
    next
  }
  
  if (grepl("editorial-board", journals$url[i])) {
    #
    #
    # N a t u r e -- E d i t o r i a l    B o a r d s
    #
    #
    
    printtext <- paste(i, journals$url[i], sep=": ")
    print(printtext)
    
    webpage <-
      html_node(ALL,
                "#content div.grid.grid-9.last.mq640-grid-12.mq640-last.mt30.mq640-mt0")
    if (length(webpage) == 0) {
      webpage <- html_node(ALL, "#content")
    }
    xmltext <- trimws(webpage)
    
    #=====================
    # (1) parse editors
    #=====================
    editors <- html_nodes(webpage, xpath = "//p/text()")
    editors <-
      unlist(stringr::str_extract_all(webpage, '(?<=<br>|<p>).*?(?=<br>|</p>)'))
    editors <- editors[nchar(editors) > 6 & nchar(editors) < 3000]
    
    #=====================
    # (2) parse affiliations
    #=====================
    affiliations <-
      stringr::str_extract_all(editors, '(?<=<i>|<em>)(?:(?!</i>).)+?(?=</i>|</em>)')
    affiliations <- stringr::str_squish(affiliations)
    affiliations <- affiliations[nchar(affiliations) > 8]
    if (length(affiliations) < length(editors) &
        length(affiliations) != 0) {
      affiliations <-
        stringr::str_extract_all(editors, '(?<=<i>|<em>)(?:(?!</i>).)+?(?=</i>|</em>)')
      affiliations <- stringr::str_squish(affiliations)
      affiliations <- affiliations[nchar(affiliations) > 2]
    }
    
    editors <- stringr::str_extract(editors, ".*?(?=(<em>|<i>))")
    
    if (is.na(editors[1])) {
      editors <- html_nodes(webpage, xpath = "//p/text()")
      editors <-
        unlist(stringr::str_extract_all(webpage, '(?<=<br>|<p>).*?(?=<br>|</p>)'))
      editors <- editors[nchar(editors) > 5]
      editors <- editors[grepl("\\(", editors)]
      
      affiliations <-
        stringr::str_extract(editors, '(?<=\\().+?(?=\\))')
      affiliations <- unlist(affiliations)
      
      editors <- stringr::str_extract(editors, ".*?(?=\\()")
    }
    if (is.na(editors[1])) {
      editors <- html_nodes(webpage, xpath = "//p/text()")
      editors <-
        unlist(stringr::str_extract_all(webpage, '(?<=<br>|<p>).*?(?=<br>|</p>)'))
      editors <- editors[nchar(editors) > 5]
      
      affiliations <-
        stringr::str_extract_all(editors, '(?<=,).*?$')
      affiliations <- unlist(affiliations)
      
      editors <- stringr::str_extract(editors, ".*?(?=,)")
    }
    if (is.na(editors[1])) {
      editors <- html_nodes(webpage, "li") %>%
        html_text()
      
      affiliations <- stringr::str_extract(editors, '(?<=,).*?$')
      affiliations <- unlist(affiliations)
      
      editors <- stringr::str_extract(editors, ".*?(?=,)")
    }
    if (is.na(editors[1])) {
      editors <-
        unlist(
          stringr::str_extract_all(webpage, '(?<=<p class=\"follows-h3\">).*?(?=</p>)')
        )
      
      affiliations <- stringr::str_extract(editors, '(?<=,).*?$')
      affiliations <- unlist(affiliations)
      
      editors <- stringr::str_extract(editors, ".*?(?=,)")
    }
    
    #=====================
    # (3) remove empty parts from list & clean data
    #=====================
    editors <- editors[!sapply(editors, identical, character(0))]
    affiliations <-
      affiliations[!sapply(affiliations, identical, character(0))]
    editors <- stringr::str_squish(editors)
    
    editors <- trimws(clean_html(editors))
    affiliations <- trimws(clean_html(affiliations))
    
    editors <- gsub(",$|, $", "", editors)
    
    editors <- editors[!is.na(editors)]
    affiliations <-
      affiliations[!is.na(affiliations) &
                     affiliations != "character(0)"]
    
    #=====================
    # (4) CHECK
    #=====================
    # they should be of equal length:
    len1 <- length(affiliations)
    len2 <- length(editors)
    
    if (len1 == len2 & len1 != 0) {
      EdB <- do.call(rbind, c(
        Map(
          data.frame,
          editors = editors,
          affiliations = affiliations,
          roles = "Editorial Board",
          publisher = "Springer Nature",
          journal = journals$journal[i],
          url = journals$url[i]
        ),
        make.row.names = FALSE
      ))
      EdB$date <- Sys.Date()
      
      EdList[[i]] <- EdB
      
      print(paste0("~*~*~*~* Yay! *~*~*~*~       found ", nrow(EdB), " editors"))
    } else {
      EdB <- data.frame(
        editor = editors,
        affiliation = NA,
        role = "Editorial Board",
        publisher = "Springer Nature",
        journal = journals$journal[i],
        url = journals$url[i]
      )
      EdB$date <- Sys.Date()
      
      EdList[[i]] <- EdB
      
      print(paste0("~*~*~*~* Yay! *~*~*~*~       found ", nrow(EdB), " editors"))
    }
    
    Sys.sleep(5)
    
  } else {
    #
    #
    # N a t u r e -- E d i t o r s?
    #
    #
    
    printtext <- paste(i, journals$url[i], sep=": ")
    print(printtext)
    
    webpage <-
      html_node(ALL,
                "#content div.grid.grid-9.last.mq640-grid-12.mq640-last.mt30.mq640-mt0")
    if (length(webpage) == 0) {
      webpage <- html_node(ALL, "#content")
    }
    xmltext <- trimws(webpage)
    
    #=====================
    # (1) parse roles
    #=====================
    roles <- NA
    
    #=====================
    # (2) parse editors
    #=====================
    editors <- html_nodes(webpage, "strong") %>%
      html_text()
    editors <- editors[nchar(editors) > 5]
    if (length(editors) == 0) {
      editors <- html_nodes(webpage, "h2") %>%
        html_text()
      editors <- editors[nchar(editors) > 5]
    }
    editors <- editors[editors != "Editorial Board"]
    
    #=====================
    # (3) parse affiliations
    #=====================
    affiliations <-
      stringr::str_extract_all(webpage,
                               '(?<=</strong></p>|</b>)[\\s\\S]*?(?=<strong>|</div>|<b>)')
    
    affiliations <- unlist(lapply(affiliations, function(x) {
      trimws(clean_html(x))
      gsub("[\r\n]", " ", x)
      stringr::str_squish(x)
    }))
    affiliations <- clean_html(affiliations)
    affiliations <- trimws(affiliations)
    affiliations <- affiliations[nchar(affiliations) > 8]
    #=====================
    # (4) remove empty parts from list & clean data
    #=====================
    editors <- editors[!sapply(editors, identical, character(0))]
    affiliations <-
      affiliations[!sapply(affiliations, identical, character(0))]
    
    editors <- editors[!is.na(editors)]
    affiliations <-
      affiliations[!is.na(affiliations) &
                     affiliations != "character(0)"]
    
    #=====================
    # (5) CHECK
    #=====================
    # they should be of equal length:
    len1 <- length(affiliations)
    len2 <- length(editors)
    
    if (len2 < len1) {
      editors <-
        stringr::str_extract_all(webpage,
                                 '(?<=<strong>|<b>)[\\s\\S]*?(?=</strong>|</b>)')
      editors <- unlist(editors)
      editors <- trimws(clean_html(editors))
      editors <- editors[nchar(editors) > 5]
      editors <- editors[!grepl("Editor", editors)]
    }
    len1 <- length(affiliations)
    len2 <- length(editors)
    if (len1 != len2) {
      affiliations <-
        stringr::str_extract_all(webpage,
                                 '(?<=(.jpg|.PNG|.png|.JPG)\">)[\\s\\S]*?(?=</p>)')
      
      affiliations <- unlist(lapply(affiliations, function(x) {
        trimws(clean_html(x))
        gsub("[\r\n]", " ", x)
        stringr::str_squish(x)
      }))
      affiliations <- clean_html(affiliations)
      affiliations <- trimws(affiliations)
      if (len2 != length(affiliations) &
          length(affiliations) != 0) {
        affiliations <- affiliations[nchar(affiliations) > 8]
      }
      
      if (length(editors) != length(affiliations) &
          length(affiliations) != 0) {
        editors <- editors[!grepl(":", editors)]
      }
      if (length(affiliations) == 0) {
        affiliations <-
          stringr::str_extract_all(webpage, '(?<=<p>)[\\s\\S]*?(?=</p>)')
        affiliations <- unlist(lapply(affiliations, function(x) {
          clean_html(x)
        }))
        affiliations <-
          str_replace_all(affiliations,
                          "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+",
                          "")
        affiliations <- affiliations[nchar(affiliations) > 9]
        
        if (length(affiliations) > length(editors)) {
          affiliations <-
            affiliations[grepl(
              "University|Department|Institute|cholarship|postgraduate",
              affiliations
            )]
        }
      }
    }
    
    len1 <- length(affiliations)
    len2 <- length(editors)
    
    if (len1 == len2 & len1 != 0) {
      EdB <- do.call(rbind, c(
        Map(
          data.frame,
          editor = editors,
          affiliation = affiliations,
          role = roles,
          publisher = "Springer Nature",
          journal = journals$journal[i],
          url = journals$url[i]
        ),
        make.row.names = FALSE
      ))
      EdB$date <- Sys.Date()
      
      EdList[[i]] <- EdB
      
      print(paste0("~*~*~*~* Yay! *~*~*~*~       found ", nrow(EdB), " editors"))
    } else {
      EdB <- data.frame(
        editor = editors,
        affiliation = NA,
        role = "Editorial Board",
        publisher = "Springer Nature",
        journal = journals$journal[i],
        url = journals$url[i]
      )
      EdB$date <- Sys.Date()
      
      EdList[[i]] <- EdB
      
      print(paste0("~*~*~*~* Yay! *~*~*~*~       found ", nrow(EdB), " editors"))
      
    }
    
    Sys.sleep(5)
    #
    # } else {
    #   #
    #   #
    #   # N a t u r e -- E d i t o r s?
    #   # 2 n d   a t t e m p t
    #   #
    #
    #   print("------- Nature (Editors?) -- 2nd attempt")
    #
    #   webpage <-
    #     html_node(
    #       ALL,
    #       "#content div.grid.grid-9.last.mq640-grid-12.mq640-last.mt30.mq640-mt0"
    #     )
    #   if (length(webpage) == 0) {
    #     webpage <- html_node(ALL, "#content")
    #   }
    #   xmltext <- trimws(webpage)
    #
    #   #=====================
    #   # (1) parse roles
    #   #=====================
    #   roles <- NA
    #
    #   #=====================
    #   # (2) parse editors
    #   #=====================
    #   people <- html_nodes(webpage, "div.editor")
    #   editors <- html_node(people, "strong") %>%
    #     html_text()
    #
    #   #=====================
    #   # (3) parse affiliations
    #   #=====================
    #
    #   affiliations <- html_node(people, "p.norm:nth-child(2)")
    #   affiliations <-
    #     lapply(affiliations, function(x)
    #       trimws(clean_html(x)))
    #   affiliations <- unlist(affiliations)
    #
    #   #=====================
    #   # (4) CHECK
    #   #=====================
    #   # they should be of equal length:
    #   len1 <- length(affiliations)
    #   len2 <- length(editors)
    #
    #   if (len1 == len2 & len1 != 0) {
    #     EdB <- do.call(rbind, c(
    #       Map(
    #         data.frame,
    #         editor = editors,
    #         affiliation = affiliations,
    #         role = roles,
    #         publisher = "Springer Nature",
    #         journal = journals$journal[i],
    #         url = journals$url[i]
    #       ),
    #       make.row.names = FALSE
    #     ))
    #     EdB$date <- Sys.Date()
    #
    #     EdList[[i]] <- EdB
    #
    #     print(paste0("~*~*~*~* Yay! *~*~*~*~       found ", nrow(EdB), " editors"))
    #
    #   } else {
    #     Sys.sleep(8)
    #     next
    #
    #   } # nature - editors: 2nd attempt
    
  } # nature - editors
  
}

DF <- dplyr::bind_rows(EdList) %>%
  select(
    publisher,
    journal,
    role,
    editor,
    affiliation,
    url,
    date
  )
DF2 <- DF %>% filter(!is.na(editor))
DF2 <- DF2 %>%
  filter(!grepl("meet-the-editors", url))
DF2 <- DF2 %>%
  filter(!grepl("(E|e)ditor(?!:)", editor, perl = TRUE))
DF2 <- DF2 %>%
  filter(!grepl("^\\w+$", editor))

write_tsv(DF2,
          paste0("Output\\2022-Scraping\\SpringerNature-", Sys.Date(), ".tsv"))
