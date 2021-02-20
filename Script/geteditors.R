# =========================================
#
# TABLE OF CONTENTS
#
# =========================================
#
# -) APA (Am Psych Assoc)
# -) ASCE (Am Soc Chem Engineers)
# -) Brill
# -) Cambridge University Press
# -) Elsevier
# -) Emerald
# -) ------ (Frontiers) [=====> see separate file]
# -) Hindawi
# -) IGI Global
# -) InderScience
# -) John Benjamins
# -) Karger
# -) MDPI
# -) Pleiades
# -) ------ (PLOS) [=====> see separate files]
# -) RSC (Royal Soc Chem)
# -) SAGE


get_editors <- function(journal_url) {
  
  # =============== APA ===============
if (grepl("apa.org", journal_url)) {
  wholepage <- xml2::read_html(url(journal_url))
  
  journalname <- rvest::html_nodes(wholepage, css = "h1")
  journalname <- rvest::html_text(journalname)
  
  issn <- rvest::html_node(wholepage, css = "#topcontent > div > section.info > div.prod_detail > div:nth-child(1) > span")
  issn <- rvest::html_text(issn)
  
  webpage <- rvest::html_nodes(wholepage, css = "#tab_item-1")
  
  roles <- rvest::html_nodes(webpage, css = "h2")
  roles <- rvest::html_text(roles)
  
  webpage <- stringr::str_extract_all(webpage, "(?<=<h2>)[\\S\\s]*?(?=(<h2>|</section>))")
  
  people <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=<p>)[\\S\\s]*?(?=<br>)"))
  names(people[[1]]) <- roles
  
  affiliations <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=<em>)[\\S\\s]*?(?=</em>)"))
  affiliations <- unlist(affiliations)
  
  EdB <- stack(people[[1]])
  names(EdB) <- c("editor", "role")
  EdB$affiliation <- affiliations
  EdB$journal <- journalname
  EdB$publisher <- "American Psychological Association"
  EdB$issn <- issn
  EdB$url <- journal_url
  EdB$date <- Sys.Date()
  
  return(EdB)
  Sys.sleep
  
  # =============== ASCE ===============
} else if (grepl("ascelibrary.org", journal_url)) {
  wholepage <- xml2::read_html(url(journal_url))
  
  journalname <- rvest::html_nodes(wholepage, css = "h2")
  journalname <- rvest::html_text(journalname)
  
  webpage <- rvest::html_nodes(wholepage, css = "#snippet-Editorial_Boards > div:nth-child(1) > div:nth-child(1)")
  webpage <- stringr::str_extract_all(webpage, "(?<=<b>)[\\S\\s]*?(?=(<b>|</div>))")
  webpage[[1]] <- webpage[[1]][nchar(webpage[[1]]) > 30] # delete first part which does not contain any names
  
  roles <- lapply(webpage[[1]], function(x) stringr::str_extract_all(x, "^.*?(?=(:|</b>))"))
  roles <- unlist(roles)
  
  people <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=<br>.?\n).*?(?=,)"))
  names(people[[1]]) <- roles
  
  affiliations <- lapply(webpage, function(x) stringr::str_extract_all(x, "(?<=<i>).*?(?=<br>)"))
  affiliations <- unlist(affiliations)
  
  EdB <- stack(people[[1]])
  EdB$affiliation <- affiliations
  EdB$affiliation <- (gsub("<.*?>", "", EdB$affiliation))
  EdB$journal <- journalname
  EdB$publisher <- "American Society of Civil Engineers"
  EdB$issn <- NA
  EdB$url <- journal_url
  EdB$date <- Sys.Date()
  
  return(EdB)
  Sys.sleep(3)
  
  # =============== Brill ===============
} else if (grepl("brill.com", journal_url)) {
  wholepage <- rvest::html_session(
    journal_url,
    httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
  )
  # wholepage <- xml2::read_html(url(journal_url))
  
  journalname <- rvest::html_nodes(wholepage, css = "h1")
  journalname <- rvest::html_text(journalname)
  
  issn <- rvest::html_node(wholepage, css = ".pr-4 > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > dl:nth-child(1) > dd:nth-child(2)")
  issn <- rvest::html_text(issn)
  issn <- trimws(issn)
  
  webpage <- rvest::html_nodes(wholepage, css = "#container-34067-item-34059 > div:nth-child(1)")
  
  webpage <- toString(webpage)
  
  editorsnodes <- stringr::str_extract_all(
    webpage,
    "(?<=<b>)[\\S\\s]*?(?=(<b>|</div>))"
  )
  
  if (length(editorsnodes[[1]]) > 0) {
    roles <- lapply(editorsnodes, function(x) stringr::str_extract(x, "^.+?(?=</b>)"))
    roles <- unlist(roles)
    
    peoplesnodes <- lapply(editorsnodes, function(x) stringr::str_extract_all(x, "(?<=(<br />|<br>))[\\S\\s]{15,180}?(?=(<br|$))"))
    peoplesnodes <- lapply(rapply(peoplesnodes, enquote, how = "unlist"), eval)
    
    people <- lapply(peoplesnodes, function(x) stringr::str_extract(x, "^.+?(?=(,|\\n<i>|$))"))
    
    affiliations <- lapply(peoplesnodes, function(x) stringr::str_extract(x, "(?<=(<i>|<br>.{0,40},)).{8,120}?(?=(</i>|\\n))"))
    
    EdB <- do.call(rbind, Map(data.frame, editor = people, affiliation = affiliations, role = roles))
    EdB$journal <- journalname
    EdB$publisher <- "Brill"
    EdB$url <- journal_url
    EdB$issn <- issn
    EdB$date <- Sys.Date()
  } else {
    EdB <- data.frame(
      "editor" = NA,
      "affiliation" = NA,
      "role" = NA,
      "journal" = journalname,
      "publisher" = "Brill",
      "url" = journal_url,
      "issn" = issn,
      "date" = Sys.Date()
    )
  }
  
  return(EdB)
  Sys.sleep(3)
  
  # =============== CAMBRIDGE UNIV PRESS ===============
} else if (grepl("cambridge.org", journal_url)) {
  wholepage <- xml2::read_html(journal_url)
  
  journalname <- rvest::html_nodes(wholepage, xpath = "/html/body/section[4]/div/div/div/h1")
  journalname <- rvest::html_text(journalname)
  journalname <- trimws(journalname)
  
  webpage <- rvest::html_nodes(wholepage, ".editorial-position")
  
  editors <- lapply(webpage, function(x) rvest::html_nodes(x, "p"))
  editors <- lapply(editors, function(x) rvest::html_text(x))
  editors <- lapply(editors, function(x) trimws(x))
  
  roles <- lapply(webpage, function(x) rvest::html_nodes(x, "h6"))
  roles <- lapply(roles, function(x) rvest::html_text(x))
  roles <- unlist(roles)
  names(editors) <- roles
  
  EdB <- plyr::ldply(editors, data.frame)
  if (length(EdB) > 0) {
    EdB$journal <- journalname
    EdB$publisher <- "Cambridge University Press"
    EdB$url <- journal_url
    EdB$date <- Sys.Date()
    colnames(EdB) <- c("role", "editor", "journal", "publisher", "url", "date")
    EdB <- dplyr::mutate(EdB, affiliation = stringr::str_extract(editor, "(?<=, ).*"))
    EdB$editor <- lapply(EdB$editor, function(x) stringr::str_extract(x, "^.*?(?=,|$)"))
  } else {
    EdB <- data.frame(
      "role" = NA,
      "editor" = NA,
      "journal" = journalname,
      "publisher" = "Cambridge University Press",
      "url" = journal_url,
      "date" = Sys.Date()
    )
  }
  
  return(EdB)
  Sys.sleep(11)
  
  # =============== ELSEVIER ===============
  } else if (grepl("journals.elsevier.com", journal_url)) {
    wholepage <- xml2::read_html(journal_url)

    journalname <- rvest::html_node(wholepage, "h1")
    journalname <- rvest::html_text(journalname)
    journalname <- stringr::str_extract(journalname, "^.*(?= -)")

    if (is.na(journalname)) {
      journalname <- stringr::str_extract(journal_url, "(?<=com/).*?(?=/edit)")

      EdB <- data.frame(
        "role" = NA,
        "editor" = NA,
        "affiliation" = NA,
        "journal" = journalname,
        "publisher" = "Elsevier",
        "url" = journal_url,
        "date" = Sys.Date()
      )
      return(EdB)
    }

    webpage <- rvest::html_nodes(wholepage, "div.publication-editors")

    editorsnodes <- rvest::html_children(webpage)

    titlesnodesnum <- which(rvest::html_attr(editorsnodes, "class") == "publication-editor-type")
    titles <- editorsnodes[titlesnodesnum]
    titles <- rvest::html_text(titles)
    titles <- trimws(titles)
    titlesnodesnum <- c(titlesnodesnum, length(editorsnodes) + 1) # identify the last record

    editors <- lapply(2:length(titlesnodesnum), function(n) {
      start <- titlesnodesnum[n - 1] + 1 # starting node in subcategory
      end <- titlesnodesnum [n] - 1 # ending node in subcategory
      names <- editorsnodes[start:end]
      names <- rvest::html_nodes(names, "div.publication-editor-name")
      names <- rvest::html_text(names)
      names <- trimws(names)
    })

    names(editors) <- titles

    affiliations <- lapply(2:length(titlesnodesnum), function(n) {
      start <- titlesnodesnum[n - 1] + 1 # starting node in subcategory
      end <- titlesnodesnum [n] - 1 # ending node in subcategory
      affiliations <- toString(editorsnodes[start:end])
      affiliations <- stringr::str_extract_all(affiliations, "(?<=<div class=\"publication-editor\")[\\S\\s]*?(?=<div class=\"clearfix\">)")
      affiliations <- lapply(affiliations, function(x) stringr::str_extract(x, "(?<=<span class=\"publication-editor-affiliation\" itemprop=\"affiliation\">).*?(?=</span>)"))
    })

    ed_af <- lapply(
      seq_along(editors),
      function(i) {
        data.frame(
          editor = editors[[i]],
          affiliation = affiliations[[i]],
          stringsAsFactors = FALSE
        )
      }
    )
    ed_af <- lapply(ed_af, setNames, c("editor", "affiliation"))
    EdB <- data.frame(role = names(editors), stringsAsFactors = FALSE)
    EdB <- dplyr::mutate(EdB, data = ed_af)
    EdB <- tidyr::unnest(EdB, cols = "data")
    EdB$journal <- journalname
    EdB$publisher <- "Elsevier"
    EdB$url <- journal_url
    EdB$date <- Sys.Date()

    return(EdB)


    Sys.sleep(3)

    # =============== EMERALD ===============
  } else if (grepl("emeraldgrouppublishing.com", journal_url)) {
    wholepage <- rvest::html_session(
      journal_url,
      httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
    )

    journalname <- rvest::html_nodes(wholepage, css = ".journal-header__title")
    journalname <- rvest::html_text(journalname)
    journalname <- trimws(journalname)

    issn <- rvest::html_nodes(wholepage, css = ".journal-header__issn")
    issn <- toString(issn)
    issn <- stringr::str_extract(issn, "(?<=ISSN:\\s{0,40}).*(?=<)")
    issn <- trimws(issn)

    webpage <- rvest::html_nodes(wholepage, css = ".editorial-team")

    people <- rvest::html_nodes(webpage, css = "ul")
    people[1] <- NULL
    people <- lapply(people, function(x) rvest::html_nodes(x, css = "li"))
    people <- sapply(people, function(x) stringr::str_extract(x, "(?<=<li>\\s{1,50})(.|\\s)*?(?=<)"))
    people <- sapply(people, function(x) trimws(x))
    people <- sapply(people, function(x) stringr::str_squish(x))

    roles <- rvest::html_nodes(webpage, css = "h4")
    roles <- rvest::html_text(roles)

    # NOT WORKING YET (disentangling affiliation and country)
    affiliations <- rvest::html_nodes(webpage, css = "ul")
    affiliations[1] <- NULL
    affiliations <- lapply(affiliations, function(x) stringr::str_extract_all(x, "(?<=<em>)[\\s\\S]*?(?=<li>|$)"))
    affiliations <- lapply(affiliations, function(x) {
      lapply(x, function(y) stringr::str_extract(y, "^[\\s\\S]+(?=</em>)"))
    })
    affiliations <- lapply(affiliations, function(x) {
      lapply(x, function(y) stringr::str_replace_all(y, "[\t|\n]|(</em>)", ""))
    })
    affiliations <- lapply(affiliations, function(x) {
      lapply(x, function(y) stringr::str_replace_all(y, "(-<em>)", ", "))
    })

    affiliations <- lapply(affiliations, function(x) unlist(x))
    affiliations <- lapply(affiliations, function(x) if (identical(x, character(0))) NA_character_ else x)


    EdB <- do.call(rbind, Map(data.frame, people = people, affiliation = affiliations, roles = roles))
    EdB$journalname <- journalname
    EdB$issn <- issn
    EdB$url <- journal_url
    EdB$publisher <- "Emerald"
    EdB$date <- Sys.Date()
    colnames(EdB) <- c("editor", "affiliation", "role", "journal", "issn", "url", "publisher", "date")

    return(EdB)
    Sys.sleep(3)

    # =============== HINDAWI ===============
  } else if (grepl("hindawi.com", journal_url)) {

    # wholepage <- rvest::html_session(journal_url,
    #                          httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20"))
    wholepage <- xml2::read_html(url(journal_url))

    journalname <- rvest::html_nodes(wholepage, css = ".article__SubtitleStyled-sc-10qhnp8-2")
    journalname <- rvest::html_text(journalname)

    issn <- toString(wholepage)
    issn <- stringr::str_extract(issn, "(?<=\"journal_issn\":\").*?(?=\",)")

    webpage <- rvest::html_nodes(wholepage, css = ".staticContents__StaticContentsSectionsWrapper-amdbga-1")

    people <- rvest::html_nodes(webpage, css = "ul")
    people <- lapply(people, function(x) rvest::html_nodes(x, css = "li"))
    if (grepl("<strong>", people[[1]][1], fixed = T)) {
      people <- lapply(people, function(x) rvest::html_nodes(x, css = "strong"))
    } else {
      people <- lapply(people, function(x) rvest::html_nodes(x, css = "span"))
    }
    people <- sapply(people, function(x) stringr::str_extract(x, "(?<=>)(.|\\s)*?(?=<)"))
    people <- purrr::compact(people)

    affiliations <- rvest::html_nodes(webpage, css = "ul")
    affiliations <- lapply(affiliations, function(x) rvest::html_nodes(x, css = "li"))
    affiliations <- sapply(affiliations, function(x) stringr::str_extract(x, "(?<=>, )(.|\\s)*?(?=<)"))
    affiliations <- purrr::compact(affiliations)

    orcid <- rvest::html_nodes(webpage, css = "ul")
    orcid <- lapply(orcid, function(x) rvest::html_nodes(x, css = "li"))
    orcid <- lapply(orcid, function(x) ifelse(grepl("orcid", x), stringr::str_extract_all(x, "(?<=orcid.org/)(.|\\s)*?(?=\")"), NA))
    orcid <- purrr::compact(orcid)
    orcid <- unlist(orcid)

    roles <- rvest::html_nodes(webpage, css = "h2")
    roles <- rvest::html_text(roles)

    EdB <- do.call(rbind, Map(data.frame, editor = people, affiliation = affiliations, role = roles))
    EdB$orcid <- orcid
    EdB$journal <- journalname
    EdB$publisher <- "Hindawi"
    EdB$issn <- issn
    EdB$url <- journal_url
    EdB$date <- Sys.Date()

    return(EdB)
    Sys.sleep(3)
    
    # =============== IGI GLOBAL ===============
  } else if (grepl("igi-global", journal_url)) {
    wholepage <- xml2::read_html(url(journal_url))
    
    journalname <- rvest::html_nodes(wholepage, css = "#lblTitle")
    journalname <- rvest::html_text(journalname)
    
    issn <- rvest::html_node(wholepage, css = "div.isbn-doi-inner > div > span > span:nth-child(2)")
    issn <- rvest::html_text(issn)
    
    webpage <- rvest::html_nodes(wholepage, css = "div.editorial-board-container")
    webpage <- rvest::html_nodes(webpage, xpath = '//div[contains(@id, "divEditorialBoard")]')
    
    people <- lapply(webpage, function(x) rvest::html_nodes(x, css = "dd"))
    people <- purrr::compact(people)
    people <- lapply(people, function(x) rvest::html_text(x))
    people <- lapply(people, function(x) trimws(x))
    people <- lapply(people, function(x) stringr::str_extract(x, ".*?(?=,)"))
    
    affiliations <- lapply(webpage, function(x) rvest::html_nodes(x, css = "dd"))
    affiliations <- purrr::compact(affiliations)
    affiliations <- lapply(affiliations, function(x) rvest::html_text(x))
    affiliations <- lapply(affiliations, function(x) stringr::str_extract(x, "(?<=, ).*"))
    affiliations <- lapply(affiliations, function(x) trimws(x))
    
    orcid <- lapply(webpage, function(x) rvest::html_nodes(x, css = "dd"))
    orcid <- purrr::compact(orcid)
    orcid <- lapply(orcid, function(x) rvest::html_text(x))
    orcid <- lapply(orcid, function(x) stringr::str_extract(x, "(?=https://orcid.org).*$"))
    
    roles <- rvest::html_nodes(wholepage, css = "dt")
    roles <- rvest::html_text(roles)
    
    EdB <- do.call(rbind, Map(data.frame, editor = people, affiliation = affiliations, role = roles))
    EdB$orcid <- unlist(orcid)
    EdB$journal <- journalname
    EdB$publisher <- "IGI Global"
    EdB$issn <- issn
    EdB$url <- journal_url
    EdB$date <- Sys.Date()
    
    return(EdB)
    Sys.sleep(3)
    
    # =============== INDERSCIENCE ===============
  } else if (grepl("inderscience.com", journal_url)) {
    wholepage <- rvest::html_session(
      journal_url,
      httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
    )
    # wholepage <- xml2::read_html(url(journal_url))
    
    journalname <- rvest::html_nodes(wholepage, css = "h2.display-4")
    journalname <- rvest::html_text(journalname)
    
    issn <- rvest::html_node(wholepage, css = "div.row:nth-child(4) > div:nth-child(2) > dl:nth-child(1)")
    issn <- rvest::html_text(issn)
    issn_online <- stringr::str_extract(issn, "(?<=ISSN online).*?(?=ISSN)")
    issn_print <- stringr::str_extract(issn, "(?<=ISSN print).........")
    
    webpage <- rvest::html_nodes(wholepage, css = "#edboard-content")
    
    roles <- rvest::html_nodes(webpage, css = "h3")
    roles <- rvest::html_text(roles)
    
    peoples <- rvest::html_nodes(webpage, css = "ul")
    peoples <- lapply(peoples, function(x) rvest::html_nodes(x, css = "li"))
    peoples <- sapply(peoples, function(x) rvest::html_text(x))
    
    people <- sapply(peoples, function(x) stringr::str_extract(x, "^(.*?,.*?)(?=,)"))
    
    affiliations <- sapply(peoples, function(x) stringr::str_extract(x, "(?<=(.{1,70},.{1,70}?,)).*"))
    affiliations <- sapply(affiliations, function(x) trimws(x))
    
    EdB <- do.call(rbind, Map(data.frame, editor = people, affiliation = affiliations, role = roles))
    EdB$journal <- journalname
    EdB$issn <- issn_online
    EdB$url <- journal_url
    EdB$publisher <- "Inderscience"
    EdB$date <- Sys.Date()
    
    return(EdB)
    Sys.sleep(3)
    
    
    # =============== JOHN BENJAMIS ===============
  } else if (grepl("benjamis.com", journal_url)) {
    wholepage <- xml2::read_html(url(journal_url))
    
    journalname <- rvest::html_node(wholepage, css = "h1")
    journalname <- rvest::html_text(journalname)
    journalname <- stringr::str_squish(journalname)
    
    issn <- rvest::html_node(wholepage, css = "div.booktext:nth-child(5)")
    issn <- rvest::html_text(issn)
    issn <- stringr::str_extract(issn, "(?<=ISSN ).*?(?= )")
    
    webpage <- rvest::html_nodes(wholepage, css = "#board")
    webpage <- rvest::html_nodes(webpage, css = ".contribgroup")
    
    people <- lapply(webpage, function(x) rvest::html_nodes(x, css = "span.name"))
    people <- lapply(people, function(x) rvest::html_text(x))
    people <- lapply(people, function(x) stringr::str_squish(x))
    
    affiliations <- lapply(webpage, function(x) rvest::html_nodes(x, css = ".member"))
    affiliations <- lapply(affiliations, function(x) stringr::str_extract(x, "(?<=ISSN pipe\"> | </span>).*?(?=(<span|</div>))"))
    affiliations <- unlist(affiliations)
    
    
    orcid <- lapply(webpage, function(x) rvest::html_nodes(x, css = ".member"))
    orcid <- lapply(orcid, function(x) ifelse(grepl("orcid", x), stringr::str_extract_all(x, "(?<=orcid.org/)(.|\\s)*?(?=\")"), NA))
    orcid <- purrr::compact(orcid)
    orcid <- unlist(orcid)
    
    roles <- rvest::html_nodes(wholepage, css = ".contribgroup_heading")
    roles <- rvest::html_text(roles)
    
    names(people) <- roles
    
    EdB <- do.call(rbind, Map(data.frame, editor = people, affiliation = affiliations, role = roles))
    EdB$journal <- journalname
    EdB$publisher <- "John Benjamins"
    EdB$issn <- issn
    EdB$url <- journal_url
    EdB$date <- Sys.Date()
    
    return(EdB)
    Sys.sleep(3)
    
    # =============== KARGER ===============
  } else if (grepl("karger.com", journal_url)) {
    wholepage <- rvest::html_session(
      journal_url,
      httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
    )
    
    journalname <- rvest::html_node(wholepage, css = "h2")
    journalname <- rvest::html_text(journalname)
    
    issn <- rvest::html_node(wholepage, css = "div.search:nth-child(6)")
    issn <- rvest::html_text(issn)
    issn <- stringr::str_extract(issn, "(?<=ISSN: ).*?(?= )")
    
    webpage <- rvest::html_node(wholepage, css = "#editorialboard")
    
    # get h3 headings
    headings <- rvest::html_nodes(webpage, "h3")
    headings <- rvest::html_text(headings)
    
    # get raw text
    raw.text <- rvest::html_text(webpage)
    
    # split raw text on h3 headings and put in a list
    list.members <- list()
    raw.text.2 <- raw.text
    for (h in headings) {
      # split on headings
      b <- strsplit(raw.text.2, h, fixed = TRUE)
      # split members using \n as separator
      c <- strsplit(b[[1]][1], "\n", fixed = TRUE)
      # clean empty elements from vector
      c <- list(c[[1]][c[[1]] != ""])
      # add vector of member to list
      list.members <- c(list.members, c)
      # update text
      raw.text.2 <- b[[1]][2]
    }
    # remove first element of main list
    list.members <- list.members[2:length(list.members)]
    # add final segment of raw.text to list
    c <- strsplit(raw.text.2, "\n", fixed = TRUE)
    c <- list(c[[1]][c[[1]] != ""])
    list.members <- c(list.members, c)
    # add names to list
    names(list.members) <- headings
    
    people <- lapply(list.members, function(x) stringr::str_extract(x, "^.*?(?= -)"))
    
    affiliations <- lapply(list.members, function(x) stringr::str_extract(x, "(?<= - ).*$"))
    affiliations <- unlist(unname(affiliations))
    
    EdB <- stack(people)
    names(EdB) <- c("editor", "role")
    EdB$affiliation <- affiliations
    EdB$journal <- journalname
    EdB$publisher <- "Karger"
    EdB$issn <- issn
    EdB$url <- journal_url
    EdB$date <- Sys.Date()
    
    
    return(EdB)
    Sys.sleep(3)
    
    # =============== MDPI ===============
  } else if (grepl(".mdpi.com", journal_url)) {

    # CHROME
    # eCaps <- list(chromeOptions = list(
    #  args = list('--user-agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko)"')
    # ))
    # rD <- RSelenium::rsDriver(browser="chrome", port=4546L, verbose=F, chromever="87.0.4280.20",
    #                          extraCapabilities = eCaps)

    # FIREFOX
    eCaps <- list(`mox:firefoxOptions` = list(general.useragent.override = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:84.0) Gecko/20100101 Firefox/84.0"))
    rD <- RSelenium::rsDriver(
      browser = "firefox", port = 4546L, verbose = F,
      extraCapabilities = eCaps
    )
    # rD <- RSelenium::rsDriver(browser="chrome", port=4546L, verbose=F)
    remDr <- rD[["client"]]
    remDr$navigate(journal_url)

    for (i in 1:5) {
      remDr$executeScript(paste("scroll(0,", i * 10000, ");"))
      Sys.sleep(3)
    }

    wholepage <- remDr$getPageSource()
    wholepage <- xml2::read_html(wholepage[[1]])

    journalname <- rvest::html_nodes(wholepage, css = "#js-main-top-container > a:nth-child(1) > img:nth-child(1)")
    journalname <- stringr::str_extract(journalname, "(?<= title=\").*?(?=\")")

    issn <- rvest::html_nodes(wholepage, css = ".journal-info > span:nth-child(1)")
    issn <- stringr::str_extract(issn, "(?<=EISSN ).*?(?=,)")

    webpage <- rvest::html_nodes(wholepage, css = ".middle-column__main")

    roles <- rvest::html_nodes(webpage, css = "h2")
    roles <- rvest::html_text(roles)
    rolesnumber <- unlist(lapply(roles, function(x) stringr::str_extract(x, "(?<=\\().*?(?=\\))")))
    rolesnumber <- unlist(lapply(rolesnumber, function(x) ifelse(is.na(x), 1, x)))
    roles <- unlist(lapply(roles, function(x) stringr::str_extract(x, "^(.|\\s)*?(?=( \\(|$))")))
    roles <- rep(roles, rolesnumber)

    peoples <- rvest::html_nodes(webpage, css = ".editor-div__content")

    people <- as.character(peoples[seq(1, length(peoples), 2)])
    people <- unlist(lapply(people, function(x) stringr::str_extract(x, "(?<=<b>).*?(?=</b>)")))

    affiliations <- as.character(peoples[seq(2, length(peoples), 2)])
    affiliations <- unlist(lapply(affiliations, function(x) stringr::str_extract(x, "(?<=>\\n).*?(?=<br>)")))

    EdB <- do.call(rbind, Map(data.frame, editor = people, affiliation = affiliations, role = roles))
    EdB$journal <- journalname
    EdB$issn <- issn
    EdB$publisher <- "MDPI"
    EdB$url <- journal_url
    EdB$date <- Sys.Date()

    remDr$close()
    gc()
    rD$server$stop()
    system("taskkill /im java.exe /f", intern = FALSE, ignore.stdout = FALSE)

    return(EdB)
    
    Sys.sleep(2)

    # =============== PLEIADES ===============
  } else if (grepl("pleiades.online", journal_url)) {
    wholepage <- xml2::read_html(url(journal_url))

    journalname <- rvest::html_node(wholepage, css = "h1")
    journalname <- rvest::html_text(journalname)

    issn <- rvest::html_node(wholepage, xpath = "/html/body/div[4]/div[1]/div[3]/div/div[2]/div[2]/p/text()[1]")
    issn <- rvest::html_text(issn)
    issn <- stringr::str_extract(issn, "(?<=: ).*")

    webpage <- rvest::html_nodes(wholepage, css = "div.tab-content")
    webpage <- rvest::html_nodes(webpage, css = ".collapse")

    people <- lapply(webpage, function(x) rvest::html_nodes(x, css = "td:nth-child(1)"))
    people <- lapply(people, function(x) rvest::html_text(x))

    affiliations <- lapply(webpage, function(x) rvest::html_nodes(x, css = "td:nth-child(2)"))
    affiliations <- lapply(affiliations, function(x) rvest::html_text(x))
    affiliations <- unlist(affiliations)

    roles <- rvest::html_nodes(wholepage, css = "h4")
    roles <- rvest::html_text(roles)
    roles <- trimws(roles)

    names(people) <- roles

    EdB <- stack(people)
    names(EdB) <- c("editor", "role")
    EdB$affiliation <- NA_character_
    EdB$affiliation[seq_along(affiliations)] <- affiliations
    EdB$journal <- journalname
    EdB$publisher <- "Pleiades"
    EdB$issn <- issn
    EdB$url <- journal_url
    EdB$date <- Sys.Date()


    return(EdB)
    Sys.sleep(3)

    # =============== RSC =============== 
  } else if (grepl("rsc.org", journal_url)) {
    
    wholepage <- rvest::html_session(
      journal_url,
      httr::user_agent("Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US) AppleWebKit/534.20 (KHTML, like Gecko) Chrome/11.0.672.2 Safari/534.20")
    )
    # wholepage <- xml2::read_html(url(journal_url))
    
    journalname <- rvest::html_nodes(wholepage, css = "h1")
    journalname <- rvest::html_text(journalname)
    
    issn <- rvest::html_node(wholepage, css = "#subscription-information")
    issn <- toString(issn)
    issn <- stringr::str_extract(issn, "(?<=ISSN ).{8,10}(?=( |</p>))")
    issn <- substr(issn, 1, 9)
    
    webpage <- rvest::html_nodes(wholepage, css = "#boards-staff")
    
    roles <- rvest::html_nodes(webpage, "span.tab__heading")
    roles <- rvest::html_text(roles)
    
    editorsnodes <- rvest::html_nodes(webpage, ".content-block, hashtaglink") # or div.block tabsinnerblock
    
    editorialboard <- toString(editorsnodes[[1]])
    editorialboard2 <- unlist(stringr::str_extract_all(editorialboard, "(?<=<h4>)[\\S\\s]*?(?=(<h4>|$))"))
    if (length(editorialboard2) > 0) {
      editorialboard <- editorialboard2
    } else {
      editorialboard <- toString(editorsnodes[[1]])
    }
    
    edboard_roles <- unlist(stringr::str_extract_all(editorialboard, "^.*?(?=</h4>)"))
    
    edboard_ppl <- lapply(editorialboard, function(x) stringr::str_extract_all(x, "(?<=<strong>).*?(?=(</strong>|</a>))"))
    edboard_ppl <- lapply(rapply(edboard_ppl, enquote, how = "unlist"), eval)
    edboard_ppl <- lapply(edboard_ppl, function(x) x[nchar(x) > 4])
    if (length(editorialboard2) > 0) {
      names(edboard_ppl) <- paste0("Editorial Board: ", edboard_roles)
    } else {
      names(edboard_ppl) <- "Editorial Board"
    }
    
    edboard_affiliations <- unlist(lapply(editorialboard, function(x) stringr::str_extract_all(x, "(?<=,).*(?=</p>)")))
    edboard_affiliations <- unlist(lapply(edboard_affiliations, function(x) x[nchar(x) > 4]))
    
    editorsnodes[[1]] <- NULL
    peoplesnodes <- lapply(editorsnodes, function(x) rvest::html_nodes(x, "p"))
    
    people <- lapply(peoplesnodes, function(x) rvest::html_nodes(x, "strong"))
    people <- lapply(peoplesnodes, function(x) rvest::html_text(x))
    people <- lapply(people, function(x) stringr::str_extract(x, "^.*?(?=,)"))
    
    affiliations <- lapply(peoplesnodes, function(x) stringr::str_extract(x, "(?<=,).*(?=</p>)"))
    
    # MERGE THE THREE CATEGORIES TO DATA FRAMES
    advisory_board <- data.frame("editor" = people[[1]], "role" = "Advisory Board", "affiliation" = affiliations[[1]])
    editorial_office <- data.frame("editor" = people[[2]], "role" = paste0("Editorial Office: ", affiliations[[2]]), "affiliation" = NA)
    edboard <- plyr::ldply(edboard_ppl, data.frame)
    names(edboard) <- c("role", "editor")
    edboard$affiliation <- edboard_affiliations
    
    EdB <- do.call("rbind", list(edboard, advisory_board, editorial_office))
    
    cleanHTML <- function(htmlString) {
      return(gsub("<.*?>", "", htmlString))
    }
    
    EdB[1:3] <- lapply(EdB[1:3], cleanHTML)
    EdB <- as.data.frame(EdB)
    
    EdB$journal <- journalname
    EdB$publisher <- "Royal Society of Chemistry"
    EdB$url <- journal_url
    EdB$issn <- issn
    EdB$date <- Sys.Date()
    
    return(EdB)
    Sys.sleep(3)
    
    # =============== SAGE ===============
  } else if (grepl("journals.sagepub.com", journal_url)) {
    wholepage <- xml2::read_html(url(journal_url))

    journalname <- rvest::html_nodes(wholepage, xpath = '//*[@id="e3c018c7-8573-4acd-93ae-0ff4b1f3baf3"]/div/div')
    journalname <- rvest::html_text(journalname)

    issn <- rvest::html_nodes(wholepage, xpath = '//*[@id="4c8a6b02-2938-4d47-8324-a3ffe9c0abdd"]/div/div/div/div/div[5]/span[1]')
    issn <- toString(issn)
    issn <- stringr::str_extract(issn, "(?<=ISSN: ).*(?=<)")

    webpage <- rvest::html_nodes(wholepage, xpath = '//*[@id="5dfa7b11-3157-4585-b786-54aa88233446"]/div/div/div')

    people <- rvest::html_nodes(webpage, xpath = "//div[@class='editorial-board']/descendant::table")
    people <- rvest::html_table(people, fill = TRUE)

    roles <- rvest::html_nodes(webpage, xpath = "//div[@class='ed-board-name']")
    roles <- rvest::html_text(roles)

    if (length(roles) > 0) {
      EdB <- do.call(rbind, Map(data.frame, people = people, roles = roles))
      EdB$journalname <- journalname
      EdB$publisher <- "SAGE"
      EdB$url <- journal_url
      EdB$issn <- issn
      EdB$date <- Sys.Date()
      colnames(EdB) <- c("editor", "affiliation", "role", "journal", "publisher", "url", "issn", "date")
    } else {
      EdB <- data.frame(
        "editor" = NA,
        "affiliation" = NA,
        "role" = NA,
        "journal" = journalname,
        "publisher" = "SAGE",
        "url" = journal_url,
        "issn" = issn,
        "scrapedate" = Sys.Date()
      )
    }

    return(EdB)
    Sys.sleep(8)

    # =============== ELSE ===============
  } else {
    stop("Could not fetch editors. Try another publisher.")
  }
}
