library(tidyverse)

# read files and merge
A <- list.files(path = "Output\\2022-Scraping\\by_publisher\\", pattern = ".*.tsv")
A <- map_df(A, ~read_tsv(paste0("Output\\2022-Scraping\\by_publisher\\", .x), locale = locale(encoding = "windows-1252")) %>%
           mutate(across(.fns = as.character))) %>%
  type_convert()

A <- A %>%
  # mutate(editor = ifelse(is.na(editor), editors, editor),
  #        affiliation = ifelse(is.na(affiliation), affiliations, affiliation),
  #        role = ifelse(is.na(role), roles, role)) %>%
  select(
    #-roles, -editors, -affiliations,
         editor, orcid, affiliation, role, journal, publisher, issn, date, url)

# harmonize dates, filter out NAs
DF <- A %>%
  filter(!is.na(url)) %>%
  filter(!is.na(editor)) %>%
  filter(!is.na(publisher)) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(!is.na(date))

# remove duplicates
doubles <- duplicated(DF[, c("editor", "journal", "publisher")], )
doubles_df <- DF[doubles, ] # take a look at the duplicates
DF <- DF[!doubles, ]

# remove one-word-editor names (unless it's at SAGE, because there it seems to be correct)
DF <- DF %>%
  filter(!(grepl("^\\w+$", editor) & publisher != "SAGE"))

DF <- DF %>% filter(nchar(as.character(editor)) > 4) 

# remove html tags
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
DF$editor <- cleanFun(DF$editor)
DF$affiliation <- cleanFun(DF$affiliation)
DF$role <- cleanFun(DF$role)

# check each publisher individually
PUB <- unique(DF$publisher)

DF2 <- DF %>% filter(publisher == PUB[24])
View(DF2)
length(unique(DF2$journal))
nrow(DF2)

# for Brill
BRILL <- DF %>% filter(publisher == "Brill")
x <- stringr::str_extract(BRILL$editor, "(?=\\().*")

BRILL <- BRILL %>%
  mutate(editor = stringr::str_remove(editor, "(?=\\().*"),
         affiliation = ifelse(is.na(affiliation), x, affiliation),
         affiliation = stringr::str_remove(affiliation, "^\\("),
         affiliation = stringr::str_remove(affiliation, "\\)$")) 
rm(x)
# for RSC
RSC <- DF %>% filter(publisher == "Royal Society of Chemistry")
RSC <- RSC %>% mutate(role = stringr::str_remove(role, "Editorial Office: "),
                      role = stringr::str_remove(role, "Editorial Board: "))
orc <- stringr::str_extract(RSC$role, "\\d{4}\\-\\d{4}\\-\\d{4}\\-\\d{3}.")
RSC <- RSC %>% mutate(role = stringr::str_remove(role, "ORCID.*"),
                      orcid = orc)
RSC <- RSC %>% mutate(naa = ifelse(is.na(affiliation), TRUE, FALSE),
                      affiliation = ifelse(journal == "Chemical Science" & naa == TRUE, role, affiliation),
                      role = ifelse(journal == "Chemical Science" & naa == TRUE, NA, role),
                      affiliation = ifelse(journal == "Materials Advances" & naa == TRUE, role, affiliation),
                      role = ifelse(journal == "Materials Advances" & naa == TRUE, NA, role),
                      affiliation = ifelse(journal == "Materials Horizons" & naa == TRUE, role, affiliation),
                      role = ifelse(journal == "Materials Horizons" & naa == TRUE, NA, role),
                      affiliation = ifelse(journal == "Nanoscale Horizons" & naa == TRUE, role, affiliation),
                      role = ifelse(journal == "Nanoscale Horizons" & naa == TRUE, NA, role),
                      affiliation = ifelse(journal == "PCCP" & naa == TRUE, role, affiliation),
                      role = ifelse(journal == "PCCP" & naa == TRUE, NA, role),
                      affiliation = ifelse(journal == "Journal of Analytical Atomic Spectrometry" & naa == TRUE, role, affiliation),
                      role = ifelse(journal == "Journal of Analytical Atomic Spectrometry" & naa == TRUE, NA, role),
                      affiliation = ifelse(journal == "Materials Chemistry Frontiers" & naa == TRUE, role, affiliation),
                      role = ifelse(journal == "Materials Chemistry Frontiers" & naa == TRUE, NA, role)
                      ) %>%
  select(-naa)
rm(orc)
# for SciTechnol
SCIT <- DF %>% filter(publisher == "SciTechnol")
SCIT <- SCIT %>% mutate(issn = stringr::str_remove(issn, "ISSN : "))

# for Nature
NATURE <- DF %>% filter(publisher == "Springer Nature")
x <- stringr::str_extract(NATURE$editor, "(?=,).*")
y <- stringr::str_remove(NATURE$editor, "(?=,).*")
x[grepl("PhD|MD|M.D.|MSc|BSc|Dsc|Dr.|DPhil|BChir|MBBS|MRCGP|FRCP|Sc.D.|FRSE|Ph.D|Phd", x) & nchar(x < 7)] <- NA
NATURE <- NATURE %>%
  mutate(editor = y,
         affiliation = ifelse(is.na(affiliation), x, affiliation))
NATURE$affiliation <- gsub("^,", "", NATURE$affiliation)

x <- stringr::str_extract(NATURE$editor, ".*(?<=:)")
y <- stringr::str_remove(NATURE$editor, ".*(?<=:)")
NATURE <- NATURE %>%
  mutate(editor = ifelse(is.na(role), y, editor),
         role = ifelse(is.na(role), x, role))
NATURE$role <- gsub(":$", "", NATURE$role)
rm(x,y)

# merge all data frames together

DF <- DF %>%
  filter(publisher != "Brill") %>%
  filter(publisher != "Royal Society of Chemistry") %>%
  filter(publisher != "SciTechnol") %>%
  filter(publisher != "Springer Nature")

DF <- rbind(DF, BRILL) %>%
  rbind(NATURE, RSC) %>%
  rbind(SCIT)
rm(BRILL, NATURE, RSC, SCIT)

# for all
DF$role <- gsub(":$", "", DF$role)
DF$role <- gsub("^,", "", DF$role)
DF$editor <- gsub(":$", "", DF$editor)
DF$editor <- gsub("^,", "", DF$editor)
DF$affiliation <- gsub(":$", "", DF$affiliation)
DF$affiliation <- gsub("^,", "", DF$affiliation)
DF <- DF %>% 
  mutate(across(where(is.character), str_trim))
DF <- DF %>% 
  mutate(across(where(is.character), str_squish))

# clean encoding (fix wrongful hex-codes)
ascii <- structure(list(Hex = c("<a0>", "<a1>", "<a2>", "<a3>", "<a4>", 
                                "<a5>", "<a6>", "<a7>", "<a8>", "<a9>", "<aa>", "<ab>", "<ac>", 
                                "<ad>", "<ae>", "<af>", "<b0>", "<b1>", "<b2>", "<b3>", "<b4>", 
                                "<b5>", "<b6>", "<b7>", "<b8>", "<b9>", "<ba>", "<bb>", "<bc>", 
                                "<bd>", "<be>", "<bf>", "<c0>", "<c1>", "<c2>", "<c3>", "<c4>", 
                                "<c5>", "<c6>", "<c7>", "<c8>", "<c9>", "<ca>", "<cb>", "<cc>", 
                                "<cd>", "<ce>", "<cf>", "<d0>", "<d1>", "<d2>", "<d3>", "<d4>", 
                                "<d5>", "<d6>", "<d7>", "<d8>", "<d9>", "<da>", "<db>", "<dc>", 
                                "<dd>", "<de>", "<df>", "<e0>", "<e1>", "<e2>", "<e3>", "<e4>", 
                                "<e5>", "<e6>", "<e7>", "<e8>", "<e9>", "<ea>", "<eb>", "<ec>", 
                                "<ed>", "<ee>", "<ef>", "<f0>", "<f1>", "<f2>", "<f3>", "<f4>", 
                                "<f5>", "<f6>", "<f7>", "<f8>", "<f9>", "<fa>", "<fb>", "<fc>", 
                                "<fd>", "<fe>", "<ff>"), Actual = c(" ", "¡", "¢", "£", "¤", 
                                                                    "¥", "¦", "§", "¨", "©", "ª", "«", "¬", "SHY", "®", "¯", "°", 
                                                                    "±", "²", "³", "´", "µ", "¶", "·", "¸", "¹", "º", "»", "¼", "½", 
                                                                    "¾", "¿", "À", "Á", "Â", "Ã", "Ä", "Å", "Æ", "Ç", "È", "É", "Ê", 
                                                                    "Ë", "Ì", "Í", "Î", "Ï", "Ð", "Ñ", "Ò", "Ó", "Ô", "Õ", "Ö", "×", 
                                                                    "Ø", "Ù", "Ú", "Û", "Ü", "Ý", "Þ", "ß", "à", "á", "â", "ã", "ä", 
                                                                    "å", "æ", "ç", "è", "é", "ê", "ë", "ì", "í", "î", "ï", "ð", "ñ", 
                                                                    "ò", "ó", "ô", "õ", "ö", "÷", "ø", "ù", "ú", "û", "ü", "ý", "þ", 
                                                                    "ÿ")), row.names = c(NA, -96L), class = "data.frame")
DF$editor <- stringi::stri_replace_all_fixed(
  DF$editor, 
  ascii$Hex, 
  ascii$Actual, 
  vectorize_all = FALSE
)
DF$affiliation <- stringi::stri_replace_all_fixed(
  DF$affiliation, 
  ascii$Hex, 
  ascii$Actual, 
  vectorize_all = FALSE
)
DF$journal <- stringi::stri_replace_all_fixed(
  DF$journal, 
  ascii$Hex, 
  ascii$Actual, 
  vectorize_all = FALSE
)
DF$role <- stringi::stri_replace_all_fixed(
  DF$role, 
  ascii$Hex, 
  ascii$Actual, 
  vectorize_all = FALSE
)
# clean encoding (fix wrongful unicodes)
WRONG <- c("<U+0096>", "<U+0092>", "<U+0097>", "<U+00A0>", "<U+009A>",
           "<U+02BB>", "<U+009E>", "<U+0086>", "<U+0093>", "<U+0094>",
           "<U+1EC7>", "<U+00A8>", "<U+0450>", "<U+008A>", "<U+00AA>",
           "<U+0218>", "<U+0091>", "<U+00B4>", "<U+200B>", "<U+206F>",
           "<U+0219>", "<U+0091>", "<U+01FA>", "<U+039C>", "<U+00A1>",
           "<U+00B7>", "<U+021B>", "<U+FFFD>",  "<U+FB01>","<U+008E>",
           "<U+00AD>", "<U+00AE>", "<U+00AB>", "<U+00BB>", "<U+0099>",
           "<U+0203>", "<U+0392>", "<U+0421>", "<U+041C>", "<U+202A>",
           "<U+039A>", "<U+0441>", "<U+0410>", "<U+00A9>", "<U+200E>",
           "<U+00B3>", "<U+02BD>", "<U+1EA1>", "<U+1ECD>", "<U+1ED9>",
           "<U+04E7>", "<U+1ECC>", "<U+00A4>", "<U+0087>", "<U+E524>",
           "<U+00A2>", "<U+1EE7>", "<U+1EE9>", "<U+009C>", "<U+00BA>",
           "<U+0084>", "<U+00B2>", "<U+00B0>", "<U+021A>", "<U+03A4>",
           "<U+0391>", "<U+041E>", "<U+041F>", "<U+0413>", "<U+0422>",
           "<U+0423>", "<U+03C1>", "<U+039F>", "<U+0095>", "<U+03B3>",
           "<U+0384>", "<U+03BF>", "<U+03BD>", "<U+03C5>", "<U+2009>",
           "<U+00AC>", "<U+03CE>", "<U+03BA>", "<U+03B9>", "<U+202F>",
           "<U+00B8>", "<U+0412>", "<U+0430>")

RIGHT <- c("–", "'", "—", " ", "š",
           "ʻ", "ž", "†", "“", "”",
           "ệ", "ü", "è", "Š", "ª",
           "Ș", "'", "´", "", "",
           "ș", "'", "Ǻ", "M", "¡",
           "·", "ț", "", "fi", "Ž",
           "-", "®", "«", "»", "™",
           "ȃ", "B", "C", "M", "",
           "K", "c", "A", "©", "",
           "³", "ʽ", "ạ", "ọ", "ộ",
           "ö", "Ọ", "¤", "‡", " ",
           "¢", "ủ", "ứ", "œ", "°",
           " ", "²", "°", "Ț", "T",
           "A", "О", "П", "Г", "Т",
           "У", "ρ", "Ο", "•", "γ",
           "´", "ο", "ν", "υ", " ",
           "¬", "ώ", "κ", "ι", " ",
           "¸", "B", "a")

DF$editor <- stringi::stri_replace_all_fixed(
  DF$editor, 
  WRONG, 
  RIGHT, 
  vectorize_all = FALSE
)
DF$affiliation <- stringi::stri_replace_all_fixed(
  DF$affiliation, 
  WRONG, 
  RIGHT, 
  vectorize_all = FALSE
)
DF$role <- stringi::stri_replace_all_fixed(
  DF$role, 
  WRONG, 
  RIGHT, 
  vectorize_all = FALSE
)
# test how many editor names and affiliations
# still contain pointy brackets
ASCII <- DF[(grepl("<.*>", DF$editor) | grepl("<.*>", DF$affiliation)), ]

# save the dataframe
install.packages("arrow")
arrow::write_parquet(DF, "Output\\2022-Scraping\\editors.parquet")

# split the data into 2 so as to not exceed 100 MB per file
DF <- as.data.frame(DF)
editors1 <- DF[1:(nrow(DF)/2),]
editors2 <- DF[((nrow(DF)/2)+1):nrow(DF),]

write_tsv(editors1, "Output\\2022-Scraping\\editors1.tsv")
write_tsv(editors2, "Output\\2022-Scraping\\editors2.tsv")
write_csv(editors1, "Output\\2022-Scraping\\editors1.csv")
write_csv(editors2, "Output\\2022-Scraping\\editors2.csv")
