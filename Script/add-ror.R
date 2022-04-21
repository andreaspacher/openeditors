library(httr)
library(jsonlite)
library(tidyverse)

#--------------------------------------------------------------
# PART 0 Define functions
#--------------------------------------------------------------

#Information on ROR API: 
#https://github.com/ror-community/ror-api

# The ROR API provides affiliation matching, which returns both 
# a matching confidence score ('score'), with values between 0 and 1 (inclusive)
# and a binary indicator ('chosen') of whether the score is high enough 
# to consider the organization correctly matched

# For this script, only ROR IDs are included for which chosen = 1
# Further improvements could be made by evaluating ROR IDs 
# for which chosen = 0 but with high values for 'score'

# define function to query ROR API with affiliation, extract ROR ID
getROR <- function(affiliation){
  url <- paste0("https://api.ror.org/organizations?affiliation=",
                URLencode(affiliation))
  raw_data <- GET(url)
  rd <- httr::content(raw_data)
  
  id <- keep(rd$items, ~.$chosen) %>%
    pluck(1, "organization", "id", .default = NA)
  
  res <- list(
    affiliation = affiliation,
    ror = id)
  
  return(res)
  
  Sys.sleep(0.1)
}


#define function to add progress bar
getROR_progress <- function(x){
  pb$tick()$print()
  res <- getROR(x)
  
  return(res)
}

#--------------------------------------------------------------
# PART 1 Read data
#--------------------------------------------------------------

# read data
# NB readr reads and writes data as UTF-8 by default, unless locale() is specified)
editors1 <- read_tsv("Output/2022-Scraping/editors1.tsv")
editors2 <- read_tsv("Output/2022-Scraping/editors2.tsv")
editors <- bind_rows(editors1, editors2)

# NB Script/clean_data_final uses read.csv from base R
#editors1 <- read.csv("Output/editors1.csv", fileEncoding = "UTF-8")
#editors2 <- read.csv("Output/editors2.csv", fileEncoding = "UTF-8")
#editors <- rbind(editors1, editors2)


#--------------------------------------------------------------
# PART 2 Clean encoding in affiliation
#--------------------------------------------------------------

# The original scripts already contain steps to fix encoding issues,
# but unfortunately, opening the saved csv-files on my system still resulted in encoding errors 
# reuse code from Script/clean-final-data.R
# use base R for this part of the script for consistency with Script/clean-final-data

# select affiliation column
editors_aff <- editors[, "affiliation", drop = FALSE]

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



editors_aff$affiliation_clean <- stringi::stri_replace_all_fixed(
  editors_aff$affiliation, 
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


editors_aff$affiliation_clean <- stringi::stri_replace_all_fixed(
  editors_aff$affiliation_clean, 
  WRONG, 
  RIGHT, 
  vectorize_all = FALSE
)

# test how many affiliations still contain pointy brackets
ASCII <- editors_aff[(grepl("<.*>", editors_aff$affiliation_clean)), ]
rm(ASCII)
# remove all tags in column affiliation_clean
# this is only for purposes of matching of affiliation strings to ROR IDs
# use dplyr and stringr from tidyverse for own efficiency
editors_aff <- editors_aff %>%
  mutate(affiliation_clean = str_remove_all(affiliation_clean, "<.*?>"),
         affiliation_clean = str_squish(affiliation_clean))

#--------------------------------------------------------------
# PART 3 Query ROR API
#--------------------------------------------------------------

#create character vector with unique cleaned affiliations
aff <- editors_aff %>%
  pull(affiliation_clean) %>%
  unique()

# query ROR API
# This takes about 1,5 hour per 10,000 records - could also split in batches

#set parameter for progress bar
pb <- progress_estimated(length(aff))
#map through list of affiliations, query ROR API for each
ror <- map_dfr(aff, getROR_progress)

#check how many unique affiliations are mapped to ROR
check <- ror %>%
  filter(!is.na(ror)) %>%
  nrow()
#105015 of 141915 affiliations mapped to ROR ID

#join ror data to dataframe with both original and cleaned affiliations
affiliations_ror <- editors_aff %>%
  left_join(ror, by = c("affiliation_clean" = "affiliation"))

#--------------------------------------------------------------
# PART 4 Add ROR data to full editor data
#-------------------------------------------------------------

editors_ror <- editors %>%
  left_join(affiliations_ror, by = "affiliation", na_matches = "never") %>%
  #only keep columns of interest
  select(publisher, 
         issn, 
         journal,
         role,
         editor,
         affiliation,
         ror,
         url,
         date)

#check for how many editors affiliations are mapped to ROR
check <- editors_ror %>%
  filter(!is.na(ror)) %>%
  nrow()
#394107 of 478512  (82%) editors have affiliations mapped to ROR ID


# save the data
editors1_ror <- slice_head(editors_ror, n=nrow(editors1))
editors2_ror <- slice_tail(editors_ror, n=nrow(editors2))

write_csv(editors1_ror, "Output/2022-Scraping/editors1_ror.csv")
write_csv(editors2_ror, "Output/2022-Scraping/editors2_ror.csv")
  
