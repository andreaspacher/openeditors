library("tidyverse")
library("maps")

#--------------------------------------------------------------
# PART 1 Read Data
#-------------------------------------------------------------
editors1 <- readr::read_csv("Output/editors1_ror.csv")
editors2 <- readr::read_csv("Output/editors2_ror.csv")
editors <- bind_rows(editors1, editors2)

#--------------------------------------------------------------
# PART 2 Add Country Names (Based on the "maps"-Library)
#-------------------------------------------------------------
data(world.cities)

# gather all country names
# NOTE: the regex "\\b" is used to denote word boundaries,
# so that "Indiana" will not be coded as "India".
all_countries <- str_c(unique(world.cities$country.etc), collapse = "\\b|\\b")
all_countries <- paste0("\\b", all_countries)

# add additional country names
all_countries <- paste0(all_countries, "\\b|\\bUnited States")
all_countries <- paste0(all_countries, "\\b|\\bUnited Kingdom\\b")
all_countries <- paste0(all_countries, "\\b|\\bRussian Federation\\b")
all_countries <- paste0(all_countries, "\\b|\\bHong Kong\\b")
all_countries <- paste0(all_countries, "\\b|\\bKorea\\b")
all_countries <- paste0(all_countries, "\\b|\\bSerbia\\b")
all_countries <- paste0(all_countries, "\\b|\\bMontenegro\\b")
all_countries <- paste0(all_countries, "\\b|\\bCzechia\\b")
all_countries <- paste0(all_countries, "\\b|\\bCôte d'Ivoire\\b")
all_countries <- paste0(all_countries, "\\b|\\bMacau\\b")
all_countries <- paste0(all_countries, "\\b|\\bSyrian Arab Republic\\b")

# create a new column in the Open Editors-dataset with the country names
editors$country <- str_extract_all(editors$affiliation,
                                   all_countries)

# replace "character(0)" with NA
editors <- editors %>%
  mutate(country = na_if(country, "character(0)"))
  
# check how many are empty
# (or how many country names could not be detected)?
empties <- sum(is.na(editors$country))
empties / nrow(editors)*100
NAS <- editors[is.na(editors$country),]
# RESULT: 32.552 rows (out of 478.512) without country names, or 6.8%

#--------------------------------------------------------------
# PART 3 Analyze the Frequency of Countries
#-------------------------------------------------------------

editorscountries <- unlist(editors$country)
editorscountries <- data.frame(table(editorscountries))
editorscountries <- editorscountries[rev(order(editorscountries$Freq)),]
colnames(editorscountries) <- c("country", "Freq")

COUNTRY1 <- c("United States", "United Kingdom", "Czechia", "Russian Federation")
COUNTRY2 <- c("USA", "UK", "Czech Republic", "Russia")
length(unique(editorscountries$country))
editorscountries$country <- stringi::stri_replace_all_fixed(
  editorscountries$country, 
  COUNTRY1, 
  COUNTRY2, 
  vectorize_all = FALSE
)
editorscountries <- editorscountries %>%
  group_by(country) %>%
  mutate(Freq = sum(Freq)) %>%
  arrange(desc(Freq)) %>%
  distinct()

#--------------------------------------------------------------
# PART 4 Add Continent Names (ERROR)
#-------------------------------------------------------------
library("countrycode")
editors <- as.data.frame(editors)
editors$country <- as.character(editors$country)
editors$continent <- countrycode(sourcevar = editors$country,
                                 origin = "country.name",
                                 destination = "continent")

#--------------------------------------------------------------
# PART 4 Save data
#-------------------------------------------------------------

# in case of multiple country names in the "country"-column,
# separate the values by comma,
# otherwise write_csv later leads to the error message:
# "Error: Flat files can't store the list column `country`"
editors <- editors %>% mutate(country = map_chr(country, toString))

disamb_country <- function(x) {
  x <- if(grepl("^c\\(\\\"", x)) {
    x <- gsub("^.{0,3}", "", x)
    x <- gsub(".{2}$", "", x)
    x <- gsub("\"", "", x)
  } else {
    x
  }
  return(x)
}
editors$country <- sapply(editors$country, function(x) disamb_country(x))

# rearrange columns
editors <- editors %>%
  select(publisher, 
         issn, 
         journal,
         role,
         editor,
         affiliation,
         country,
         continent,
         ror,
         url,
         date)

# split & save the files
editors1_ror_c <- slice_head(editors, n=nrow(editors1))
editors2_ror_c <- slice_tail(editors, n=nrow(editors2))

write_csv(editors1_ror_c, "Output/editors1_ror_and_countries.csv")
write_csv(editors2_ror_c, "Output/editors2_ror_and_countries.csv")
