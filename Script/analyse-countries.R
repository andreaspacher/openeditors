library("tidyverse")
library("maps")

#--------------------------------------------------------------
# PART 1 Read Data
#-------------------------------------------------------------
editors <- arrow::read_parquet("Output\\2022-Scraping\\editors.parquet")

# editors <- editors %>%
#   filter(publisher == "Allied Academies" | publisher == "Longdom" | publisher == "SCIRP" | publisher == "SciTechnol")

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
all_countries <- paste0(all_countries, "\\b|\\bCÃ´te d'Ivoire\\b")
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

COUNTRY1 <- c("United States", "United Kingdom", "Czechia", "Russian Federation",
              "Falkland Islands", "Isle of Man", "Saint Helena", "Gibraltar",
              "US Virgin Islands", "Jersey",
              "Sicily",
              "Guadeloupe", "New Caledonia", "French Guiana", "French Polynesia", "Reunion", "Martinique",
              "Azores", "Madeira",
              "Canary Islands",
              "Syrian Arab Republic",
              "Faroe Islands", "Greenland",
              "Hong Kong SAR China",
              "South Korea", "Macao SAR China",
              "Bosnia & Herzegovina",
              "Myanmar (Burma)",
              "Congo - Kinshasa",
              "Puerto Rico",
              "Georgia" #perhaps controversial to replace it with USA?
)
COUNTRY2 <- c("USA", "UK", "Czech Republic", "Russia",
              "UK", "UK", "UK", "UK",
              "USA", "USA",
              "Italy",
              "France", "France", "France", "France", "France", "France",
              "Portugal", "Portugal",
              "Spain",
              "Syria",
              "Denmark", "Denmark",
              "Hong Kong",
              "Korea", "Macau",
              "Bosnia and Herzegovina",
              "Myanmar",
              "DR Congo",
              "USA",
              "USA")
editors$country <- stringi::stri_replace_all_fixed(
  editors$country,
  COUNTRY1,
  COUNTRY2,
  vectorize_all = FALSE
)
editors <- editors %>%
  mutate(country = ifelse(grepl("Tbilisi|Kutaisi", affiliation), "Georgia", country))

#--------------------------------------------------------------
# PART 2b: Analyze countries per journal?
#--------------------------------------------------------------
# function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# nr of countries per journal
SUM <- editors %>%
  unnest(country)

SUM2 <- aggregate(country ~ journal+publisher, SUM, paste, collapse = ";", na.action = na.omit)
SUM2$country <- sapply(strsplit(SUM2$country, ";", fixed = TRUE), function(x)
  paste(x, collapse = ";"))
colnames(SUM2) <- c("journal", "publisher", "all_countries")
SUM2$all_countries <- stringr::str_remove_all(SUM2$all_countries, "(NA;|NA$)")
SUM2$all_countries <- stringr::str_replace_all(SUM2$all_countries, "\\\", \\\"", ";")
SUM2$all_countries <- stringr::str_remove_all(SUM2$all_countries, "c\\(\\\"")
SUM2$all_countries <- stringr::str_remove_all(SUM2$all_countries, "\\\"\\)")
SUM2$short_countries <- sapply(strsplit(SUM2$all_countries, ";", fixed = TRUE), function(x)
  paste(unique(x), collapse = ";"))

CC <- SUM2 %>% select(publisher, "country" = all_countries)
CC <- CC %>% separate_rows(country, sep = ";")

# =============
# intermezzo: predatory publishers
# =============
PRED <- CC %>%
  filter(grepl("Allied Acad|iMedPub|Longdom|SCIRP|SciTec", publisher)) %>%
  group_by(country) %>%
  summarise(Freq.pred = n()) %>%
  arrange(desc(Freq.pred)) %>%
  mutate(pred_share = Freq.pred / sum(Freq.pred) * 100)
PRED$pred_rank <- seq.int(nrow(PRED))
NOPRED <- CC %>%
  filter(!grepl("Allied Acad|iMedPub|Longdom|SCIRP|SciTec", publisher)) %>%
  group_by(country) %>%
  summarise(Freq.nopred = n()) %>%
  arrange(desc(Freq.nopred)) %>%
  mutate(nopred_share = Freq.nopred / sum(Freq.nopred) * 100)
NOPRED$nopred_rank <- seq.int(nrow(NOPRED))
ALLPRED <- full_join(PRED, NOPRED)
cor.test(ALLPRED$Freq.pred, ALLPRED$Freq.nopred)
cor.test(ALLPRED$pred_share, ALLPRED$nopred_share)
cor.test(ALLPRED$pred_rank, ALLPRED$nopred_rank)
library(reshape2)
PREDMELT <- melt(ALLPRED %>% select(-starts_with("Freq")),
                 id.vars = c("country", "pred_rank", "nopred_rank"))
PREDMELT <- PREDMELT %>%
  mutate(value = ifelse(variable == "nopred_share", 0 - value, value))
library(forcats)
# create two-sided barchart
PREDMELT %>%
  filter(pred_rank < 31) %>%
  #mutate(variable = factor(variable, levels = c("ed_pct", "wb_pct"))) %>%
  ggplot(aes(x = reorder(country, -pred_rank),
             y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "identity") +
  xlab("") +
  ylab("") +
  scale_fill_manual(labels = c("share of editorships in predatory journals",
                               "share of editorships in non-predatory journals"),
                    values = c("black", "grey")) +
  scale_y_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30),
                    labels = c("30%", "20%", "10%", "0", "10%", "20%", "30%"),
                    limits = c(-35, 35)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave("Graph\\predatory_countries.png",
       width = 5,
       height = 5,
       units = "in",
       dpi = 300)

# SUM <- SUM2 %>%
#   mutate(nr_countries = as.numeric(unlist(stringi::stri_split(all_countries, fixed=';'))))
library(data.table)
SUM2$nr_countries <- sapply(strsplit(SUM2$short_countries,';'), uniqueN)
SUM2$nr_countries_all <- sapply(strsplit(SUM2$all_countries,';'), length)

# Quality Check... ok, not too bad actually
SUM2 %>%
  group_by(journal, publisher) %>%
  mutate(dataqual = ifelse(nr_countries == 0, TRUE, FALSE)) %>%
  ungroup() %>%
  group_by(publisher) %>%
  summarise(notgood = sum(dataqual))
# add that column to SUM2, just in case
SUM2 <- SUM2 %>%
  group_by(journal, publisher) %>%
  mutate(dataqual = ifelse(nr_countries == 0, TRUE, FALSE)) %>%
  ungroup()

SUM2 %>%
  #filter(nr_countries != 0) %>%
  group_by(publisher) %>%
  summarise(mean = round(mean(nr_countries), 1),
            median = median(nr_countries),
            mode = Mode(nr_countries),
            sd = round(sd(nr_countries),1),
            min = min(nr_countries),
            q1 = quantile(nr_countries, probs = 0.25),
            q3 = quantile(nr_countries, probs = 0.75),
            max = max(nr_countries)) %>%
  View()

# nr of journals with US editors
SUM2 %>%
  mutate(has_us = ifelse(grepl("USA", all_countries), 1, 0)) %>%
  group_by(has_us) %>%
  summarise(n = n())
# ^ 6.310 have US editors, 929 do not have any US editors?
# (sample = 7.045 journals... where we have data about countries)

# add as column:
SUM2 <- SUM2 %>%
  mutate(has_us = ifelse(grepl("USA", all_countries), 1, 0),
         us_count = stringr::str_count(all_countries, "USA"),
         us_share = round((us_count / nr_countries_all)*100, 1),
         
         has_ca = ifelse(grepl("Canada", all_countries), 1, 0),
         ca_count = stringr::str_count(all_countries, "Canada"),
         ca_share = round((ca_count / nr_countries_all)*100, 1),
         
         has_uk = ifelse(grepl("UK", all_countries), 1, 0),
         uk_count = stringr::str_count(all_countries, "UK"),
         uk_share = round((uk_count / nr_countries_all)*100, 1),
         
         has_an = ifelse(grepl("Australia|New Zealand", all_countries), 1, 0),
         an_count = stringr::str_count(all_countries, "Australia|New Zealand"),
         an_share = round((an_count / nr_countries_all)*100, 1),
         
         angloamer_count = us_count + ca_count + uk_count + an_count,
         angloamer_share = us_share + ca_share + uk_share + an_share
  )

SUM2 %>%
  group_by(publisher) %>%
  summarise(mean_aa = mean(angloamer_share, na.rm = T),
            mean_us = mean(us_share, na.rm = T),
            mean_ca = mean(ca_share, na.rm = T),
            mean_uk = mean(uk_share, na.rm = T),
            mean_an = mean(an_share, na.rm = T)) %>%
  arrange(desc(mean_aa))

SUM2 %>%
  group_by(publisher) %>%
  summarise(mean = round(mean(angloamer_share, na.rm = T), 1),
            median = median(angloamer_share, na.rm = T),
            mode = Mode(angloamer_share),
            sd = round(sd(angloamer_share, na.rm = T),1),
            min = min(angloamer_share, na.rm = T),
            q1 = quantile(angloamer_share, probs = 0.25, na.rm = T),
            q3 = quantile(angloamer_share, probs = 0.75, na.rm = T),
            max = max(angloamer_share, na.rm = T)) %>%
  View()

# boxplot --- share of angloamerican editors
SUM2 %>%
  mutate(publisher = case_when(
    publisher == "American Psychological Association" ~ "APA",
    publisher == "Cambridge University Press" ~ "Cambridge UP",
    publisher == "Royal Society of Chemistry" ~ "RSC",
    publisher == "American Society of Civil Engineers" ~ "ASCE",
    TRUE ~ publisher
  )) %>%
  filter(nr_countries != 0) %>%
  ggplot(aes(x = angloamer_share, y = reorder(publisher, angloamer_share, FUN = median))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  ylab("") +
  xlab("Share (in %) of Angloamerican editors per journal") +
  theme(axis.title = element_text(size = 9))

ggsave("Graph\\angloamerican.png",
       width = 5,
       height = 4.5,
       units = "in",
       dpi = 300)

CCC <- SUM2 %>%
  select(publisher, all_countries) %>%
  separate_rows(all_countries, sep = ";")
CCC2 <- CCC %>%
  filter(all_countries != "") %>%
  group_by(publisher) %>%
  mutate(all_ed = n()) %>%
  group_by(publisher, all_countries) %>%
  summarise(n = n(),
            share = round(n / all_ed * 100, 1)) %>%
  arrange(desc(n)) %>%
  distinct()
CCRANK <- CCC2 %>%
  group_by(publisher) %>%
  slice(1:5)
CCRANK %>%
  group_by(all_countries) %>%
  summarise(n = n(),
            share = round(n/26*100, 1)) %>%
  arrange(desc(n))

TOP5 <- CCRANK %>%
  mutate(all_countries = paste0(all_countries, " -- ", n, " (", share, "%)")) %>%
  mutate(rank = seq(1:5)) %>%
  select(-n, -share) %>%
  pivot_wider(names_from = rank, values_from = all_countries)

write_tsv(TOP5, "top5.txt")

# SUM.US <- SUM2 %>%
#   filter(nr_countries != 0) %>%
#   mutate(
#     nr_countries = ifelse(short_countries == "USA;Georgia", 1, nr_countries),
#     nr_countries = ifelse(short_countries == "USA;Puerto Rico", 1, nr_countries),
#     nr_countries = ifelse(short_countries == "Puerto Rico;USA", 1, nr_countries),
#     nr_countries = ifelse(short_countries == "Georgia;USA", 1, nr_countries),
#    
#     short_countries = ifelse(short_countries == "USA;Georgia", "USA", short_countries),
#     short_countries = ifelse(short_countries == "USA;Puerto Rico", "USA", short_countries),
#     short_countries = ifelse(short_countries == "Puerto Rico;USA", "USA", short_countries),
#     short_countries = ifelse(short_countries == "Georgia;USA", "USA", short_countries),
#   ) #%>%
#   # mutate(has_us = ifelse(grepl("USA", all_countries), 1, 0),
#   #        nr_countries = ifelse(nr_countries == 1, 1, 2))
# SUM.US %>%
#   group_by(has_us, nr_countries) %>%
#   summarise(n = n())
# so, in total, 116 journals with only US editors.. not too many!
# which ones are these?
# SUM.US %>%
#   filter(nr_countries == 1 & grepl("USA", all_countries))

# boxplot --- nr of per journal countries
SUM2 %>%
  mutate(publisher = case_when(
    publisher == "American Psychological Association" ~ "APA",
    publisher == "Cambridge University Press" ~ "Cambridge UP",
    publisher == "Royal Society of Chemistry" ~ "RSC",
    publisher == "American Society of Civil Engineers" ~ "ASCE",
    TRUE ~ publisher
  )) %>%
  filter(nr_countries != 0) %>%
  ggplot(aes(x = nr_countries, y = reorder(publisher, nr_countries, FUN = median))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  ylab("") +
  xlab("Number of countries per journal") +
  theme(axis.title = element_text(size = 9)) +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130))

ggsave("Graph\\nr_countries_per_j.png",
       width = 5,
       height = 4.5,
       units = "in",
       dpi = 300)

#coord_cartesian(xlim = c(0,130)) +
#scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130)) +
#ylab("") +
#theme_bw()
MEDIAN <- SUM2 %>%
  group_by(publisher) %>%
  summarise(median = median(nr_countries),
            mean = mean(nr_countries))
median(MEDIAN$median)
mean(MEDIAN$mean)
median(SUM2$nr_countries)
#--------------------------------------------------------------
# PART 3 Analyze the Frequency of Countries
#-------------------------------------------------------------

editorscountries <- unlist(editors$country)
editorscountries <- data.frame(table(editorscountries))
editorscountries <- editorscountries[rev(order(editorscountries$Freq)),]
colnames(editorscountries) <- c("country", "Freq")

#length(unique(editorscountries$country))

editorscountries <- editorscountries %>%
  group_by(country) %>%
  mutate(Freq = sum(Freq)) %>%
  arrange(desc(Freq)) %>%
  distinct()

editorscountries$rank <- seq.int(nrow(editorscountries))
editorscountries <- editorscountries %>%
  select(rank, country, Freq)
write_tsv(editorscountries, "Output\\country-stats.txt")
#-------------------------------------------------------------
# Compare with World Bank Population numbers
#-------------------------------------------------------------
wb <- world_bank_pop %>%
  filter(indicator == "SP.POP.TOTL") %>%
  select(country, `2017`) %>%
  arrange(desc(`2017`))
wb$wb_rank <- seq.int(nrow(wb))
library(countrycode)
wb$countryname <- countrycode(wb$country, "wb", "country.name")
wb$countryname <- stringi::stri_replace_all_fixed(
  wb$countryname,
  COUNTRY1,
  COUNTRY2,
  vectorize_all = FALSE
)
wb <- wb %>%
  filter(!is.na(countryname))
wb$wb_rank <- seq.int(nrow(wb))
ccc <- full_join(wb, editorscountries, by = c("countryname" = "country")) %>%
  arrange(desc(Freq))
ccc.first <- ccc[match(unique(ccc$countryname), ccc$countryname),]

# KOR = South Korea
# TWN = Taiwan
# HKG = Hong Kong, China
# MAC = Macao

# add percentages
ccc.first$ed_pct <- ccc.first$Freq / nrow(editors) * 100
ccc.first$ed_pct <- round(ccc.first$ed_pct, 1)

totalpop <- world_bank_pop %>% filter(country == "WLD" & indicator == "SP.POP.TOTL") %>% select(`2017`) %>% unlist() %>% unname()
ccc.first$wb_pct <- ccc.first$`2017` / totalpop * 100
ccc.first$wb_pct <- round(ccc.first$wb_pct, 1)

ccc.first <- ccc.first %>%
  select(country, countryname, wb_rank, wb_pct, "wb_pop"=`2017`, "ed_rank" = rank, ed_pct, "ed_nr" = Freq)
ccc.first <- ccc.first %>%
  mutate(wb_pop = ifelse(countryname == "Taiwan", 23674546, wb_pop))
ccc.first$wb_pct <- ccc.first$wb_pop / totalpop * 100
ccc.first$wb_pct <- round(ccc.first$wb_pct, 1)
ccc.first <- ccc.first %>%
  arrange(desc(wb_pop)) %>%
  mutate(wb_rank = seq.int(nrow(ccc.first)))

library(reshape2)
cccp <- melt(ccc.first %>% select(countryname, wb_rank, ed_rank, wb_pct, ed_pct),
             id.vars = c("countryname", "wb_rank", "ed_rank"))
cccp <- cccp %>%
  mutate(value = ifelse(variable == "ed_pct", 0 - value, value))
library(forcats)
# create two-sided barchart
cccp %>%
  filter(ed_rank < 31) %>%
  mutate(variable = factor(variable, levels = c("ed_pct", "wb_pct"))) %>%
  ggplot(aes(x = reorder(countryname, -ed_rank),
             y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "identity") +
  xlab("") +
  ylab("") +
  scale_fill_manual(labels = c("share of editorships", "share of global population"),
                    values = c("black", "grey")) +
  scale_y_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30),
                     labels = c("30%", "20%", "10%", "0", "10%", "20%", "30%"),
                     limits = c(-30, 30)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
ggsave("Graph\\countries.png",
       width = 5,
       height = 5,
       units = "in",
       dpi = 300)

cccp %>%
  filter(wb_rank < 31) %>%
  mutate(variable = factor(variable, levels = c("ed_pct", "wb_pct"))) %>%
  ggplot(aes(x = reorder(countryname, -wb_rank), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "identity") +
  xlab("") +
  ylab("") +
  scale_fill_manual(labels = c("share of editorships", "share of global population"),
                    values = c("black", "grey")) +
  scale_y_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30),
                     labels = c("30%", "20%", "10%", "0", "10%", "20%", "30%"),
                     limits = c(-30, 30)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())


# use kendall's tau test (b/c distributions are not normal)
cor.test(ccc.first$wb_pop, ccc.first$ed_nr,
         method = "kendall",
         # remove NAs
         use = "complete.obs")
# strong correlation!
#-------------------------------------------------------------
# PART 4 Add Continent Names (ERROR)
#-------------------------------------------------------------
editors <- as.data.frame(editors)
editors$country <- as.character(editors$country)

iso2c_to_continents <- c(USA = "North America" , Canada = "North America")

editors$country <- stringi::stri_replace_all_fixed(
  editors$country,
  COUNTRY1,
  COUNTRY2,
  vectorize_all = FALSE
)

editors$continent <- countrycode(sourcevar = editors$country,
                                 origin = "country.name",
                                 destination = "continent",
                                 custom_match = iso2c_to_continents)
wb$continent <- countrycode(sourcevar = wb$countryname,
                            origin = "country.name",
                            destination = "continent",
                            custom_match = iso2c_to_continents)

## examine after Tweet in Dec. 2022:
# wb %>% group_by(continent) %>% summarise(n = sum(`2017`, na.rm = T)) %>% distinct() %>% arrange(desc(n))

wb2 <- wb %>%
  group_by(continent) %>%
  summarise(n = sum(`2017`, na.rm = T)) %>%
  distinct() %>%
  arrange(desc(n)) %>%
  mutate(percent = n/7501739318) # sum(wb$`2017`, na.rm=T)

ct.ed <- janitor::tabyl(editors$continent) %>%
  mutate(percent = percent*100,
         percent = round(percent, 1)) %>%
  arrange(desc(percent))
ct.wb <- wb2 %>%
  mutate(percent = percent*100,
         percent = round(percent, 1)) %>%
  arrange(desc(percent))
colnames(ct.ed) <- c("continent", "ed_freq", "ed_pct", "ed_valid_pct")
colnames(ct.wb) <- c("continent", "wb_freq", "wb_pct")
cont <- full_join(ct.ed, ct.wb, by = "continent") %>%
  filter(!is.na(ed_valid_pct))
cont <- cont %>%
  select(continent, ed_pct, wb_pct)

cont <- melt(cont, id.vars = c("continent"))
cont <- cont %>%
  mutate(value = ifelse(variable == "ed_pct", 0 - value, value))
# create two-sided barchart
cont %>%
  mutate(
    continent = ifelse(continent == "Americas", "Latin America", continent),
    continent = factor(continent,
                       levels = c("Africa", "Oceania", "Latin America", "Asia", "North America", "Europe"))
    #variable = factor(variable, levels = c("ed_pct", "wb_pct"))
  ) %>%
  ggplot(aes(x = continent,
             y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "identity") +
  xlab("") +
  ylab("") +
  scale_fill_manual(labels = c("share of editorships", "share of global population"),
                    values = c("black", "grey")) +
  scale_y_continuous(breaks = c(-60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60),
                     labels = c("60%", "50%", "40%", "30%", "20%", "10%", "0", "10%", "20%", "30%", "40%", "50%", "60%"),
                     limits = c(-60, 60)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
ggsave("Graph\\continents-corr.png",
       width = 5,
       height = 2,
       units = "in",
       dpi = 300)
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

#remove consecutive duplicates
zz <- strsplit(editors2$country,",")
zz <- sapply(zz,function(x) rle(x)$value)

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