library('dplyr') # --------------- for bind_rows() & select()
library('stringr') # ------------- for str_extract_all()

# obtain a dataframe of MDPI journals
journals <- read.csv("Data\\alljournals.csv")
journals <- journals[journals$publisher == "Cambridge University Press",]

# get journal abbreviations from the URLs
Abbr <- str_extract_all(journals$url, "(?<=/journals/).*")
Abbr <- unlist(Abbr)

# prepare the scraping process
EdList <- list()

source("Script\\geteditors.R")

for(i in 1:length(journals)) {
  
  editors_url <- paste0("https://www.cambridge.org/core/journals/"
                        , Abbr[i]
                        ,"/information/editorial-board")
  
  printtext <- paste(i, editors_url, sep=": ")
  print(printtext)
  
  EdList[[i]] <- get_editors(editors_url)
  
  Sys.sleep(11)
}

DF <- dplyr::bind_rows(EdList) %>%
  select(publisher, journal, role, editor, affiliation, url, date)

write.csv(DF, file = paste0("Output\\CUP-",i,".csv"))
