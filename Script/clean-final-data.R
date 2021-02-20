# read data
editors <- read.csv("Output\\editors.csv", header = TRUE)

# remove automatically created numeric columns
editors$X <- NULL

# remove duplicates
doubles <- duplicated(editors[, c("editor", "journal", "publisher")], )
doubles_df <- editors[doubles, ] # take a look at the duplicates
editors <- editors[!doubles, ]

# remove rows where publisher is NA
editors <- editors[!is.na(editors$publisher), ]

# check whether there are rows where both editor and affiliation are NA
nas_df <- editors[which(is.na(editors$editor) & is.na(editors$affiliation)), ] # 0
editors <- editors[!with(editors, is.na(editor) & is.na(affiliation)), ]

# save the cleaned data
write.csv(editors, "Output\\editors.csv")