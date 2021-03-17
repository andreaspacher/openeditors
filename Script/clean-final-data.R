# read data
editors <- read.csv("Output\\editors.csv", header = TRUE, fileEncoding = "UTF-8")

# remove duplicates
doubles <- duplicated(editors[, c("editor", "journal", "publisher")], )
doubles_df <- editors[doubles, ] # take a look at the duplicates
editors <- editors[!doubles, ]

# remove rows where publisher is NA
editors <- editors[!is.na(editors$publisher), ]

# check whether there are rows where both editor and affiliation are NA
nas_df <- editors[which(is.na(editors$editor) & is.na(editors$affiliation)), ] # 0
editors <- editors[!with(editors, is.na(editor) & is.na(affiliation)), ]

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

editors$editor <- stringi::stri_replace_all_fixed(
  editors$editor, 
  ascii$Hex, 
  ascii$Actual, 
  vectorize_all = FALSE
)
editors$affiliation <- stringi::stri_replace_all_fixed(
  editors$affiliation, 
  ascii$Hex, 
  ascii$Actual, 
  vectorize_all = FALSE
)
editors$journal <- stringi::stri_replace_all_fixed(
  editors$journal, 
  ascii$Hex, 
  ascii$Actual, 
  vectorize_all = FALSE
)

# save the cleaned data
write.csv(editors, "Output\\editors.csv", fileEncoding = "UTF-8", row.names = FALSE)
