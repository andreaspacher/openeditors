# read data
editors1 <- read.csv("Output\\editors1.csv", fileEncoding = "UTF-8")
editors2 <- read.csv("Output\\editors2.csv", fileEncoding = "UTF-8")
editors <- rbind(editors1, editors2)

# remove duplicates
doubles <- duplicated(editors[, c("editor", "journal", "publisher")], )
doubles_df <- editors[doubles, ] # take a look at the duplicates
editors <- editors[!doubles, ]

# remove rows where publisher is NA
editors <- editors[!is.na(editors$publisher), ]

# check whether there are rows where both editor and affiliation are NA
nas_df <- editors[which(is.na(editors$editor) & is.na(editors$affiliation)), ] # 0

# perhaps remove editors who are NA
editors <- editors[!with(editors, is.na(editor)), ]

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
           "<U+00B3>", "<U+02BD>", "<U+1EA1>", "<U+1ECD>", "<U+1ED9>")

RIGHT <- c("–", "'", "—", " ", "š",
           "ʻ", "ž", "†", "“", "”",
           "ệ", "ü", "è", "Š", "ª",
           "Ș", "'", "´", "", "",
           "ș", "'", "Ǻ", "M", "¡",
           "·", "ț", "", "fi", "Ž",
           "-", "®", "«", "»", "™",
           "ȃ", "B", "C", "M", "",
           "K", "c", "A", "©", "",
           "³", "ʽ", "ạ", "ọ", "ộ")

editors$editor <- stringi::stri_replace_all_fixed(
  editors$editor, 
  WRONG, 
  RIGHT, 
  vectorize_all = FALSE
)
editors$affiliation <- stringi::stri_replace_all_fixed(
  editors$affiliation, 
  WRONG, 
  RIGHT, 
  vectorize_all = FALSE
)

# test how many editor names and affiliations
# still contain pointy brackets
ASCII <- editors[(grepl("<.*>", editors$editor) | grepl("<.*>", editors$affiliation)), ]

# save the cleaned data
editors1 <- editors[1:nrow(editors1),]
startrow <- nrow(editors1)+1
editors2 <- editors[startrow:nrow(editors),]

write.csv(editors1, "Output\\editors1.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv(editors2, "Output\\editors2.csv", fileEncoding = "UTF-8", row.names = FALSE)
