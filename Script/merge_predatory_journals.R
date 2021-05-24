library(tidyverse)
pred1 <- read_csv("Output\\AlliedAcademies\\AlliedAcademies-120.csv")
pred2 <- read_csv("Output\\iMedPub\\iMedPub-165.csv")
pred3 <- read_csv("Output\\Longdom\\Longdom-190.csv")
pred4 <- read_csv("Output\\SCIRP\\SCIRP-247.csv")
pred5 <- read_csv("Output\\SciTechnol\\SciTechnol-92.csv")

PRED <- do.call("rbind", list(pred1, pred2, pred3, pred4, pred5))
PRED$X1 <- NULL

PRED <- distinct(PRED)

write.csv(PRED, "Output\\editors_predatory_journals.csv", row.names = FALSE)

# basic data
nrow(PRED) # 19.257 editors
length(table(PRED$journal)) # 752
length(table(PRED$publisher)) # 5

# Allied Academies
# iMedPub
# Longdom
# SCIRP
# SciTechnol


# ============================
# Example: Find affiliations in Austria
# ============================
PRED[grepl("(Austria|Vienna|Wien|Graz|Innsbruck|Salzburg)", PRED$affiliation), ]
