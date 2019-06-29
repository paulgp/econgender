install.packages("stringr")

library(stringr)

output <- read.csv("output_2011.csv")

nber <- read.csv("nber_master_gender.csv")

facebook <- read.csv("firstname_gender_facebook.csv")

output_all_names <- data.frame(author = unlist(output[c(2,10,12,14,16,18,20)]), row.names = NULL)

names_after_nber <- data.frame(author= setdiff(output$author, nber$author), row.names = NULL)

final <- names_after_nber[!word(str_to_upper(names_after_nber$author), 1) %in% facebook$firstname,, drop = FALSE]

write.table(final, file="unassigned_gender_2011.csv", row.names=F, sep=",")

