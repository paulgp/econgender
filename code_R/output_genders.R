
install.packages("tidyverse")

library (tidyverse)

output <- read.csv("output_2019.csv")

nber <- read.csv("nber_master_gender.csv")

facebook <- read.csv("firstname_gender_facebook.csv")

assigned <- read.csv("assigned_gender_all.csv")

nber_new <- nber%>%select(author, gender)

assigned_new <- assigned%>%select(author, gender)

combined <- rbind(nber_new, assigned_new)

output_new <- output%>%select(author)

new <- merge(output_new, combined)

unassigned_names <- data.frame(author= setdiff(output_new$author, combined$author), row.names = NULL)

unassigned_names$first_name <- stringr::word(unassigned_names$author)

final <- merge(unassigned_names, facebook)

final_new <- final%>%select(author, gender)

final_new_unique <- unique(final_new) 

new_unique <- unique(new)

all <- rbind(final_new_unique, new_unique)

write.table(all, file="output_2019_authors_gender.csv", row.names=F, sep=",")




