install.packages("tidyverse")
library (tidyverse)

#Read releveant files 
output <- read.csv("output_2019.csv")
nber <- read.csv("nber_master_gender.csv")
facebook <- read.csv("firstname_gender_facebook.csv")
assigned <- read.csv("assigned_gender_all.csv")

#Select author and gender columns
nber_new <- nber%>%select(author, gender)
assigned_new <- assigned%>%select(author, gender)

#Add these data frames to each other vertically
combined <- rbind(nber_new, assigned_new)

#Select the author column
output_new <- output%>%select(author)

#Merge two data frames by common elements
new <- merge(output_new, combined)

#Find unique names found in output_new and not in combined  
unassigned_names <- data.frame(author= setdiff(output_new$author, combined$author), row.names = NULL)

#Add another column named first_name which contains first names
unassigned_names$first_name <- stringr::word(unassigned_names$author)

#Merge two data frames by common elements
final <- merge(unassigned_names, facebook)

#Select author and gender columns
final_new <- final%>%select(author, gender)

#Remove duplicates 
final_new_unique <- unique(final_new) 
new_unique <- unique(new)

#Add these data frames to each other vertically
all <- rbind(final_new_unique, new_unique)

#Write the csv file
write.table(all, file="output_2019_authors_gender.csv", row.names=F, sep=",")




