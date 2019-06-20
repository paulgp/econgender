
##PGP NOTE: Please end your file names with a ".R" extension, rather than leaving it blank
##PGP NOTE: You should be able to run this code with a "source()" command.
##          If I run the code below, it does not work, because it is copy and pasted
##          (The way you can tell is because of the "+" symbols



##Author and paper per session count tables

##Load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

##Create list of data frames
files <- list.files(path = "/Users/naaseyarthur/Documents/trial", pattern = "*.csv", full.names = T)

##Create table for imports to be stored
data <- tibble()

##Import data frames and organise them per year
## PGP note: delete the + signs
for(i in seq(1,length(files))) {
    fn <- files[i]
    year_match <- "output_(\\d+)\\.csv"
    year <- str_match(fn,year_match)
    print(as.numeric(year[2]))
    input_file <- read_csv(fn, col_types = cols(time = col_character())) %>% mutate(year = year[2])
    data <- data %>% bind_rows(input_file)
}

##Count papers per session
##PGP note: put spaces between your <- and %>% , and probably don't need to put the select statement on a different line
count_papers_per_session <- select(data, paper_title, session_title, year) %>%
    distinct %>%
    count(session_title, year)

##Rename "n" column to "number_of_papers"
## PGP note: this works, but can also use the tidy rename() command
colnames(count_papers_per_session)[colnames(count_papers_per_session)=="n"] <- "number_of_papers"

#Count authors per session
count_authors_per_session <-  select(data, author, session_title, year) %>%
    distinct() %>%
    count(session_title, year)

# #Rename "n" column to "number_of_authors"
## PGP note: this works, but can also use the tidy rename() command
colnames(count_authors_per_session)[colnames(count_authors_per_session)=="n"] <- "number_of_authors"

#Create final tables of author and paper per session counts
count_per_session<-full_join(count_authors_per_session, count_papers_per_session)

#Export table
write_csv(count_per_session, "count_per_session.csv")

##Total author and paper count graphs

#Count total number of authors for all years, adding missing 2017 value
total_number_of_authors <- distinct(data, author, year) %>%
  count(year)%>%
  rbind(c("2017", "NA"))

#Rename "n" column to "number_of_authors"
colnames(total_number_of_authors)[colnames(total_number_of_authors)=="n"] <- "number_of_authors"

#Count total number of papers for all years, adding missing 2017 value
total_number_of_papers<-distinct(data, paper_title, year)%>%
  count(year)%>%
  rbind(c("2017", "NA"))

#Rename "n" column to "number_of_papers"
colnames(total_number_of_papers)[colnames(total_number_of_papers)=="n"] <- "number_of_papers"

#Create graph for number of authors per year
ggplot(total_number_of_authors, mapping=aes(x=factor(year), y=as.numeric(number_of_authors)))+
  geom_point()+
  geom_line(group=1)+
  scale_x_discrete("years") + 
  scale_y_continuous("number of authors") + 
  ggtitle("Number of Authors Per Year in the AEA, 2011-2019")

#Create graph for number of papers per year
ggplot(total_number_of_papers, mapping=aes(x=factor(year), y=as.numeric(number_of_papers)))+
  geom_point()+
  geom_line(group=1)+
  scale_x_discrete("years") + 
  scale_y_continuous("number of papers") + 
  ggtitle("Number of Papers Per Year in the AEA, 2011-2019")
