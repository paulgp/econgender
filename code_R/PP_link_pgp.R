##AEA in AER P&P Comparison

#Load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(fuzzyjoin)
library(stringdist)

#Create list of AEA data frames
AEAfiles <- list.files(path = "/Users/PSG24/repos/econgender/data/raw/", pattern = "output_\\d*.csv", full.names = T)


#Create table for AEA imports to be stored
AEAdata <- tibble()

#Import AEA data frames and organise them per year
##I should probably use the glue package
for(i in seq(1,length(AEAfiles))) {
  fn_AEA <- AEAfiles[i]
  year_match_AEA <- "output_(\\d+)\\.csv"
  year_AEA <- str_match(fn_AEA,year_match_AEA)
  print(as.numeric(year_AEA[2]))
  input_file_AEA<- read_csv(fn_AEA, col_types = cols(time = col_character())) %>% mutate(year= year_AEA[2])
  AEAdata <- AEAdata %>% bind_rows(input_file_AEA)
}

#Create list of AER P&P data frames
AERfiles <- list.files(path = "/Users/PSG24/repos/econgender/data/raw/", pattern = "\\d+PP.csv", full.names = T)

#Create table for AER P&P imports to be stored
AERdata <- tibble()

#Import AER P&P data frames and organise them per year
for(i in seq(1,length(AERfiles))) {
  fn_AER <- AERfiles[i]
  year_match_AER <- "(\\d+)PP.csv"
  year_AER <- str_match(fn_AER,year_match_AER)
  print(as.numeric(year_AER[,2]))
  input_file_AER<- read_csv(fn_AER) %>% mutate(year= year_AER[,2])
  AERdata <- AERdata %>% bind_rows(input_file_AER)
}


## THis is the data that we want to match onto with authors and sessions
AERdata_shape <- AERdata %>% mutate(author = str_replace_all(author, "[\\[|\'|\\]]", "")) %>% 
  separate(author, sep = ",", into=c("author1","author2","author3", "author4", "author5", "author6","author7", "author8", "author9")) %>%
  gather(author_num, author, -session_title, -paper_title, -year) %>%
  mutate(author = trimws(author)) %>% arrange( year, session_title, paper_title, author_num) %>%
  filter(!is.na(author))
  
  


#### First match on *sessions*
AER_sessions = AERdata_shape %>% select(session_title, year) %>% unique()
## There are 315 Sessions in total

AEA_sessions = AEAdata %>% select(session_title, year) %>% unique()
## There are 3794 potential sessions...

## First, exact matches.
exact_session_matches = AER_sessions %>% inner_join(AEA_sessions)
exact_session_matches_AER = AERdata_shape  %>% inner_join(AEAdata, by=c("session_title" = "session_title", "year"="year", "author" = "author"))
## That matches 150 sessions. 
remaining_AER_data = AERdata_shape %>% anti_join(exact_session_matches)
remaining_AEA_sessions = AEAdata %>% anti_join(exact_session_matches)
## Leaving 155 Sessions


### No exact matches on author year and session title
### so, match on author, year and then FUZZY match on session title
remaining_AER_data %>% select(author, author_num, year, session_title) %>% 
  inner_join(remaining_AEA_sessions, by=c("author"="author", "year"="year")) %>%
  mutate(session_distance = stringdist(session_title.x, session_title.y)) %>% 
  select(session_distance, everything()) %>%
  group_by(paper_title, year, author_num) %>% 
  arrange(session_distance) %>% 
  ungroup() %>%
  select(session_title.x, session_title.y, session_distance) %>% 
  unique() %>%
  group_by(session_title.x) %>% 
  arrange(session_title.x, session_distance) %>% 
  mutate(num_matches = n()) %>%
  filter(row_number() == 1) %>% view()
  


base = AERdata_shape %>% filter(year == "2011") %>% group_by(session_title, paper_title) %>%
  arrange(session_title, paper_title, author) %>%
  filter(row_number() == 1) 
#136 rows in 2011

a = AERdata_shape %>% filter(year == "2011") %>% 
  select(session_title, paper_title, author, year) %>%
  group_by(session_title, paper_title) %>%
  arrange(session_title, paper_title, author) %>%
  filter(author != "") %>%
  filter(row_number() == 1) %>%
  stringdist_left_join(AEA_merge, distance_col = "distance", by=c("session_title", "author"), max_dist=20) %>%
  ungroup() %>% 
  group_by(session_title.x, author.x) %>%
  arrange(session_title.x, author.x, paper_title.x, session_title.distance, author.distance) %>%
  filter(row_number() == 1) %>% 
  filter(session_title.distance < 3)
