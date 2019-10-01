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
  filter(!is.na(author)) %>% 
  filter(author != "") %>%
  mutate(author = str_replace_all(author, '\"', "")) %>%
  mutate(author = trimws(author)) %>%
  mutate(paper_title = str_replace_all(paper_title, '\"', "")) %>%
  mutate(paper_title = trimws(paper_title))  %>%
  filter(!(session_title %in% c("Proceedings", "Proceedings: Reports", "Richard T. Ely Lecture", "Reports", "Contents")))

## clean up authors and sessions:
AEAdata = AEAdata %>% 
  filter(!is.na(author)) %>% 
  filter(author != "") %>%
  mutate(author = str_replace_all(author, '\"', "")) %>%
  mutate(author = trimws(author)) %>%
  mutate(paper_title = str_replace_all(paper_title, '\"', "")) %>%
  mutate(paper_title = trimws(paper_title))  %>%
  mutate(session_title = str_replace_all(session_title, '([A-Z][1-9], [A-Z][1-9])', "")) %>%
  mutate(session_title = str_replace_all(session_title, fixed("()"), ""))
  
## How many times is an author in the AER Data (in a given year)?
## Some people have more than one AER P&P in a given year.



## Basic stats on the AER P&Ps -- 1178 papers, relatively equal over time. :
AERdata_shape %>% select(paper_title, year) %>% unique() %>% tally()
AERdata_shape %>% select(paper_title, year) %>% unique() %>% group_by(year) %>% tally()

## First, we want to match up on sessions to find the selected "sessions"
AER_sessions = AERdata_shape %>% ungroup() %>% select(session_title, year) %>% filter(year != 2017) %>% unique()
## There are 257 Sessions in total

AEA_sessions = AEAdata %>% ungroup() %>% select(session_title, year) %>% unique()
## There are 3794 potential sessions...

## First, exact matches.
exact_session_matches = AER_sessions %>% inner_join(AEA_sessions)
exact_session_matches_AER = AERdata_shape  %>% inner_join(AEAdata, by=c("session_title" = "session_title", "year"="year", "author" = "author"))
## That matches 150 sessions. Need to then link the papers and authors within this. 
remaining_AER_data = AERdata_shape %>% anti_join(exact_session_matches)
remaining_AEA_data = AEAdata %>% anti_join(exact_session_matches)
remaining_AER_sessions = AER_sessions %>% anti_join(exact_session_matches)
remaining_AEA_sessions = AEA_sessions %>% anti_join(exact_session_matches)

## Leaving 155 Sessions
## How close are these sessions?
matched_sessions_string = remaining_AER_sessions %>% 
  left_join(AEA_sessions, by=c("year", "year")) %>%
  mutate(session_dist = stringdist(session_title.x, session_title.y)) %>%
  group_by(session_title.x) %>% arrange(session_dist) %>%
  filter(row_number() == 1) %>%
  mutate(session_dist = session_dist / str_length(session_title.x)) %>% ungroup() %>% arrange(session_dist) %>% 
  filter(year != 2017) %>% 
  filter(session_dist < .21) %>%
  rename(session_title.AER = session_title.x,
         session_title.AEA = session_title.y) %>%
  select(-session_dist)

## After string matches, leaving...
remaining_AER_sessions %>% 
  filter(year != 2017) %>% 
  anti_join(matched_sessions_string, by = c("session_title" = "session_title.AER")) %>%
  tally()
## 32 sessions out of 257 total

## Before worrying about the other 32, let's link up the sessions we do match.
## Use string matched cross walk to clean up AER data
remaining_AER_data_clean = AERdata_shape %>% left_join(matched_sessions_string, by=c("session_title" = "session_title.AER", "year")) %>%
  mutate(session_title = case_when(is.na(session_title.AEA) ~ session_title, 
                                   TRUE ~ session_title.AEA)) %>%
  select(-session_title.AEA)

## Now, redo the same exercise, but matching authors *within* sessions
linked_AER_data = remaining_AER_data_clean %>% 
  inner_join(AEAdata, by=c("session_title", "year")) %>%
  mutate(author.x.fn = word(author.x,1),
         author.x.ln = word(author.x,-1),
         author.y.fn = word(author.y,1),
         author.y.ln = word(author.y,-1)) %>%
  unite("author.x.fl", author.x.fn, author.x.ln, sep=" ") %>%
  unite("author.y.fl", author.y.fn, author.y.ln, sep=" ") %>%
  mutate(author_dist = stringdist(author.x.fl, author.y.fl) / str_length(author.x.fl)) %>%
  group_by(session_title, year, author.x) %>% 
  arrange(session_title, year, author.x, author_dist) %>% 
  filter(row_number() == 1)  %>% ungroup() %>% 
  select(author_dist, author.x, author.y, everything()) %>%
  arrange(year, session_title, paper_title.x, author.x) %>%
  group_by(session_title, year, paper_title.y, author.y) %>% 
  mutate(num_author_match_AEA = n()) %>%
  arrange(session_title, year, paper_title.y, author.y, author_dist) %>% 
  filter(row_number() == 1) %>%
  select(-num_author_match_AEA) %>%
  filter(author_dist < .5) %>%
  ungroup()

## Match it back to the AEA data to get the authors who didn't match
linked_AER_data_all = remaining_AER_data_clean %>% 
  left_join(linked_AER_data, 
            by= c("session_title", "year", "paper_title" = "paper_title.x", "author" = "author.x", "author_num")) %>%
  group_by(session_title) %>% mutate(num_matches_session = sum(!is.na(author.y))) %>% 
  select(num_matches_session, everything()) %>% filter(num_matches_session != 0)

## First match on year and paper titles, and then make sure we get the right authors
AERdata_shape %>% group_by(paper_title, year) %>% mutate(num_authors = n()) %>% ungroup() %>% 
  inner_join(AEAdata, 
             by = c("year", "paper_title")) %>%
  mutate(author_dist = stringdist(author.x, author.y)) %>%
  group_by(year, paper_title, author.x) %>% arrange(author.x, author_dist) %>%
  #filter(author_dist < 5) %>% 
  mutate(match = author_dist < 5) %>%
  group_by(year, paper_title) %>%
  mutate(num_matches = sum(match)) %>%
  group_by(year, paper_title, author.x) %>%
  filter(row_number() == 1) %>%
  filter(num_matches != num_authors) %>%
  select(author_dist, match, num_matches, num_authors, author.x, author.y) %>% arrange(paper_title, author.x) %>% view()


## Five distance ensures unique authors
## But, does it ensure all authors on paper are found?
## There are 80 papers where it doesn't get everyone...
## Turns out that there 




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
