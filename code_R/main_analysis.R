
require(tidyverse)
require(stringr)
require(readxl)
dblue = rgb(0,114,178,maxColorValue = 255)
dred = rgb(213,94,0,maxColorValue = 255)

### Gender Time Series Comparison
load_aea_data <- function() {
  ## This code constructs an AEA dataset of names that are unidentified w/ gender
  # Read relevant files 
  files <- list.files(path = "/Users/PSG24/repos/econgender/data/raw",
                      pattern = "output_\\d+.csv",
                      full.names = T)
  ##Create table for imports to be stored
  data <- tibble()
  
  ##Import AEA  data frames and organise them per year
  for(i in seq(1,length(files))) {
    fn <- files[i]
    year_match <- "/Users/PSG24/repos/econgender/data/raw/output_(\\d+).csv"
    year <- str_match(fn,year_match)
    print(as.numeric(year[2]))
    input_file <- read.csv(fn) %>% mutate(year = year[2])
    data <- data %>% bind_rows(input_file)
  }

  aea_output     <- data
  nber_names     <- read.csv("/Users/PSG24/repos/econgender/data/nber_master_gender.csv")
  facebook_names <- read.csv("/Users/PSG24/repos/econgender/data/firstname_gender_facebook.csv") %>%
    mutate(firstname = str_to_title(firstname) )
  assigned_names <- read.csv("/Users/PSG24/repos/econgender/data/modified/assigned_gender_all.csv")
  assigned_names_anusha <- read_excel("~/repos/econgender/data/modified/unassigned_names_anusha_complete.xlsx",
                                      col_names = FALSE, na = "?") %>% select(-...3) %>%
    rename(author = ...1, gender = ...2)
  assigned_names_paul <- read_csv("~/repos/econgender/data/modified/unassigned_names_paul_complete.csv")
  #Select author and gender columns
  nber_new       <- nber_names %>% select(author, gender) %>% 
    mutate(author = str_to_title(author) ) 
  assigned_new   <- assigned_names %>% select(author, gender) %>% 
    bind_rows(assigned_names_anusha) %>%
    bind_rows(assigned_names_paul) %>%
    mutate(female = gender == "F") %>%
    mutate(male   = gender == "M") %>%
    mutate(author = str_to_title(author) ) %>% unique()
  
  assigned_names %>% group_by(gender) %>% tally()
  
  ## USE FACEBOOK
  aea_output_new <- aea_output %>% 
    mutate(author = str_to_title(author)) %>%
    mutate(firstname = stringr::word(author)) %>%
    left_join(facebook_names) %>%
    mutate(gender_facebook = case_when(prob_female > 0.95 ~ "F",
                                       prob_male   > 0.95 ~ "M",
                                       TRUE               ~ ""))  %>% 
    select(gender_facebook, everything())
  
  ##Add these data frames to each other vertically
  combined_names <- bind_rows(nber_new, assigned_new) %>%
    mutate(gender = replace(gender, gender == "Male", "M")) %>%
    mutate(gender = replace(gender, gender == "Female", "F")) %>%
    mutate(gender = replace(gender, gender == "??", NA)) %>%
    mutate(gender = replace(gender, gender == "?", NA)) %>%
    mutate(gender = replace(gender, gender == "Unknown", NA)) %>%
    filter(!is.na(gender)) %>%
    mutate(female = gender == "F") %>%
    mutate(male   = gender == "M") %>% 
    unique()
  
  aea_output_final = aea_output_new %>% 
    left_join(combined_names) %>% 
    mutate(gender = replace(gender, gender_facebook == "F" & is.na(gender), "F"),
           gender = replace(gender, gender_facebook == "M" & is.na(gender), "M")) %>%
    select(-gender_facebook) %>%
    mutate(female = case_when(gender == "F" ~ 1,
                              gender == "M" ~ 0)) %>%
    mutate(female = case_when(gender == "M" ~ 1,
                              gender == "F" ~ 0)) %>%
  
  return(aea_output_final)
}

make_paper_session_pp_link = function() {
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
  
  return(linked_AER_data_all)
}

load_nber_data = function() {
  nber_data     <- read_dta("~/repos/econgender/data/modified/data_master.dta")
  return(nber_data)
}

aea_data = load_aea_data()

aer_pp_linked = make_paper_session_pp_link()

nber_data = load_nber_data()

aer_pp_linked_papers = aer_pp_linked %>% 
  select(paper_title.y, session_title, year) %>% unique() %>% filter(!is.na(paper_title.y))

d1 = aea_data %>% inner_join(aer_pp_linked_papers, by=c("paper_title" = "paper_title.y", "session_title", "year")) %>%
  mutate(aer_pp = TRUE)

aea_data.pp = bind_rows(d1, anti_join(aea_data, aer_pp_linked_papers, by=c("paper_title" = "paper_title.y", "session_title", "year")) %>% 
                          mutate(aer_pp = FALSE))


## Gender by Year 
yearly_gender.nber = nber_data %>%
  filter(role == "author") %>%
  mutate(female = case_when(gender == "Female" ~ 1,
                            gender == "Male" ~ 0)) %>% 
  group_by(year) %>% summarize(share_female = mean(female, na.rm=TRUE)) %>%
  mutate(group = "NBER")

yearly_gender.aea = aea_data.pp %>%
  group_by(year) %>% summarize(share_female = mean(female, na.rm=TRUE)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(group = "AEA")

yearly_gender = bind_rows(yearly_gender.nber, yearly_gender.aea)

ggplot(data = yearly_gender, aes(y = share_female, x = year, fill = group)) + 
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c(dblue, dred),
                     labels = c("AEA", "NBER")) +
  scale_y_continuous(expand = c(0,0), limits=c(0,0.3)) +
  theme_classic() +
  theme(text = element_text(size=24)) +
  theme(text = element_text(size=12),
        legend.position = c(0.25, 0.95),
        legend.justification = c("right", "top")) +
  labs(
    y = "",
    x = "Year",
    fill = "Conference"
  )


## Gender by group by Year
nber_data %>%
  filter(role == "author") %>%
  mutate(female = case_when(gender == "Female" ~ 1,
                            gender == "Male" ~ 0)) %>% 
  select(year, code, paper_title, author, female, field ) %>% group_by(field, year) %>% tally()

aea_data.pp %>% 
  select(year, session_jel_code, paper_title, author, female ) %>% group_by(session_jel_code) %>% tally() %>% view()


           