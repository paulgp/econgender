#install.packages("tidyverse")
require(tidyverse)
require(stringr)


## This code constructs an AEA dataset of names that are unidentified w/ gender
#Read relevant files 
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
  mutate(female = gender == "F") %>%
  mutate(male   = gender == "M")

