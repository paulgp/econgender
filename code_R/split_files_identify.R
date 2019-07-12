
set.seed(7)
library(tidyverse)

setwd("~/repos/econgender/data/")

files <- list.files(path = ".",
                    pattern = "unassigned_gender_\\d+.csv",
                    full.names = T)

##Create table for imports to be stored
data <- tibble()

##Import data frames and organise them per year
for(i in seq(1,length(files))) {
    fn <- files[i]
    year_match <- "unassigned_gender_(\\d+).csv"
    year <- str_match(fn,year_match)
    print(as.numeric(year[2]))
    input_file <- read.csv(fn) %>% mutate(year = year[2])
    data <- data %>% bind_rows(input_file)
    }


## split files into groups 

ss <- sample(1:4,size=nrow(data),
             replace=TRUE,
             prob=c(0.35,0.35,0.15, 0.15))
output.list <- setNames(split(data, ss),
                   c("kumsal","naasey","paul", "anusha"))

#write.csv(output.list$naasey, "modified/naasey_names.csv")
#write.csv(output.list$kumsal, "modified/kumsal_names.csv")
#write.csv(output.list$paul, "modified/paul_names.csv")
#write.csv(output.list$anusha, "modified/anusha_names.csv")



## Paul will take all four files        
## Merge and link them on
naasey_names_complete <- read_csv("~/repos/econgender/data/modified/naasey_names_complete.csv") %>%
    rename(paperid = X1) %>% select(-author)
anusha_names_complete <- read_csv("~/repos/econgender/data/modified/anusha_names_complete.csv") %>% 
    select(-X5) %>%
    rename(gender = Gender, paperid = X1) %>% select(-author)
paul_names_complete <- read_csv("~/repos/econgender/data/modified/paul_names_complete.csv") %>%
    rename(paperid = X1) %>% select(-author)
kumsal_names_complete <- read_csv("~/repos/econgender/data/modified/kumsal_names_complete.csv") %>%
    rename(gender = X4, paperid = X1) %>% 
    mutate(gender = replace(gender, gender == "Male", "M"),
           gender = replace(gender, gender == "Female", "F")) %>% select(-author)


full_list = bind_rows(list(output.list$anusha %>% mutate(paperid = row_number(), 
                                                  year = as.numeric(year)) %>%
                      left_join(anusha_names_complete) %>% select(-paperid),
                 output.list$naasey %>% mutate(paperid = row_number(), 
                                               year = as.numeric(year)) %>%
                     left_join(naasey_names_complete) %>% select(-paperid),
                 output.list$paul %>% mutate(paperid = row_number(), 
                                               year = as.numeric(year)) %>%
                     left_join(paul_names_complete) %>% select(-paperid),
                 output.list$kumsal %>% mutate(paperid = row_number(), 
                                             year = as.numeric(year)) %>%
                     left_join(kumsal_names_complete) %>% select(-paperid)
))

full_list = full_list %>% filter(gender != "M" | gender != "F")

write.csv(full_list, "modified/assigned_gender_all.csv")
