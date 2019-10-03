
##AEA in AER P&P Comparison

#Load relevant libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

#Create list of AEA data frames
AEAfiles <- list.files(path = "/Users/PSG24/repos/econgender/data/raw/", pattern = "output_\\d*.csv", full.names = T)

#Create table for AEA imports to be stored
AEAdata <- tibble()

#Import AEA data frames and organise them per year
for(i in seq(1,length(AEAfiles))) {
  fn_AEA <- AEAfiles[i]
  year_match_AEA <- "output_(\\d+)\\.csv"
  year_AEA <- str_match(fn_AEA,year_match_AEA)
  print(as.numeric(year_AEA[2]))
  input_file_AEA<- read_csv(fn_AEA, col_types = cols(time = col_character())) %>% mutate(year= year_AEA[2])
  AEAdata <- AEAdata %>% bind_rows(input_file_AEA)
}

#Create list of AER P&P data frames
  AERfiles <- list.files(path = "/Users/PSG24/repos/econgender/data/raw/", pattern = "\\d*_P&P_Papers.csv", full.names = T)
  
#Create table for AER P&P imports to be stored
  AERdata <- tibble()
  
#Import AER P&P data frames and organise them per year
  for(i in seq(1,length(AERfiles))) {
    fn_AER <- AERfiles[i]
    year_match_AER <- "(\\d+)_P&P_Papers.csv"
    year_AER <- str_match(fn_AER,year_match_AER)
    print(as.numeric(year_AER[2]))
    input_file_AER<- read_csv(fn_AER, col_types = cols(time = col_character())) %>% mutate(year= year_AER[2])
    AERdata <- AERdata %>% bind_rows(input_file_AER)
    }

#Creating linked dataframe with P&P indicator 
AEAlinked <- 
  AEAdata %>%
  left_join(AERdata %>% transmute(author, paper_title, PandP = 'yes')) %>%
  replace_na(list(PandP = 'no'))

#Export linked dataframe with P&P indicator
write.csv(AEAlinked, "AEAlinked.csv")


##Checking that (almost) all records in AER P&P are in AEA
#Create linked data
AERlinked<-
  AERdata %>%
  left_join(AEAdata %>% transmute(author, paper_title, year, AEA = 'yes')) %>%
  replace_na(list(AEA = 'no'))

#Compute percent of P&P in AEA
percent_link<-(tally(AERlinked, AEA=='yes')/tally(AERlinked)*100) %>% as.numeric
###21% for now?
###Should be close to 100%
###Yet to verify this value

##Compute share of AEA in P&P
sharePandP<-(tally(AEAlinked, PandP=='yes')/tally(AEAlinked)*100)%>%as.numeric



