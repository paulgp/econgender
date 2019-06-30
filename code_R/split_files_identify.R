
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

write.csv(output.list$naasey, "modified/naasey_names.csv")
write.csv(output.list$kumsal, "modified/kumsal_names.csv")
write.csv(output.list$paul, "modified/paul_names.csv")
write.csv(output.list$anusha, "modified/anusha_names.csv")
