
## A couple of comments:
## 1. Please put comments in describing what code chunks are supposed to do
## 2. If there are redundant lines of code, take them out
## 3. Put necessary "library" statements at the beginning
## 4. Rather than copying lines of code for each year, construct a single
##    file from the different years, or use a loop. THe current file is too repetitive.
## 5. Make sure to have the ".R" suffix on the file.
## Try redoing this file, but minimizing the amount of repetition, and writing comments
## to clarify the goals of each component.

## Put "library" statements at the beginning
library(readr)
output_2019 <- read_csv("data/output_2019.csv")

## You can put line breaks to make these more readable
authors_important_rows<-select(output_2019, author, session_title, session_jel_code)
count_authors_per_session<-count(authors_important_rows, session_title, session_jel_code)

## I'm not sure what these two lines of code are supposed to do
authors_per_session<- colnames(count_authors_per_session)[colnamescount_authors_per_session)=="n"] <- "number_of_authors"
colnames(count_authors_per_session)[colnamescount_authors_per_session)=="n"] <- "number_of_authors"

## Similarly here, what is this supposed to do?
authors_per_session<- colnames(count_authors_per_session)[colnames(count_authors_per_session)=="n"] <- "number_of_authors"

papers_important_rows<-select(output_2019, paper_title, session_title, session_jel_code)
papers_important_rows_distinct<-distinct(papers_important_rows)
count_papers_per_session<-count(papers_important_rows_distinct, session_title, session_jel_code)

## Similarly here, what is this supposed to do?
papers_per_session<- colnames(count_papers_per_session)[colnames(count_papers_per_session)=="n"] <- "number_of_papers"

output_2018 <- read_csv("data/output_2018.csv")
authors_important_rows_2019<-authors_important_rows
output_2016<-read.csv("data/output_2016.csv")
output_2015<-read.csv("data/output_2015.csv")
output_2014<-read.csv("data/output_2014.csv")
output_2013<-read.csv("data/output_2013.csv")
output_2012<-read.csv("data/output_2012.csv")
output_2011<-read.csv("data/output_2011.csv")
count_authors_per_session_2019<-count_authors_per_session
papers_important_rows_2019<-papers_important_rows

## Put "library" statements at the beginning of the file
library(dplyr)
authors_important_rows_2018<-select(output_2018, author, session_title, session_jel_code)
count_authors_per_session_2018<-count(authors_important_rows_2018, session_title, session_jel_code)
colnames(count_papers_per_session_2018)[colnames(count_papers_per_session_2018)=="n"] <- "number_of_papers"
colnames(count_authors_per_session_2018)[colnames(count_authors_per_session_2018)=="n"] <- "number_of_papers"
colnames(count_authors_per_session_2018)[colnames(count_authors_per_session_2018)=="number_of_papers"] <- "number_of_authors"
count_papers_per_session_2019<-count(papers_important_rows_2019, session_title, session_jel_code)
count_papers_per_session_2018<-count(papers_important_rows_2018, session_title, session_jel_code)
papers_important_rows_2018<-select(output_2018, paper_title, session_title, session_jel_code)
count_papers_per_session_2018<-count(papers_important_rows_2018, session_title, session_jel_code)
colnames(count_papers_per_session_2018)[colnames(count_papers_per_session_2018)=="n"] <- "number_of_papers"
authors_important_rows_2016<-select(output_2016, author, session_title, session_jel_code)
count_authors_per_session_2016<-count(authors_important_rows_2016, session_title, session_jel_code)
colnames(count_authors_per_session_2016)[colnames(count_authors_per_session_2016)=="n"] <- "number_of_authors"
select(output_2016, author, session_title, session_jel_code)%>% count_authors_per_session_2015<-count(session_title, session_jel_code)
> select(output_2016, author, session_title, session_jel_code)%>% count_authors_per_session_2015<-count(, session_title, session_jel_code)
select(output_2016, author, session_title, session_jel_code)%>% count_authors_per_session_2015<-count(, session_title, session_jel_code)
count_authors_per_session_2015<-count(select(output_2015, author, session_title, session_jel_code), session_title, session_jel_code)
library(tidyr)
separate(count_authors_per_session_2015, session_title, into=c("session_title", "code"), sep="[(]")
unite(separate(count_authors_per_session_2015, session_title, into=c("session_title", "code"), sep="[(]"), "session_jel_code1", c("code", "session_jel_code")))
unite(separate(count_authors_per_session_2015, session_title, into=c("session_title", "code"), sep="[(]"), "session_jel_code1", c("code", "session_jel_code"))
#issue with missing session_jel_code for some sessions; found in parentheses in session_title column#
count_authors_per_session_2014<-count(select(output_2014, author, session_title, session_jel_code), session_title, session_jel_code)
colnames(count_authors_per_session_2014)[colnames(count_authors_per_session_2014)=="n"] <- "number_of_authors"
colnames(count_authors_per_session_2015)[colnames(count_authors_per_session_2015)=="n"] <- "number_of_authors"
count_authors_per_session_2013<-count(select(output_2013, author, session_title, session_jel_code), session_title, session_jel_code)
colnames(count_authors_per_session_2013)[colnames(count_authors_per_session_2013)=="n"] <- "number_of_authors"
count_authors_per_session_2012<-count(select(output_2012, author, session_title, session_jel_code), session_title, session_jel_code)
olnames(count_authors_per_session_2012)[colnames(count_authors_per_session_2012)=="n"] <- "number_of_authors"
colnames(count_authors_per_session_2012)[colnames(count_authors_per_session_2012)=="n"] <- "number_of_authors"
count_authors_per_session_2011<-count(select(output_2011, author, session_title, session_jel_code), session_title, session_jel_code)
colnames(count_authors_per_session_2011)[colnames(count_authors_per_session_2011)=="n"] <- "number_of_authors"
View(count_papers_per_session_2019)
View(count_papers_per_session_2018)
count_papers_per_session_2016<-count(select(output_2016, session_title, session_jel_code), session_title, session_jel_code)
rm(count_papers_per_session_2016)
rm(count_papers_per_session_2018)
rm(count_papers_per_session_2019)
rm(authors_per_session)
rm(papers_per_session)
count(papers_important_rows_2019)
count(papers_important_rows_2019, session_jel_code)
count(papers_important_rows_2019, session_jel_code, session_title)
count(papers_important_rows_2019, session_title, session_jel_code)
count_papers_per_session_2019<-count(papers_important_rows_2019, session_title, session_jel_code)
papers_important_rows_distinct<-distinct(papers_important_rows)
papers_important_rows_distinct_2019<-distinct(papers_important_rows_2019)
papers_important_rows_distinct_2019<-distinct(papers_important_rows_2019)
count_papers_per_session_2019<-count(papers_important_rows_2019_distinct, session_title, session_jel_code)
count_papers_per_session_2019<-count(papers_important_rows_distinct_2019, session_title, session_jel_code)
colnames(count_papers_per_session_2019)[colnames(count_papers_per_session_2019)=="n"] <- "number_of_papers"
count_papers_per_session_2018<-count(distinct(select(output_2018, paper_title, session_title,session_jel_code), session_title, session_jel_code)
count_papers_per_session_2018<-count(distinct(select(output_2018, paper_title, session_title,session_jel_code), session_title, session_jel_code))
count_papers_per_session_2018<-count(distinct(select(output_2018, paper_title, session_title,session_jel_code)), session_title, session_jel_code))
count_papers_per_session_2018<-count(distinct(select(output_2018, paper_title, session_title,session_jel_code),) session_title, session_jel_code))
count_papers_per_session_2018<-count(distinct(select(output_2018, paper_title, session_title,session_jel_code)), session_title, session_jel_code)))
select(output_2018, paper_title, session_title,session_jel_code)%>%distinct
select(output_2018, paper_title, session_title,session_jel_code)%>%distinct%>%count(session_title, session_jel_code)
count_papers_per_session_2018<-select(output_2018, paper_title, session_title,session_jel_code)%>%distinct%>%count(session_title, session_jel_code)
colnames(count_papers_per_session_2018)[colnames(count_papers_per_session_2018)=="n"] <- "number_of_papers"
count_papers_per_session_2016<-select(output_2016, paper_title, session_title,session_jel_code)%>%distinct%>%count(session_title, session_jel_code)+colnames(count_papers_per_session_2016)[colnames(count_papers_per_session_2016)=="n"] <- "number_of_papers"
count_papers_per_session_2016<-select(output_2016, paper_title, session_title,session_jel_code)%>%distinct%>%count(session_title, session_jel_code)
colnames(count_papers_per_session_2016)[colnames(count_papers_per_session_2016)=="n"] <- "number_of_papers"
count_papers_per_session_2015<-select(output_2015, paper_title, session_title,session_jel_code)%>%distinct%>%count(session_title, session_jel_code)
colnames(count_papers_per_session_2015)[colnames(count_papers_per_session_2015)=="n"] <- "number_of_papers"
count_papers_per_session_2014<-select(output_2014, paper_title, session_title,session_jel_code)%>%distinct%>%count(session_title, session_jel_code)
colnames(count_papers_per_session_2014)[colnames(count_papers_per_session_2014)=="n"] <- "number_of_papers"
count_papers_per_session_2013<-select(output_2013, paper_title, session_title,session_jel_code)%>%distinct%>%count(session_title, session_jel_code)
colnames(count_papers_per_session_2013)[colnames(count_papers_per_session_2013)=="n"] <- "number_of_papers"
count_papers_per_session_2013<-select(output_2013, paper_title, session_title,session_jel_code)%>%distinct%>%count(session_title, session_jel_code)
count_papers_per_session_2012<-select(output_2012, paper_title, session_title,session_jel_code)%>%distinct%>%count(session_title, session_jel_code)
colnames(count_papers_per_session_2012)[colnames(count_papers_per_session_2012)=="n"] <- "number_of_papers"
count_papers_per_session_2011<-select(output_2011, paper_title, session_title,session_jel_code)%>%distinct%>%count(session_title, session_jel_code)
colnames(count_papers_per_session_2011)[colnames(count_papers_per_session_2011)=="n"] <- "number_of_papers"
full_join(count_authors_per_session_2019, count_papers_per_session_2019, by= c("session_jel_code", "session_title"))
count_per_session_2019<-full_join(count_authors_per_session_2019, count_papers_per_session_2019, by= c("session_jel_code", "session_title"))
count_per_session_2019<-full_join(count_authors_per_session_2018, count_papers_per_session_2018, by= c("session_jel_code", "session_title"))
count_per_session_2019<-full_join(count_authors_per_session_2019, count_papers_per_session_2019, by= c("session_jel_code", "session_title"))
count_per_session_2018<-full_join(count_authors_per_session_2018, count_papers_per_session_2018, by= c("session_jel_code", "session_title"))
count_per_session_2017<-full_join(count_authors_per_session_2017, count_papers_per_session_2019, by= c("session_jel_code", "session_title"))
count_per_session_2016<-full_join(count_authors_per_session_2016, count_papers_per_session_2016, by= c("session_jel_code", "session_title"))
count_per_session_2015<-full_join(count_authors_per_session_2015, count_papers_per_session_2015, by= c("session_jel_code", "session_title"))
count_per_session_2014<-full_join(count_authors_per_session_2014, count_papers_per_session_2014, by= c("session_jel_code", "session_title"))
count_per_session_2013<-full_join(count_authors_per_session_2013, count_papers_per_session_2013, by= c("session_jel_code", "session_title"))
count_per_session_2012<-full_join(count_authors_per_session_2012, count_papers_per_session_2012, by= c("session_jel_code", "session_title"))
count_per_session_2011<-full_join(count_authors_per_session_2011, count_papers_per_session_2011, by= c("session_jel_code", "session_title"))
write.csv(count_per_session_2011, "count_per_session_2011.csv")
write.csv(count_per_session_2012, "count_per_session_2012.csv")
write.csv(count_per_session_2013, "count_per_session_2013.csv")
write.csv(count_per_session_2014, "count_per_session_2014.csv")
write.csv(count_per_session_2015, "count_per_session_2015.csv")
write.csv(count_per_session_2016, "count_per_session_2016.csv")
write.csv(count_per_session_2018, "count_per_session_2018.csv")
write.csv(count_per_session_2019, "count_per_session_2019.csv")
