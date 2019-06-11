
## You can put line breaks to make these more readable
select(count_authors_per_session_2019, number_of_authors_2019)%>% sum
library(tidyverse)
total_number_of_authors_all_years<-c(distinct(output_2019, author)%>%count(.), distinct(output_2018, author)%>%count(.), distinct(output_2016, author)%>%count(.), NA, distinct(output_2015, author)%>%count(.), distinct(output_2014, author)%>%count(.), distinct(output_2013, author)%>%count(.), distinct(output_2012, author)%>%count(.), distinct(output_2011, author)%>%count(.))
tibble(years, total_number_of_authors_all_years)
total_number_of_authors_all_years_tibble<-tibble(years, total_number_of_authors_all_years)
total_number_of_authors_all_years_tibble<-tibble(years, total_number_of_authors_all_years)
total_number_of_authors_all_years<-c(distinct(output_2019, author)%>%count(.), distinct(output_2018, author)%>%count(.), NA, distinct(output_2016, author)%>%count(.), distinct(output_2015, author)%>%count(.), distinct(output_2014, author)%>%count(.), distinct(output_2013, author)%>%count(.), distinct(output_2012, author)%>%count(.), distinct(output_2011, author)%>%count(.))
years<-(2011:2019)
total_number_of_authors_all_years_tibble<-tibble(years, total_number_of_authors_all_years)
ggplot(data=total_number_of_authors_all_years_tibble)+geom_point(aes(x=factor(years), y=as.numeric(total_number_of_authors_all_years)))+scale_x_discrete("years") + scale_y_continuous("number of authors") + ggtitle("Number of Authors Per Year in the AEA, 2011-2019")+geom_line(aes(x=factor(years), y=as.numeric(total_number_of_authors_all_years), group=1))
total_number_of_papers_all_years<-c(distinct(output_2011, paper_title)%>%count(.), distinct(output_2012, paper_title)%>%count(.), distinct(output_2013, paper_title)%>%count(.), distinct(output_2014, paper_title)%>%count(.), distinct(output_2015, paper_title)%>%count(.), distinct(output_2016, paper_title)%>%count(.), NA, distinct(output_2018, paper_title)%>%count(.), distinct(output_2019, paper_title)%>%count(.))
total_number_of_papers_all_years_tibbles<-tibble(years, total_number_of_papers_all_years)
ggplot(data=total_number_of_papers_all_years_tibbles)+geom_point(aes(x=factor(years), y=as.numeric(total_number_of_papers_all_years)))+scale_x_discrete("years") + scale_y_continuous("number of papers") + ggtitle("Number of Papers Per Year in the AEA, 2011-2019")+geom_line(aes(x=factor(years), y=as.numeric(total_number_of_papers_all_years), group=1))
