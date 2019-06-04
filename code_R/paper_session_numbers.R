library(Hmisc)

pp_papers_sessions <- read.csv("Paper_Session_Number.csv")

plot(pp_papers_sessions$year, pp_papers_sessions$session_number, 
     type = "o", 
     col = "red", 
     lwd = 1,
     xlab = "Year",
     ylab = "Number of Sessions",
     main = "Number of Sessions Per Year In The AER P&P (2011-2019)")

plot(pp_papers_sessions$year, pp_papers_sessions$paper_number, 
     type = "o", 
     col = "red", 
     lwd = 1,
     xlab = "Year",
     ylab = "Number of Papers",
     main = "Number of Papers Per Year In The AER P&P (2011-2019)")
