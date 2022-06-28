#Assignment 4

library(dplyr)

#Setting working directory
setwd("C:\\Users\\charg\\Documents\\MBiotech\\Biomed Comm\\Assignment 4")

#Read ufo file
read <- read.csv("ufo_subset.csv")
class(read)

#Cleaning rows that don't have shape or country info
new.data <- filter(read, country != "" & shape != "")
#Or do I convert the blanks to NA then do na.omit???

#Changing Datetime & Date_posted into appropriate formats
datetime <- as.Date(new.data$datetime)
date_posted <- as.Date(new.data$date.posted)
#Whats wrong with the format above???? I converted to date, but I don't have to do anything else

#Remove sightings from NUFORC
new.data2 <- new.data[!grepl("NUFORC", new.data$comments), ]

#Adding column report_delay to display difference in days between date of sighting and reported
#Converting dates from character to dates again
datetime2 <- as.Date(new.data2$datetime)
date_posted2 <- as.Date(new.data2$date.posted)
days_difference <- date_posted2 - datetime2
new.data3 <- new.data2 %>%
  mutate(reported_delay = days_difference)

#Filtering the rows where the sighting was reported before it happened
new.data4 <- filter(new.data3, reported_delay > 0) 

#Making table with avg report_delay per country
new.data5 <- new.data4 %>%
  group_by(country) %>%
  summarise(avg_reported_delay = mean(reported_delay))
new.data5

#Checking data quality of of duration(seconds) column
class(new.data4$duration..seconds.)
summary(new.data4$duration..seconds.)
which(new.data4$duration..seconds. == "")
table(new.data4$duration..seconds.)
IQR(new.data4$duration..seconds.)
new.data4$duration..seconds.[new.data4$duration..seconds. > 1432.5] <- NA
#What I did was understand what type of data we're dealing with by first doing a class function.
#I then followed by a summary function to understand the range, and to see if there's any missing
#values. I did the which function to see if there were any blanks. I noticed that there were large
#values that did not make sense, such as having 82800000 seconds on the duration of sighitng. 
#A sighting that goes over 2 year must have been a typo. Therefore, by using the IQR, I removed
#values that are outliers that are above Q3+1.5*IQR and below Q1-1.5*IQR. Therefore, to graphically 
#display a clearer graph, outliers were removed. 

#Histogram of seconds
hist(log(new.data4$duration..seconds.), xlab = "Seconds", main = "Log of Duration of sightings in 
     seconds")
