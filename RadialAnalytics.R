#Name: Neeraj Prasad
#Date: 10/17/2017
#Radial Analytics Data Science Coding Challenge

#Dependencies
#install.packages("dplyr")
library(dplyr)

## Read in Data
filename <- "Hospital General Information.csv"
myData <- read.csv(filename, header = TRUE, colClasses = "character")

## Pre-process Data
myData$Hospital.overall.rating[myData$Hospital.overall.rating == "Not Available"] <- NA 

##Split Data
myData$county_state <- paste(myData$County.Name,", ",myData$State, sep="")
modData <- myData %>% group_by(county_state)

##Summarize factors
output <- summarise(modData,  
                    numHospitals = n(), 
                    pct_private_non_profit = round((sum(Hospital.Ownership == "Voluntary non-profit - Private")/n())*100, digits=2), 
                    num_acute_care_hospitals = (sum(Hospital.Type == "Acute Care Hospitals")),
                    avg_acute_care_rating = round(mean(as.numeric(Hospital.overall.rating[(Hospital.Type == "Acute Care Hospitals")]), na.rm = T), digits=2),
                    median_acute_care_rating = median(as.numeric(Hospital.overall.rating[(Hospital.Type == "Acute Care Hospitals")]), na.rm = T))

## Arrange data
outcome <- arrange(output, desc(numHospitals))
write.csv(outcome, file="hospitals_by_county.csv")

#Extra Credit

##Pre-process Data
mnc <- sapply(myData$Mortality.national.comparison, switch, 
                                               "Same as the national average" = 3, 
                                               "Below the national average" = 1, 
                                               "Above the national average" = 5, 
                                               "Not Available" = NA)
rnc <- sapply(myData$Readmission.national.comparison, switch, 
                                               "Same as the national average" = 3, 
                                               "Below the national average" = 1, 
                                               "Above the national average" = 5, 
                                               "Not Available" = NA)

## p-values < 0.05 in both cases. Thus, there is definitive correlation between samples
cor.test(as.numeric(myData$Hospital.overall.rating), as.numeric(mnc), method = "pearson", use = "complete.obs")
cor.test(as.numeric(myData$Hospital.overall.rating), as.numeric(rnc), method = "pearson", use = "complete.obs")
