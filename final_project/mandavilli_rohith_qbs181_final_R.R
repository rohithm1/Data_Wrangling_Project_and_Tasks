## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache=FALSE)


## ------------------------------------------------------------------------
diastolics <- read.csv("IC_BP_V2.csv", header = TRUE)


## ------------------------------------------------------------------------
# Simply changing the name from 'BPAlerts' to 'BPStatus'
colnames(diastolics) [4] <- "BPStatus"
diastolics[sample(nrow(diastolics), 10), ]


## ------------------------------------------------------------------------
# Instead of using the default library and method to turning a data set into 1 hot encoding, I will manually do it because not every row value will need to be its own dummy variable

# Default set all the values to 0, then change all the Hypotension-1 & Normal to 1 to indicate they the patients with that condition have controlled blood pressure
diastolics$Controlled_blood_pressure <- 0
diastolics$Controlled_blood_pressure[diastolics$BPStatus=="Hypo1" | diastolics$BPStatus=="Normal"] <- 1

# Now that controlled_blood_pressure has the measurements of whether a patient has controlled or uncontrolled data, we should change it to the column name we need.

# First delete the current status column
diastolics[4] <- NULL

# Rename blood status column to alerts column
colnames(diastolics) [5] <- "BPStatus"
diastolics[sample(nrow(diastolics), 10), ]


## ------------------------------------------------------------------------
# Need to install the odbc package so i can grab data needed from SQL
library("RODBC")
library(dplyr)
myconn<-odbcConnect("qbs181","rmandavilli","rmandavilli@qbs181")

# This will grab the actual demographic data needed
Demographics<-sqlQuery(myconn,"select * from Demographics")

# Now merge the two tables
# The two tables common column is the ID for diastolics and contactid for Demographics
dem_diastolics <- merge(diastolics, Demographics, by.x = "ID", by.y = "contactid") 
dem_diastolics[sample(nrow(dem_diastolics), 10), ]


## ------------------------------------------------------------------------
# grab the enrollmentdate and 12 weeks after to get the 12 week interval we need
dem_diastolics$tri_imaginecareenrollmentemailsentdate <- as.Date(dem_diastolics$tri_imaginecareenrollmentemailsentdate, format = "%m/%d/%Y")
library(lubridate)
dem_diastolics$ObservedTime <- as.Date("1900-01-01") + days(dem_diastolics$ObservedTime)

# Now need to grab the chunk of data that is within the interval and put it back into the current data table
dem_diastolics <- subset(dem_diastolics, dem_diastolics$ObservedTime >= dem_diastolics$tri_imaginecareenrollmentemailsentdate & dem_diastolics$ObservedTime < dem_diastolics$tri_imaginecareenrollmentemailsentdate + 12*7)

# Based on the justification for how i assume which rows come first, need to sort each patient's data entry by their estimated time
dem_diastolics <- dem_diastolics[order(dem_diastolics$ID, dem_diastolics$ObservedTime), ]

# Create the total counts column to be able to get the average diastolic, systolic, and BPStatus values
library(plyr)
ID_counts <- ddply(dem_diastolics,.(ID),nrow)
colnames(ID_counts) [2] <- "total_id_counts"
dem_diastolics <- merge(dem_diastolics, ID_counts, by.x = "ID", by.y = "ID") 

# Get all the total counts for each diastolic value and merge with the current big table
dem_diastolics <- dem_diastolics[rowSums(is.na(dem_diastolics[1]))!=1,]

library(dplyr)
detach(package:plyr) #detatch to fix groupby error

total_counts_table <- dem_diastolics %>% 
    group_by(ID) %>% 
    summarise(AvgSys = sum(SystolicValue, na.rm=TRUE),
              AvgDia = sum(Diastolicvalue, na.rm=TRUE),
              AvgScore = sum(BPStatus, na.rm=TRUE))


dem_diastolics <- merge(dem_diastolics, total_counts_table, by.x = "ID", by.y = "ID") 
dem_diastolics$AvgDia <- dem_diastolics$AvgDia/dem_diastolics$total_id_counts
dem_diastolics$AvgSys <- dem_diastolics$AvgSys/dem_diastolics$total_id_counts
dem_diastolics$AvgScore <- dem_diastolics$AvgScore/dem_diastolics$total_id_counts

# After the average scores have been calculated - they are currently in decimal format, but use rounding to get generally whether someones blood pressure will be controlled or not
dem_diastolics$AvgScore[dem_diastolics$AvgScore>=0.5] <- 1
dem_diastolics$AvgScore[dem_diastolics$AvgScore<0.5] <- 0
dem_diastolics[sample(nrow(dem_diastolics), 10), ]


## ------------------------------------------------------------------------
# Filter the groups of ids by grabbing the first row and the last row
compare_list <- dem_diastolics %>%
                group_by(ID) %>%
                filter(row_number()==1 | row_number()==n())
compare_list[sample(nrow(compare_list), 10), ]


## ------------------------------------------------------------------------
# Filter the groups of ids by grabbing the first row and the last row
change_data <- dem_diastolics %>%
  group_by(ID) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  select(ID, BPStatus )

# Add a count variable that counts the order of the rows - makes transposing the data easier
change_data <- change_data %>% group_by(ID) %>% mutate(count = row_number())

# Transpose the data
library(tidyr)
change_data <- spread(change_data, count, BPStatus)

# Rename columns for clarity and remove missing values
colnames(change_data) [2] <- "first"
colnames(change_data) [3] <- "last"
change_data <- na.omit(change_data)

# Find the number of patients who moved from uncontrolled to controlled
sum(change_data$first == 0 & change_data$last == 1)
subset(change_data, change_data$first == 0 & change_data$last == 1)
change_data[sample(nrow(change_data), 10), ]


## ----pressure, echo=FALSE, fig.cap="Question 1a picture", out.width = '100%'----
knitr::include_graphics("question2part1.png")
knitr::include_graphics("question2part2.png")


## ------------------------------------------------------------------------
# First need to merge the tables
dem_cond <- sqlQuery(myconn,"select A.*, B.* from Demographics A
                             inner join Conditions B
                             on A.contactid=tri_patientid")
text_messages <- sqlQuery(myconn,"select * from TextMessages")

dem_cond_texts <- merge(dem_cond, text_messages, by.x = "contactid", by.y = "tri_contactId") 
dem_cond_texts$TextSentDate <- as.Date(dem_cond_texts$TextSentDate, '%m/%d/%y')

# Delete all duplicate rows with a text message date later than the most recent
library(dplyr)
dem_cond_texts <- dem_cond_texts %>% 
                  group_by(contactid) %>%
                  slice(which.max(TextSentDate))

#Install necessary packages and libraries to make one hot encoding possible
library(mltools)
library(haven)
library(data.table)

#Perform the function on the necessary columns to preserve the data
dem_cond_texts <- one_hot(setDT(dem_cond_texts, keep.rownames=TRUE, key=NULL, check.names=FALSE), cols = "tri_name", dropCols = TRUE, dropUnusedLevels = TRUE)
dem_cond_texts <- one_hot(setDT(dem_cond_texts, keep.rownames=TRUE, key=NULL, check.names=FALSE), cols = "SenderName", dropCols = TRUE, dropUnusedLevels = TRUE)
dem_cond_texts$tri_patientid <- NULL
dem_cond_texts$rn <- NULL

dem_cond_texts[sample(nrow(dem_cond_texts), 10), ]

