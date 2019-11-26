#First, I have to connect to the SQL server to access the databases I need to finish the project. In addition, I install the relevant packages required to complete the problem set.
library("RODBC")
library(sqldf)
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
myconn<-odbcConnect("qbs181","rmandavilli","rmandavilli@qbs181")

#Then, I Have to merge PhoneCall and PhoneCall_Encounter together to be able to edit PhoneCall and access codes

PhoneCalls <- sqlQuery(myconn,"select A.*, B.* from PhoneCall A
                                 inner join PhoneCall_Encounter B
                                 on A.tri_CustomerIDEntityReference=B.CustomerId")

#Next, to be able to manipulate the columns, I create a new column and instantiate it as NULL
PhoneCalls$EnrollmentGroup <-NA

#Based on question one, edit PhoneCalls so that EnrollmentGroup is updated based on the EncounterCodes
PhoneCalls <- PhoneCalls%>%
  mutate(EnrollmentGroup= case_when(PhoneCalls$EncounterCode == 125060000 ~ "Clinical Alert",
                                    PhoneCalls$EncounterCode == 125060001 ~ "Health Coaching",
                                    PhoneCalls$EncounterCode == 125060002 ~ "Technical Question",
                                    PhoneCalls$EncounterCode == 125060003 ~ "Administrative",
                                    PhoneCalls$EncounterCode == 125060004 ~ "Other",
                                    PhoneCalls$EncounterCode == 125060005 ~ "Lack of engagement"))

## Question 2 - need to count up all the instances in which the 'EnrollmentGroup' values occur
#the count() function exists in the dplyr library, meaning you just need to call the function on the column to get all the instances a value appears
PhoneCalls%>% count(EnrollmentGroup)

## Question 3 - need to merge the Phone Call Encounters with the Call Duration table now
#very similar to the first step in problem 1, just use SQL to merge the two tables and plcae it into a Encounter_Duration table
PhoneCall_Duration <- sqlQuery(myconn,"select A.*, B.* from PhoneCall_encounter A
                                 inner join CallDuration B
                                 on A.CustomerID=B.tri_CustomerIDEntityReference")

## Question 4 - need to find out the # of records for different call outcomes and call type
#Use 1-Inbound and 2-Outbound, fort call types; use 1-No response,2-Left voice mail and 3 successful. Please also find the call duration for each of the enrollment groups 

#Here I'll make the call label, which is descriptive of whether the call was inbound or outbound

#Make a new column to label the calls
PhoneCall_Duration$CallLabel <- NA

#Set the label to inbound or outbound dependent on the calltype
PhoneCall_Duration <- PhoneCall_Duration%>%
  mutate(CallLabel= case_when(PhoneCall_Duration$CallType == 1 ~ "Inbound",
                              PhoneCall_Duration$CallType == 2 ~ "Outbound",))

#Here I'll make the call outcome, which details what happened as a result of the call
#Make a new column to label the call outcomes
PhoneCall_Duration$CallOutcomeLabel <- NA

#Set the label to no response, left voice mail, and successful dependent on the calltype
PhoneCall_Duration <- PhoneCall_Duration%>%
  mutate(CallOutcomeLabel= case_when(PhoneCall_Duration$CallOutcome == 1 ~ "No response",
                                     PhoneCall_Duration$CallOutcome == 2 ~ "Left voice mail",
                                     PhoneCall_Duration$CallOutcome == 3 ~ "Successful"))

## Question 5 - need to merge Demographics, Conditions and TextMessages, Find the # of texts/per week, by the type of sender, then make a visual plot to obtain # of texts and color it by the type of sender

#First start off with merging all the tables
#Merging Demographics and Conditions
Demographics_Conditions_TextMessages <- sqlQuery(myconn,"select A.*, B.* from Demographics A
                                                         inner join Conditions B
                                                         on A.contactid=B.tri_patientid")

#Merging Demographics_Conditions_TextMessages with Textmessages
TextMessages <- sqlQuery(myconn,"select * from TextMessages")
Demographics_Conditions_TextMessages <- merge(Demographics_Conditions_TextMessages, TextMessages, by.x = "contactid", by.y = "tri_contactId") 

#Now, we have to find the number of texts sent per week

#First thing we will do is convert the column from type factor to type date
Demographics_Conditions_TextMessages$TextSentDate <- as.Date(Demographics_Conditions_TextMessages$TextSentDate, format = "%m/%d/%y")
Demographics_Conditions_TextMessages[sample(nrow(Demographics_Conditions_TextMessages), 10), ]

#Now we need to add a column that returns a number for each week based on when it falls in the year. 
#This column will be used as part of the group by.

#Creates a new column, and then assigns the week number according to the text sent date
Demographics_Conditions_TextMessages$WeekNumber <- week(strptime(Demographics_Conditions_TextMessages$TextSentDate,format='%Y-%m-%d'))

#Need to obtain the instances for each week, but since they will be mapped to a specific week, it should be put in a new table
TextCounts <- Demographics_Conditions_TextMessages %>% group_by(WeekNumber)  %>%count(SenderName)

#Close the ODBC connection when you have no more queries to make

#Creates the bar graph based on when the text is sent by week
library(ggplot2)
ggplot(TextCounts, aes(TextCounts$WeekNumber, TextCounts$n, colour=TextCounts$SenderName)) + geom_line() + xlab("Weeks") + ylab("Count") + labs(col="Sender Name")

## Question 6 - need to obtain the count of texts based on the chronic condition over a period of time (say per week). Draw a visual using ggplot to obtain the counts
#Grab the count of texts
ConditionsCount <- Demographics_Conditions_TextMessages %>% group_by(WeekNumber) %>% count(tri_name)

#Display it in ggplot
ggplot(ConditionsCount, aes(ConditionsCount$WeekNumber, ConditionsCount$n, colour=ConditionsCount$tri_name)) + geom_line() + xlab("Weeks") + ylab("Count") + labs(col="Conditions Name")


