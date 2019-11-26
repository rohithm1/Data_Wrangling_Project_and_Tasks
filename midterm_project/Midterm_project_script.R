
# Problem: the file downloaded will have problems with it, so we need to clean it up and make it easier to manipulate and read.
#First thing we need to do is download the .xpt file and put the data into a table to access and modify in R.
#Since this is an XPT file, there are specific packages that need to be installed and accessed to be able to add the data to a table in r.
install.packages("tidyverse")
install.packages("haven")
install.packages("mltools")
install.packages("data.table")

#Here are the libraries
library(tidyverse)
library(haven)
library(dplyr)
library(sqldf)
library(data.table)
library(mltools)
#Make sure you are in the current directory this file is in if you want to download the dataset.
National_Health_Nutrition <- read_xpt("DIQ_I.XPT")

# Part A

#List the problems you see in the data set.
#Missing values - there are a lot of NAs in the data set
#Improper/Unreadable Column Names - all the column names are meaningless to anyone normally viewing the data set, so they should be changed to be a bit more descriptive and meaningful.
#One hot encoding - if this data is meant to predict future trends or find trends within the data, it may be useful to one hot encode them so we can calculate weights for each column. Therefore all the necessary data should be turned into columns readable into a machine learning algorithm. 

# Part B

#How will you address each data related issue?
#Missing values -  I will go column by column and make changes that make sense for that column. Generally, since most of the columns are numerical data but still correspond to factor pieces of information, most of the data will be turned into catagories. However, some data, like age and otherwise is an actual range of numeric data, so it is important to keep those as such. For these, I will replace those missing values with the median of the column. 

#Improper/Unreadable Column Names - I will change the column names to make more sense to a user viewing the dataset.

#One hot encoding - Since the data is generally numerical, and one hot encoding only works with catagorical data, it is necessary to convert the numerical data into factors. This process can be quite involved since each column has a different number that corresponds to a different piece of information. Therefore, I need to manually add the level to the factor table and then turn the numeric into a string with the information it pertains to. I will get this information from the website and do this for all the values. After every relevant factor data is turned into an actual string, I will run the one_hot method and create dummy variables for all the columns.

# Part C

#Give a Justification for why you dealt with the issues this way.
#Missing values - For reasons sometimes unpredictable, there are oftentimes many missing values in a table. This may be due to human error (forgetting to input data, accidently deleting), data leakage/breaches, or even because it makes sense for the data to be missing for that row based on the information.

#Nonetheless, it is still important to rectify for this mistake, and do something to ensure that missing values will not change the usefullness of a database table. 

#Since data for each column in a table can be radically different, it is important to change the data for each column in a way that makes sense for that specific column. 

#I decided to include numerical and catagorical data into my consideration because I feel like the way the data table is structured isnt entirely representative of how it should be since the numbers dont mean anything to anyone just looking at the data.

#Improper column names - I think data tables should be readable to a data scientist without an extreme amount of documentation, and so they should be changed. Otherwise it can lead to confusion and potential misinterpretation.

#One hot encoding - to analyze the data set with machine learning, it needs to be in a one hot encoding format to ensure the algorithm can be efficient and successful. Therefore, one hot encoding is a much better way to analyze the dataset than the current structure.

# Going column by column, I will address each Data-related issue
#The first column details whether a patient was told they have diabetes.

#Step 1: turn the data into a factor column because it is catagorical. It doesn't make sense to keep the data as numerical because each number corresponds to a non-numerical piece of information.

#Step 2: Replace the numerical data with its catagorical equivalent - here is the data structure:
#1 - Yes
#2 - No
#3 - Borderline
#7 - Refused
#9 - Don't know
#NA - Missing

#Step 3: Replace the column with an acceptable version, making it easier for the user to read.

#Step 4: One Hot encode the column

#Step 1
National_Health_Nutrition$DIQ010 <- as.factor(National_Health_Nutrition$DIQ010)

#Step 2
levels(National_Health_Nutrition$DIQ010) <- c(levels(National_Health_Nutrition$DIQ010), "Yes")
National_Health_Nutrition$DIQ010[National_Health_Nutrition$DIQ010=='1']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ010) <- c(levels(National_Health_Nutrition$DIQ010), "No")
National_Health_Nutrition$DIQ010[National_Health_Nutrition$DIQ010=='2']  <- 'No' 

levels(National_Health_Nutrition$DIQ010) <- c(levels(National_Health_Nutrition$DIQ010), "Borderline")
National_Health_Nutrition$DIQ010[National_Health_Nutrition$DIQ010=='3']  <- 'Borderline'

levels(National_Health_Nutrition$DIQ010) <- c(levels(National_Health_Nutrition$DIQ010), "Refused")
National_Health_Nutrition$DIQ010[National_Health_Nutrition$DIQ010=='7']  <- 'Refused'

levels(National_Health_Nutrition$DIQ010) <- c(levels(National_Health_Nutrition$DIQ010), "Don't know")
National_Health_Nutrition$DIQ010[National_Health_Nutrition$DIQ010=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ010) <- c(levels(National_Health_Nutrition$DIQ010), "Missing")
National_Health_Nutrition$DIQ010[is.na(National_Health_Nutrition$DIQ010)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [2] <- "Told_Diabetes"

#Step 4
National_Health_Nutrition <- one_hot(setDT(National_Health_Nutrition, keep.rownames=TRUE, key=NULL, check.names=FALSE), dropCols = TRUE, dropUnusedLevels = TRUE)

#The second column details the age at which each patient was told they have diabetes.

#Step 1: Since this data is, functionally, numerical, the missing values will be replaced with the median. We wouldn't do the mean because of the structure of the data, as shown:
#2 to 78 -	Range of Values
#80 - 80 years or older
#666 - Less than 1 year
#777 - Refused
#999 - Don't know
#NA - Missing

#Since the values are perfectly corresponded to the age, the actual mean will be a more inaccurate measure of the average patient than the median.

#Step 2: Replace the column with an acceptable version, making it easier for the user to read.
#Step 1
National_Health_Nutrition$DID040[is.na(National_Health_Nutrition$DID040)] <- median(National_Health_Nutrition$DID040, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [7] <- "Age_When_Told_Diabetes"

#Column 3 details whether an individual has ever had prediabetes.

#The strategy toward cleaning this data will be the exact same as the 'Told_Diabetes' column - please refer to those steps to see the logical progression.

#The only difference is that there is no 'borderline' category.

#Step 1
National_Health_Nutrition$DIQ160 <- as.factor(National_Health_Nutrition$DIQ160)

#Step 2
levels(National_Health_Nutrition$DIQ160) <- c(levels(National_Health_Nutrition$DIQ160), "Yes")
National_Health_Nutrition$DIQ160[National_Health_Nutrition$DIQ160=='1']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ160) <- c(levels(National_Health_Nutrition$DIQ160), "No")
National_Health_Nutrition$DIQ160[National_Health_Nutrition$DIQ160=='2']  <- 'No' 

levels(National_Health_Nutrition$DIQ160) <- c(levels(National_Health_Nutrition$DIQ160), "Refused")
National_Health_Nutrition$DIQ160[National_Health_Nutrition$DIQ160=='7']  <- 'Refused'

levels(National_Health_Nutrition$DIQ160) <- c(levels(National_Health_Nutrition$DIQ160), "Don't know")
National_Health_Nutrition$DIQ160[National_Health_Nutrition$DIQ160=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ160) <- c(levels(National_Health_Nutrition$DIQ160), "Missing")
National_Health_Nutrition$DIQ160[is.na(National_Health_Nutrition$DIQ160)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [8] <- "Told_prediabetes"

#Step 4
National_Health_Nutrition <- one_hot(setDT(National_Health_Nutrition, keep.rownames=TRUE, key=NULL, check.names=FALSE), dropCols = TRUE, dropUnusedLevels = TRUE)

#Column 4 details whether an individual has ever been told they have a health risk for diabetes.

#This column has the exact same structure as 'Told_prediabetes', and as such, will be dealt with the same exact way.

#Step 1
National_Health_Nutrition$DIQ170 <- as.factor(National_Health_Nutrition$DIQ170)

#Step 2
levels(National_Health_Nutrition$DIQ170) <- c(levels(National_Health_Nutrition$DIQ170), "Yes")
National_Health_Nutrition$DIQ170[National_Health_Nutrition$DIQ170=='1']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ170) <- c(levels(National_Health_Nutrition$DIQ170), "No")
National_Health_Nutrition$DIQ170[National_Health_Nutrition$DIQ170=='2']  <- 'No' 

levels(National_Health_Nutrition$DIQ170) <- c(levels(National_Health_Nutrition$DIQ170), "Refused")
National_Health_Nutrition$DIQ170[National_Health_Nutrition$DIQ170=='7']  <- 'Refused'

levels(National_Health_Nutrition$DIQ170) <- c(levels(National_Health_Nutrition$DIQ170), "Don't know")
National_Health_Nutrition$DIQ170[National_Health_Nutrition$DIQ170=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ170) <- c(levels(National_Health_Nutrition$DIQ170), "Missing")
National_Health_Nutrition$DIQ170[is.na(National_Health_Nutrition$DIQ170)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [12] <- "Told_diabetes_health_risk"

#Step 4
National_Health_Nutrition <- one_hot(setDT(National_Health_Nutrition, keep.rownames=TRUE, key=NULL, check.names=FALSE), dropCols = TRUE, dropUnusedLevels = TRUE)

#Column 5 details whether a patient feels they are at risk for diabetes.
#This column has the exact same structure as 'Told_prediabetes', and as such, will be dealt with the same exact way.

#Step 1
National_Health_Nutrition$DIQ172 <- as.factor(National_Health_Nutrition$DIQ172)

#Step 2
levels(National_Health_Nutrition$DIQ172) <- c(levels(National_Health_Nutrition$DIQ172), "Yes")
National_Health_Nutrition$DIQ172[National_Health_Nutrition$DIQ172=='1']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ172) <- c(levels(National_Health_Nutrition$DIQ172), "No")
National_Health_Nutrition$DIQ172[National_Health_Nutrition$DIQ172=='2']  <- 'No' 

levels(National_Health_Nutrition$DIQ172) <- c(levels(National_Health_Nutrition$DIQ172), "Refused")
National_Health_Nutrition$DIQ172[National_Health_Nutrition$DIQ172=='7']  <- 'Refused'

levels(National_Health_Nutrition$DIQ172) <- c(levels(National_Health_Nutrition$DIQ172), "Don't know")
National_Health_Nutrition$DIQ172[National_Health_Nutrition$DIQ172=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ172) <- c(levels(National_Health_Nutrition$DIQ172), "Missing")
National_Health_Nutrition$DIQ172[is.na(National_Health_Nutrition$DIQ172)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [16] <- "Felt_at_risk"

#Step 4
National_Health_Nutrition <- one_hot(setDT(National_Health_Nutrition, keep.rownames=TRUE, key=NULL, check.names=FALSE), dropCols = TRUE, dropUnusedLevels = TRUE)

#Columns DIQ175A to DIQ175X are actually quite interesting. The previous column asks if an individual felt at risk whereas these columns indicate if the specific column was the reason why.

#Every column (besides family history) only has 2 values in its structure, as follows:
#  [insert number] - [insert reason]
#NA - Missing

#Family history has the following structure:
#10 -	Family history	
#77 - Refused	
#99 - Don't know	
#NA - Missing

#However, the counts for refused and Don't know are 0, meaning it still is functionally the same structure as the rest of the variables (reasons why a patient felt at risk). As a result, they will all be dealt with in the same way (again similar to 'Told_diabetes'/'Told_prediabetes')

##Family history
#Step 1
National_Health_Nutrition$DIQ175A <- as.factor(National_Health_Nutrition$DIQ175A)

#Step 2
levels(National_Health_Nutrition$DIQ175A) <- c(levels(National_Health_Nutrition$DIQ175A), "Yes")
National_Health_Nutrition$DIQ175A[National_Health_Nutrition$DIQ175A=='10']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175A) <- c(levels(National_Health_Nutrition$DIQ175A), "Refused")
National_Health_Nutrition$DIQ175A[National_Health_Nutrition$DIQ175A=='77']  <- 'Refused' 

levels(National_Health_Nutrition$DIQ175A) <- c(levels(National_Health_Nutrition$DIQ175A), "Don't know")
National_Health_Nutrition$DIQ175A[National_Health_Nutrition$DIQ175A=='99']  <- "Don't know"

levels(National_Health_Nutrition$DIQ175A) <- c(levels(National_Health_Nutrition$DIQ175A), "Missing")
National_Health_Nutrition$DIQ175A[is.na(National_Health_Nutrition$DIQ175A)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [21] <- "Reason_Family_History"

##Overweight
#Step 1
National_Health_Nutrition$DIQ175B <- as.factor(National_Health_Nutrition$DIQ175B)

#Step 2
levels(National_Health_Nutrition$DIQ175B) <- c(levels(National_Health_Nutrition$DIQ175B), "Yes")
National_Health_Nutrition$DIQ175B[National_Health_Nutrition$DIQ175B=='11']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175B) <- c(levels(National_Health_Nutrition$DIQ175B), "Missing")
National_Health_Nutrition$DIQ175B[is.na(National_Health_Nutrition$DIQ175B)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [22] <- "Reason_Overweight"

##Age
#Step 1
National_Health_Nutrition$DIQ175C <- as.factor(National_Health_Nutrition$DIQ175C)

#Step 2
levels(National_Health_Nutrition$DIQ175C) <- c(levels(National_Health_Nutrition$DIQ175C), "Yes")
National_Health_Nutrition$DIQ175C[National_Health_Nutrition$DIQ175C=='12']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175C) <- c(levels(National_Health_Nutrition$DIQ175C), "Missing")
National_Health_Nutrition$DIQ175C[is.na(National_Health_Nutrition$DIQ175C)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [23] <- "Reason_Age"

##Poor Diet
#Step 1
National_Health_Nutrition$DIQ175D <- as.factor(National_Health_Nutrition$DIQ175D)

#Step 2
levels(National_Health_Nutrition$DIQ175D) <- c(levels(National_Health_Nutrition$DIQ175D), "Yes")
National_Health_Nutrition$DIQ175D[National_Health_Nutrition$DIQ175D=='13']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175D) <- c(levels(National_Health_Nutrition$DIQ175D), "Missing")
National_Health_Nutrition$DIQ175D[is.na(National_Health_Nutrition$DIQ175D)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [24] <- "Reason_Poor_Diet"

##Race
#Step 1
National_Health_Nutrition$DIQ175E <- as.factor(National_Health_Nutrition$DIQ175E)

#Step 2
levels(National_Health_Nutrition$DIQ175E) <- c(levels(National_Health_Nutrition$DIQ175E), "Yes")
National_Health_Nutrition$DIQ175E[National_Health_Nutrition$DIQ175E=='14']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175E) <- c(levels(National_Health_Nutrition$DIQ175E), "Missing")
National_Health_Nutrition$DIQ175E[is.na(National_Health_Nutrition$DIQ175E)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [25] <- "Reason_Race"

##Baby over 9 pounds
#Step 1
National_Health_Nutrition$DIQ175F <- as.factor(National_Health_Nutrition$DIQ175F)

#Step 2
levels(National_Health_Nutrition$DIQ175F) <- c(levels(National_Health_Nutrition$DIQ175F), "Yes")
National_Health_Nutrition$DIQ175F[National_Health_Nutrition$DIQ175F=='15']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175F) <- c(levels(National_Health_Nutrition$DIQ175F), "Missing")
National_Health_Nutrition$DIQ175F[is.na(National_Health_Nutrition$DIQ175F)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [26] <- "Reason_Baby_Over_9lbs"

##Lack of physical activity
#Step 1
National_Health_Nutrition$DIQ175G <- as.factor(National_Health_Nutrition$DIQ175G)

#Step 2
levels(National_Health_Nutrition$DIQ175G) <- c(levels(National_Health_Nutrition$DIQ175G), "Yes")
National_Health_Nutrition$DIQ175G[National_Health_Nutrition$DIQ175G=='16']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175G) <- c(levels(National_Health_Nutrition$DIQ175G), "Missing")
National_Health_Nutrition$DIQ175G[is.na(National_Health_Nutrition$DIQ175G)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [27] <- "Reason_Lack_of_Physical_Activity"

##High blood pressure
#Step 1
National_Health_Nutrition$DIQ175H <- as.factor(National_Health_Nutrition$DIQ175H)

#Step 2
levels(National_Health_Nutrition$DIQ175H) <- c(levels(National_Health_Nutrition$DIQ175H), "Yes")
National_Health_Nutrition$DIQ175H[National_Health_Nutrition$DIQ175H=='17']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175H) <- c(levels(National_Health_Nutrition$DIQ175H), "Missing")
National_Health_Nutrition$DIQ175H[is.na(National_Health_Nutrition$DIQ175H)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [28] <- "Reason_High_Blood_Pressure"

##High blood sugar
#Step 1
National_Health_Nutrition$DIQ175I <- as.factor(National_Health_Nutrition$DIQ175I)

#Step 2
levels(National_Health_Nutrition$DIQ175I) <- c(levels(National_Health_Nutrition$DIQ175I), "Yes")
National_Health_Nutrition$DIQ175I[National_Health_Nutrition$DIQ175I=='18']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175I) <- c(levels(National_Health_Nutrition$DIQ175I), "Missing")
National_Health_Nutrition$DIQ175I[is.na(National_Health_Nutrition$DIQ175I)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [29] <- "Reason_High_Blood_Sugar"

##High cholesterol
#Step 1
National_Health_Nutrition$DIQ175J <- as.factor(National_Health_Nutrition$DIQ175J)

#Step 2
levels(National_Health_Nutrition$DIQ175J) <- c(levels(National_Health_Nutrition$DIQ175J), "Yes")
National_Health_Nutrition$DIQ175J[National_Health_Nutrition$DIQ175J=='19']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175J) <- c(levels(National_Health_Nutrition$DIQ175J), "Missing")
National_Health_Nutrition$DIQ175J[is.na(National_Health_Nutrition$DIQ175J)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [30] <- "Reason_High_Cholesterol"

##Hypoglycemic
#Step 1
National_Health_Nutrition$DIQ175K <- as.factor(National_Health_Nutrition$DIQ175K)

#Step 2
levels(National_Health_Nutrition$DIQ175K) <- c(levels(National_Health_Nutrition$DIQ175K), "Yes")
National_Health_Nutrition$DIQ175K[National_Health_Nutrition$DIQ175K=='20']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175K) <- c(levels(National_Health_Nutrition$DIQ175K), "Missing")
National_Health_Nutrition$DIQ175K[is.na(National_Health_Nutrition$DIQ175K)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [31] <- "Reason_Hypoglycemic"

##Extreme hunger
#Step 1
National_Health_Nutrition$DIQ175L <- as.factor(National_Health_Nutrition$DIQ175L)

#Step 2
levels(National_Health_Nutrition$DIQ175L) <- c(levels(National_Health_Nutrition$DIQ175L), "Yes")
National_Health_Nutrition$DIQ175L[National_Health_Nutrition$DIQ175L=='21']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175L) <- c(levels(National_Health_Nutrition$DIQ175L), "Missing")
National_Health_Nutrition$DIQ175L[is.na(National_Health_Nutrition$DIQ175L)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [32] <- "Reason_Extreme_Hunger"

##Tingling/Numbness
#Step 1
National_Health_Nutrition$DIQ175M <- as.factor(National_Health_Nutrition$DIQ175M)

#Step 2
levels(National_Health_Nutrition$DIQ175M) <- c(levels(National_Health_Nutrition$DIQ175M), "Yes")
National_Health_Nutrition$DIQ175M[National_Health_Nutrition$DIQ175M=='22']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175M) <- c(levels(National_Health_Nutrition$DIQ175M), "Missing")
National_Health_Nutrition$DIQ175M[is.na(National_Health_Nutrition$DIQ175M)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [33] <- "Reason_Numbness"

##Blurred vision
#Step 1
National_Health_Nutrition$DIQ175N <- as.factor(National_Health_Nutrition$DIQ175N)

#Step 2
levels(National_Health_Nutrition$DIQ175N) <- c(levels(National_Health_Nutrition$DIQ175N), "Yes")
National_Health_Nutrition$DIQ175N[National_Health_Nutrition$DIQ175N=='23']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175N) <- c(levels(National_Health_Nutrition$DIQ175N), "Missing")
National_Health_Nutrition$DIQ175N[is.na(National_Health_Nutrition$DIQ175N)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [34] <- "Reason_Blurred_Vision"

##Increased fatigue
#Step 1
National_Health_Nutrition$DIQ175O <- as.factor(National_Health_Nutrition$DIQ175O)

#Step 2
levels(National_Health_Nutrition$DIQ175O) <- c(levels(National_Health_Nutrition$DIQ175O), "Yes")
National_Health_Nutrition$DIQ175O[National_Health_Nutrition$DIQ175O=='24']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175O) <- c(levels(National_Health_Nutrition$DIQ175O), "Missing")
National_Health_Nutrition$DIQ175O[is.na(National_Health_Nutrition$DIQ175O)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [35] <- "Reason_Increased_Fatigue"

##Anyone could be at risk
#Step 1
National_Health_Nutrition$DIQ175P <- as.factor(National_Health_Nutrition$DIQ175P)

#Step 2
levels(National_Health_Nutrition$DIQ175P) <- c(levels(National_Health_Nutrition$DIQ175P), "Yes")
National_Health_Nutrition$DIQ175P[National_Health_Nutrition$DIQ175P=='25']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175P) <- c(levels(National_Health_Nutrition$DIQ175P), "Missing")
National_Health_Nutrition$DIQ175P[is.na(National_Health_Nutrition$DIQ175P)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [36] <- "Reason_Anyone_At_Risk"

##Doctor warning
#Step 1
National_Health_Nutrition$DIQ175Q <- as.factor(National_Health_Nutrition$DIQ175Q)

#Step 2
levels(National_Health_Nutrition$DIQ175Q) <- c(levels(National_Health_Nutrition$DIQ175Q), "Yes")
National_Health_Nutrition$DIQ175Q[National_Health_Nutrition$DIQ175Q=='26']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175Q) <- c(levels(National_Health_Nutrition$DIQ175Q), "Missing")
National_Health_Nutrition$DIQ175Q[is.na(National_Health_Nutrition$DIQ175Q)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [37] <- "Reason_Doctor_Warning"

##Other, specify
#Step 1
National_Health_Nutrition$DIQ175R <- as.factor(National_Health_Nutrition$DIQ175R)

#Step 2
levels(National_Health_Nutrition$DIQ175R) <- c(levels(National_Health_Nutrition$DIQ175R), "Yes")
National_Health_Nutrition$DIQ175R[National_Health_Nutrition$DIQ175R=='27']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175R) <- c(levels(National_Health_Nutrition$DIQ175R), "Missing")
National_Health_Nutrition$DIQ175R[is.na(National_Health_Nutrition$DIQ175R)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [38] <- "Reason_Other"

##Gestational diabetes
#Step 1
National_Health_Nutrition$DIQ175S <- as.factor(National_Health_Nutrition$DIQ175S)

#Step 2
levels(National_Health_Nutrition$DIQ175S) <- c(levels(National_Health_Nutrition$DIQ175S), "Yes")
National_Health_Nutrition$DIQ175S[National_Health_Nutrition$DIQ175S=='28']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175S) <- c(levels(National_Health_Nutrition$DIQ175S), "Missing")
National_Health_Nutrition$DIQ175S[is.na(National_Health_Nutrition$DIQ175S)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [39] <- "Reason_Gestational_Diabetes"

##Frequent Urination
#Step 1
National_Health_Nutrition$DIQ175T <- as.factor(National_Health_Nutrition$DIQ175T)

#Step 2
levels(National_Health_Nutrition$DIQ175T) <- c(levels(National_Health_Nutrition$DIQ175T), "Yes")
National_Health_Nutrition$DIQ175T[National_Health_Nutrition$DIQ175T=='29']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175T) <- c(levels(National_Health_Nutrition$DIQ175T), "Missing")
National_Health_Nutrition$DIQ175T[is.na(National_Health_Nutrition$DIQ175T)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [40] <- "Reason_Frequent_Urination"

##Thirst
#Step 1
National_Health_Nutrition$DIQ175U <- as.factor(National_Health_Nutrition$DIQ175U)

#Step 2
levels(National_Health_Nutrition$DIQ175U) <- c(levels(National_Health_Nutrition$DIQ175U), "Yes")
National_Health_Nutrition$DIQ175U[National_Health_Nutrition$DIQ175U=='30']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175U) <- c(levels(National_Health_Nutrition$DIQ175U), "Missing")
National_Health_Nutrition$DIQ175U[is.na(National_Health_Nutrition$DIQ175U)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [41] <- "Reason_Thirst"

##Craving for sweet/eating a lot of sugar
#Step 1
National_Health_Nutrition$DIQ175V <- as.factor(National_Health_Nutrition$DIQ175V)

#Step 2
levels(National_Health_Nutrition$DIQ175V) <- c(levels(National_Health_Nutrition$DIQ175V), "Yes")
National_Health_Nutrition$DIQ175V[National_Health_Nutrition$DIQ175V=='31']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175V) <- c(levels(National_Health_Nutrition$DIQ175V), "Missing")
National_Health_Nutrition$DIQ175V[is.na(National_Health_Nutrition$DIQ175V)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [42] <- "Reason_Craving_Sweet"

##Medication
#Step 1
National_Health_Nutrition$DIQ175W <- as.factor(National_Health_Nutrition$DIQ175W)

#Step 2
levels(National_Health_Nutrition$DIQ175W) <- c(levels(National_Health_Nutrition$DIQ175W), "Yes")
National_Health_Nutrition$DIQ175W[National_Health_Nutrition$DIQ175W=='32']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175W) <- c(levels(National_Health_Nutrition$DIQ175W), "Missing")
National_Health_Nutrition$DIQ175W[is.na(National_Health_Nutrition$DIQ175W)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [43] <- "Reason_Medication"

##Polycystic ovarian syndrome
#Step 1
National_Health_Nutrition$DIQ175X <- as.factor(National_Health_Nutrition$DIQ175X)

#Step 2
levels(National_Health_Nutrition$DIQ175X) <- c(levels(National_Health_Nutrition$DIQ175X), "Yes")
National_Health_Nutrition$DIQ175X[National_Health_Nutrition$DIQ175X=='33']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ175X) <- c(levels(National_Health_Nutrition$DIQ175X), "Missing")
National_Health_Nutrition$DIQ175X[is.na(National_Health_Nutrition$DIQ175X)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [44] <- "Reason_Ovarian_Syndrome"

#At this point, since we have proven that the dummy variable creator works, we will do the last step at the very end to save code.

#The next column describes whether a patient has had their blood tested within the past three years.
#This column has the exact same structure as 'Told_prediabetes', and as such, will be dealt with the same exact way.

#Step 1
National_Health_Nutrition$DIQ180 <- as.factor(National_Health_Nutrition$DIQ180)

#Step 2
levels(National_Health_Nutrition$DIQ180) <- c(levels(National_Health_Nutrition$DIQ180), "Yes")
National_Health_Nutrition$DIQ180[National_Health_Nutrition$DIQ180=='1']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ180) <- c(levels(National_Health_Nutrition$DIQ180), "No")
National_Health_Nutrition$DIQ180[National_Health_Nutrition$DIQ180=='2']  <- 'No' 

levels(National_Health_Nutrition$DIQ180) <- c(levels(National_Health_Nutrition$DIQ180), "Refused")
National_Health_Nutrition$DIQ180[National_Health_Nutrition$DIQ180=='7']  <- 'Refused'

levels(National_Health_Nutrition$DIQ180) <- c(levels(National_Health_Nutrition$DIQ180), "Don't know")
National_Health_Nutrition$DIQ180[National_Health_Nutrition$DIQ180=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ180) <- c(levels(National_Health_Nutrition$DIQ180), "Missing")
National_Health_Nutrition$DIQ180[is.na(National_Health_Nutrition$DIQ180)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [45] <- "Had_Blood_Tested"


#The next column describes whether a patient is currently taking insulin
#This column has the exact same structure as 'Told_prediabetes', and as such, will be dealt with the same exact way.
#Step 1
National_Health_Nutrition$DIQ050 <- as.factor(National_Health_Nutrition$DIQ050)

#Step 2
levels(National_Health_Nutrition$DIQ050) <- c(levels(National_Health_Nutrition$DIQ050), "Yes")
National_Health_Nutrition$DIQ050[National_Health_Nutrition$DIQ050=='1']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ050) <- c(levels(National_Health_Nutrition$DIQ050), "No")
National_Health_Nutrition$DIQ050[National_Health_Nutrition$DIQ050=='2']  <- 'No' 

levels(National_Health_Nutrition$DIQ050) <- c(levels(National_Health_Nutrition$DIQ050), "Refused")
National_Health_Nutrition$DIQ050[National_Health_Nutrition$DIQ050=='7']  <- 'Refused'

levels(National_Health_Nutrition$DIQ050) <- c(levels(National_Health_Nutrition$DIQ050), "Don't know")
National_Health_Nutrition$DIQ050[National_Health_Nutrition$DIQ050=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ050) <- c(levels(National_Health_Nutrition$DIQ050), "Missing")
National_Health_Nutrition$DIQ050[is.na(National_Health_Nutrition$DIQ050)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [46] <- "Taking_Insulin"

#The next column details how long the individual has been taking insulin.
#Though this is a numerical data set - each number doesn't necessarily correspond to the length of time an individual has been taking insulin, the missing values will be replaced with the median. We wouldn't do the mean because of the structure of the data, as shown:
#1 to 55	- Range of Values
#666	- Less than 1 month
#777	- Refused
#999	- Don't know
#NA - Missing

#Step 1: Since the values are perfectly corresponded to the time, the actual mean will be a more inaccurate measure of the average patient than the median.

#Step 2: Replace the column with an acceptable version, making it easier for the user to read.
#Step 1
National_Health_Nutrition$DID060[is.na(National_Health_Nutrition$DID060)] <- median(National_Health_Nutrition$DID060, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [47] <- "Insulin_Time_Length"


#The next column is just a unit of measurement of the previous column, so it will be dealt with in a similar way, but not exactly.
#Step 1
National_Health_Nutrition$DIQ060U <- as.factor(National_Health_Nutrition$DIQ060U)

#Step 2
levels(National_Health_Nutrition$DIQ060U) <- c(levels(National_Health_Nutrition$DIQ060U), "Months")
National_Health_Nutrition$DIQ060U[National_Health_Nutrition$DIQ060U=='1']  <- 'Months' 

levels(National_Health_Nutrition$DIQ060U) <- c(levels(National_Health_Nutrition$DIQ060U), "Years")
National_Health_Nutrition$DIQ060U[National_Health_Nutrition$DIQ060U=='2']  <- 'Years' 

levels(National_Health_Nutrition$DIQ060U) <- c(levels(National_Health_Nutrition$DIQ060U), "Missing")
National_Health_Nutrition$DIQ060U[is.na(National_Health_Nutrition$DIQ060U)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [48] <- "Insulin_Measurement"

#The next column describes whether a patient is currently taking diabetic pills to lower blood sugar
#This column has the exact same structure as 'Told_prediabetes', and as such, will be dealt with the same exact way.

#Step 1
National_Health_Nutrition$DIQ070 <- as.factor(National_Health_Nutrition$DIQ070)

#Step 2
levels(National_Health_Nutrition$DIQ070) <- c(levels(National_Health_Nutrition$DIQ070), "Yes")
National_Health_Nutrition$DIQ070[National_Health_Nutrition$DIQ070=='1']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ070) <- c(levels(National_Health_Nutrition$DIQ070), "No")
National_Health_Nutrition$DIQ070[National_Health_Nutrition$DIQ070=='2']  <- 'No' 

levels(National_Health_Nutrition$DIQ070) <- c(levels(National_Health_Nutrition$DIQ070), "Refused")
National_Health_Nutrition$DIQ070[National_Health_Nutrition$DIQ070=='7']  <- 'Refused'

levels(National_Health_Nutrition$DIQ070) <- c(levels(National_Health_Nutrition$DIQ070), "Don't know")
National_Health_Nutrition$DIQ070[National_Health_Nutrition$DIQ070=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ070) <- c(levels(National_Health_Nutrition$DIQ070), "Missing")
National_Health_Nutrition$DIQ070[is.na(National_Health_Nutrition$DIQ070)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [49] <- "Taking_Diabetic_Pills"

#The next column describes how long ago a patient saw a diabetes specialist. 

#The structure for the column is extremely peculiar, and is as follows:
#1	- 1 year ago or less
#2	- More than 1 year ago but no more than 2 years ago
#3	- More than 2 years ago but no more than 5 years ago	
#4	- More than 5 years ago
#5	- Never
#7	- Refused
#9	- Don't know
#.	- Missing

#Because this data structure is so ambiguous and in pure ranges of data, we will treat it as catagorical - using our logic similar to previous catagorical variables, but renaming the columns to be more reader friendly.

#Step 1
National_Health_Nutrition$DIQ230 <- as.factor(National_Health_Nutrition$DIQ230)

#Step 2
levels(National_Health_Nutrition$DIQ230) <- c(levels(National_Health_Nutrition$DIQ230), "<1")
National_Health_Nutrition$DIQ230[National_Health_Nutrition$DIQ230=='1']  <- '<1' 

levels(National_Health_Nutrition$DIQ230) <- c(levels(National_Health_Nutrition$DIQ230), "1-2")
National_Health_Nutrition$DIQ230[National_Health_Nutrition$DIQ230=='2']  <- '1-2' 

levels(National_Health_Nutrition$DIQ230) <- c(levels(National_Health_Nutrition$DIQ230), "2-5")
National_Health_Nutrition$DIQ230[National_Health_Nutrition$DIQ230=='3']  <- '2-5'

levels(National_Health_Nutrition$DIQ230) <- c(levels(National_Health_Nutrition$DIQ230), ">5")
National_Health_Nutrition$DIQ230[National_Health_Nutrition$DIQ230=='4']  <- ">5"

levels(National_Health_Nutrition$DIQ230) <- c(levels(National_Health_Nutrition$DIQ230), "Never")
National_Health_Nutrition$DIQ230[National_Health_Nutrition$DIQ230=='5']  <- "Never"

levels(National_Health_Nutrition$DIQ230) <- c(levels(National_Health_Nutrition$DIQ230), "Refused")
National_Health_Nutrition$DIQ230[National_Health_Nutrition$DIQ230=='7']  <- "Refused"

levels(National_Health_Nutrition$DIQ230) <- c(levels(National_Health_Nutrition$DIQ230), "Don't know")
National_Health_Nutrition$DIQ230[National_Health_Nutrition$DIQ230=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ230) <- c(levels(National_Health_Nutrition$DIQ230), "Missing")
National_Health_Nutrition$DIQ230[is.na(National_Health_Nutrition$DIQ230)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [50] <- "Years_Since_Specialist"

#The next column describes whether a patient sees 1 dr for diabetes
#This column has the exact same structure as 'Told_prediabetes', and as such, will be dealt with the same exact way.

#Step 1
National_Health_Nutrition$DIQ240 <- as.factor(National_Health_Nutrition$DIQ240)

#Step 2
levels(National_Health_Nutrition$DIQ240) <- c(levels(National_Health_Nutrition$DIQ240), "Yes")
National_Health_Nutrition$DIQ240[National_Health_Nutrition$DIQ240=='1']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ240) <- c(levels(National_Health_Nutrition$DIQ240), "No")
National_Health_Nutrition$DIQ240[National_Health_Nutrition$DIQ240=='2']  <- 'No' 

levels(National_Health_Nutrition$DIQ240) <- c(levels(National_Health_Nutrition$DIQ240), "Refused")
National_Health_Nutrition$DIQ240[National_Health_Nutrition$DIQ240=='7']  <- 'Refused'

levels(National_Health_Nutrition$DIQ240) <- c(levels(National_Health_Nutrition$DIQ240), "Don't know")
National_Health_Nutrition$DIQ240[National_Health_Nutrition$DIQ240=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ240) <- c(levels(National_Health_Nutrition$DIQ240), "Missing")
National_Health_Nutrition$DIQ240[is.na(National_Health_Nutrition$DIQ240)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [51] <- "One_DR"

#The next column describes how many times in the past year the patient has seen the doctor.

#Step 1: Since this data is, functionally, numerical, the missing values will be replaced with the median. We wouldn't do the mean because of the structure of the data, as shown:
#1 to 60	- Range of Values
#0	- None
#777 - Refused
#999 - Don't know
#NA - Missing

#Since the values are not perfectly corresponded to the times a person has visited in the pasrt year, the actual mean will be a more inaccurate measure of the average patient than the median.

#Step 2: Replace the column with an acceptable version, making it easier for the user to read.

#Step 1
National_Health_Nutrition$DID250[is.na(National_Health_Nutrition$DID250)] <- median(National_Health_Nutrition$DID250, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [52] <- "Doctor_Visits_Past_Year"

#The next column details how often a patient checks for glucose levels. 
#This column is also a range of values, and is therefore very similar to the "Doctor_Visits_Past_Year", and therefore will be treated as such. 
#Step 1
National_Health_Nutrition$DID260[is.na(National_Health_Nutrition$DID260)] <- median(National_Health_Nutrition$DID260, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [53] <- "Glucose_Checks"

#The next column is just a unit of measurement of the previous column, so it will be dealt with in a catagorical way.
#Step 1
National_Health_Nutrition$DIQ260U <- as.factor(National_Health_Nutrition$DIQ260U)

#Step 2
levels(National_Health_Nutrition$DIQ260U) <- c(levels(National_Health_Nutrition$DIQ260U), "Per Day")
National_Health_Nutrition$DIQ260U[National_Health_Nutrition$DIQ260U=='1']  <- 'Per Day' 

levels(National_Health_Nutrition$DIQ260U) <- c(levels(National_Health_Nutrition$DIQ260U), "Per Week")
National_Health_Nutrition$DIQ260U[National_Health_Nutrition$DIQ260U=='2']  <- 'Per Week' 

levels(National_Health_Nutrition$DIQ260U) <- c(levels(National_Health_Nutrition$DIQ260U), "Per Month")
National_Health_Nutrition$DIQ260U[National_Health_Nutrition$DIQ260U=='3']  <- 'Per Month' 

levels(National_Health_Nutrition$DIQ260U) <- c(levels(National_Health_Nutrition$DIQ260U), "Per Year")
National_Health_Nutrition$DIQ260U[National_Health_Nutrition$DIQ260U=='4']  <- 'Per Year' 

levels(National_Health_Nutrition$DIQ260U) <- c(levels(National_Health_Nutrition$DIQ260U), "Missing")
National_Health_Nutrition$DIQ260U[is.na(National_Health_Nutrition$DIQ260U)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [54] <- "Diabetes_Measurement"

#The next column describes whether a doctor checked them for A1C in the past year.
#This column has the exact same structure as 'Told_prediabetes', and as such, will be dealt with the same exact way.

#Step 1
National_Health_Nutrition$DIQ275 <- as.factor(National_Health_Nutrition$DIQ275)

#Step 2
levels(National_Health_Nutrition$DIQ275) <- c(levels(National_Health_Nutrition$DIQ275), "Yes")
National_Health_Nutrition$DIQ275[National_Health_Nutrition$DIQ275=='1']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ275) <- c(levels(National_Health_Nutrition$DIQ275), "No")
National_Health_Nutrition$DIQ275[National_Health_Nutrition$DIQ275=='2']  <- 'No' 

levels(National_Health_Nutrition$DIQ275) <- c(levels(National_Health_Nutrition$DIQ275), "Refused")
National_Health_Nutrition$DIQ275[National_Health_Nutrition$DIQ275=='7']  <- 'Refused'

levels(National_Health_Nutrition$DIQ275) <- c(levels(National_Health_Nutrition$DIQ275), "Don't know")
National_Health_Nutrition$DIQ275[National_Health_Nutrition$DIQ275=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ275) <- c(levels(National_Health_Nutrition$DIQ275), "Missing")
National_Health_Nutrition$DIQ275[is.na(National_Health_Nutrition$DIQ275)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [55] <- "Checked_A1C_Past_Year"

#The next column describes what the patient's last A1C level was
#This column is also a range of values, and is therefore very similar to the "Doctor_Visits_Past_Year", and therefore will be treated as such. 
#Step 1
National_Health_Nutrition$DIQ280[is.na(National_Health_Nutrition$DIQ280)] <- median(National_Health_Nutrition$DIQ280, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [56] <- "Last_A1C_Level"

#The next column describes what the doctor recommends the patient's A1C level to be. Here is the data structure:
  
#1	- Less than 6	
#2	- Less than 7	
#3	- Less than 8	
#4	- Less than 9
#5	- Less than 10
#6	- Provider did not specify goal
#77 - Refused
#99 - Don't know
#NA - Missing

#Because this data structure is so ambiguous and in pure ranges of data, we will treat it as catagorical - using our logic similar to previous catagorical variables, but renaming the columns to be more reader friendly.

#Step 1
National_Health_Nutrition$DIQ291 <- as.factor(National_Health_Nutrition$DIQ291)

#Step 2
levels(National_Health_Nutrition$DIQ291) <- c(levels(National_Health_Nutrition$DIQ291), "<6")
National_Health_Nutrition$DIQ291[National_Health_Nutrition$DIQ291=='1']  <- '<6' 

levels(National_Health_Nutrition$DIQ291) <- c(levels(National_Health_Nutrition$DIQ291), "<7")
National_Health_Nutrition$DIQ291[National_Health_Nutrition$DIQ291=='2']  <- '<7' 

levels(National_Health_Nutrition$DIQ291) <- c(levels(National_Health_Nutrition$DIQ291), "<8")
National_Health_Nutrition$DIQ291[National_Health_Nutrition$DIQ291=='3']  <- '<8' 

levels(National_Health_Nutrition$DIQ291) <- c(levels(National_Health_Nutrition$DIQ291), "<9")
National_Health_Nutrition$DIQ291[National_Health_Nutrition$DIQ291=='4']  <- '<9' 

levels(National_Health_Nutrition$DIQ291) <- c(levels(National_Health_Nutrition$DIQ291), "<10")
National_Health_Nutrition$DIQ291[National_Health_Nutrition$DIQ291=='5']  <- '<10' 

levels(National_Health_Nutrition$DIQ291) <- c(levels(National_Health_Nutrition$DIQ291), "Refused")
National_Health_Nutrition$DIQ291[National_Health_Nutrition$DIQ291=='77']  <- "Refused"

levels(National_Health_Nutrition$DIQ291) <- c(levels(National_Health_Nutrition$DIQ291), "Don't know")
National_Health_Nutrition$DIQ291[National_Health_Nutrition$DIQ291=='99']  <- "Don't know"

levels(National_Health_Nutrition$DIQ291) <- c(levels(National_Health_Nutrition$DIQ291), "Missing")
National_Health_Nutrition$DIQ291[is.na(National_Health_Nutrition$DIQ291)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [57] <- "Doctor_A1C_Recommendation"

#The next column describes what the patient's last SBP level was.  
#This column is also a range of values, and is therefore very similar to the "Doctor_Visits_Past_Year", and therefore will be treated as such. 
#Step 1
National_Health_Nutrition$DIQ300S[is.na(National_Health_Nutrition$DIQ300S)] <- median(National_Health_Nutrition$DIQ300S, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [58] <- "Recent_SBP"


#The next column describes what the patient's last DBP level was.  
#This column is also a range of values, and is therefore very similar to the "Doctor_Visits_Past_Year", and therefore will be treated as such. 
#Step 1
National_Health_Nutrition$DIQ300D[is.na(National_Health_Nutrition$DIQ300D)] <- median(National_Health_Nutrition$DIQ300D, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [59] <- "Recent_DBP"

#The next column describes what the doctor recommends the patient's SBP level to be  
#This column is also a range of values, and is therefore very similar to the "Doctor_Visits_Past_Year", and therefore will be treated as such. 
#Step 1
National_Health_Nutrition$DID310S[is.na(National_Health_Nutrition$DID310S)] <- median(National_Health_Nutrition$DID310S, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [60] <- "Doctor_Recommended_SBP"

#The next column describes what the doctor recommends the patient's DBP level to be  
#This column is also a range of values, and is therefore very similar to the "Doctor_Visits_Past_Year", and therefore will be treated as such. 
#Step 1
National_Health_Nutrition$DID310D[is.na(National_Health_Nutrition$DID310D)] <- median(National_Health_Nutrition$DID310D, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [61] <- "Doctor_Recommended_DBP"

#The next column describes the patient's most recent LDL number. 
#This column is also a range of values, and is therefore very similar to the "Doctor_Visits_Past_Year", and therefore will be treated as such. 
#Step 1
National_Health_Nutrition$DID320[is.na(National_Health_Nutrition$DID320)] <- median(National_Health_Nutrition$DID320, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [62] <- "Recent_LDL_Number"

#The next column describes what the doctor recommends the patient's LDL number to be.
#This column is also a range of values, and is therefore very similar to the "Doctor_Visits_Past_Year", and therefore will be treated as such. 
#Step 1
National_Health_Nutrition$DID330[is.na(National_Health_Nutrition$DID330)] <- median(National_Health_Nutrition$DID330, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [63] <- "DR_Rec_LDL"

#The next column describes how many times a doctor has checked the sores of a patient in the past year
#This column is also a range of values, and is therefore very similar to the "Doctor_Visits_Past_Year", and therefore will be treated as such. 
#Step 1
National_Health_Nutrition$DID341[is.na(National_Health_Nutrition$DID341)] <- median(National_Health_Nutrition$DID341, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [64] <- "DR_Sore_Check_Year"


#The next column describes how many times a patient has checked their own feet for sores
#This column is also a range of values, and is therefore very similar to the "Doctor_Visits_Past_Year", and therefore will be treated as such. 
#Step 1
National_Health_Nutrition$DID350[is.na(National_Health_Nutrition$DID350)] <- median(National_Health_Nutrition$DID350, na.rm = TRUE)

#Step 2
colnames(National_Health_Nutrition) [65] <- "Patient_Sore_Check_Year"

#The next column is just a unit of measurement of the previous column, so it will be dealt with in a similar way, but not exactly.
#Step 1
National_Health_Nutrition$DIQ350U <- as.factor(National_Health_Nutrition$DIQ350U)

#Step 2
levels(National_Health_Nutrition$DIQ350U) <- c(levels(National_Health_Nutrition$DIQ350U), "Per Day")
National_Health_Nutrition$DIQ350U[National_Health_Nutrition$DIQ350U=='1']  <- 'Per Day' 

levels(National_Health_Nutrition$DIQ350U) <- c(levels(National_Health_Nutrition$DIQ350U), "Per Week")
National_Health_Nutrition$DIQ350U[National_Health_Nutrition$DIQ350U=='2']  <- 'Per Week' 

levels(National_Health_Nutrition$DIQ350U) <- c(levels(National_Health_Nutrition$DIQ350U), "Per Month")
National_Health_Nutrition$DIQ350U[National_Health_Nutrition$DIQ350U=='3']  <- 'Per Month' 

levels(National_Health_Nutrition$DIQ350U) <- c(levels(National_Health_Nutrition$DIQ350U), "Per Year")
National_Health_Nutrition$DIQ350U[National_Health_Nutrition$DIQ350U=='4']  <- 'Per Year' 

levels(National_Health_Nutrition$DIQ350U) <- c(levels(National_Health_Nutrition$DIQ350U), "Missing")
National_Health_Nutrition$DIQ350U[is.na(National_Health_Nutrition$DIQ350U)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [66] <- "Sore_Measurement"

#The next column describes how long ago a patient had their pupils dialated for an exam. 

#The structure for the column is extremely peculiar, and is as follows:
#1	- Less than 1 month	
#2	- 1-12 months
#3	- 13-24 months
#4	- Greater than 2 years
#5	- Never
#7	- Refused
#9	- Don't know
#NA - Missing

#Because this data structure is so ambiguous and in pure ranges of data, we will treat it as catagorical - using our logic similar to previous catagorical variables, but renaming the columns to be more reader friendly.

#Step 1
National_Health_Nutrition$DIQ360 <- as.factor(National_Health_Nutrition$DIQ360)

#Step 2
levels(National_Health_Nutrition$DIQ360) <- c(levels(National_Health_Nutrition$DIQ360), "<1 month")
National_Health_Nutrition$DIQ360[National_Health_Nutrition$DIQ360=='1']  <- '<1 month' 

levels(National_Health_Nutrition$DIQ360) <- c(levels(National_Health_Nutrition$DIQ360), "1-12 months")
National_Health_Nutrition$DIQ360[National_Health_Nutrition$DIQ360=='2']  <- '1-12 months' 

levels(National_Health_Nutrition$DIQ360) <- c(levels(National_Health_Nutrition$DIQ360), "13-24 months")
National_Health_Nutrition$DIQ360[National_Health_Nutrition$DIQ360=='3']  <- '13-24 months' 

levels(National_Health_Nutrition$DIQ360) <- c(levels(National_Health_Nutrition$DIQ360), "Greater than 2 years")
National_Health_Nutrition$DIQ360[National_Health_Nutrition$DIQ360=='4']  <- "Greater than 2 years"

levels(National_Health_Nutrition$DIQ360) <- c(levels(National_Health_Nutrition$DIQ360), "Never")
National_Health_Nutrition$DIQ360[National_Health_Nutrition$DIQ360=='5']  <- "Never"

levels(National_Health_Nutrition$DIQ360) <- c(levels(National_Health_Nutrition$DIQ360), "Refused")
National_Health_Nutrition$DIQ360[National_Health_Nutrition$DIQ360=='7']  <- "Refused"

levels(National_Health_Nutrition$DIQ360) <- c(levels(National_Health_Nutrition$DIQ360), "Don't know")
National_Health_Nutrition$DIQ360[National_Health_Nutrition$DIQ360=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ360) <- c(levels(National_Health_Nutrition$DIQ360), "Missing")
National_Health_Nutrition$DIQ360[is.na(National_Health_Nutrition$DIQ360)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [67] <- "Time_Since_Dialation"
#The next column describes whether a patient has had retinopathy
#This column has the exact same structure as 'Told_prediabetes', and as such, will be dealt with the same exact way.

#Step 1
National_Health_Nutrition$DIQ080 <- as.factor(National_Health_Nutrition$DIQ080)

#Step 2
levels(National_Health_Nutrition$DIQ080) <- c(levels(National_Health_Nutrition$DIQ080), "Yes")
National_Health_Nutrition$DIQ080[National_Health_Nutrition$DIQ080=='1']  <- 'Yes' 

levels(National_Health_Nutrition$DIQ080) <- c(levels(National_Health_Nutrition$DIQ080), "No")
National_Health_Nutrition$DIQ080[National_Health_Nutrition$DIQ080=='2']  <- 'No' 

levels(National_Health_Nutrition$DIQ080) <- c(levels(National_Health_Nutrition$DIQ080), "Refused")
National_Health_Nutrition$DIQ080[National_Health_Nutrition$DIQ080=='7']  <- 'Refused'

levels(National_Health_Nutrition$DIQ080) <- c(levels(National_Health_Nutrition$DIQ080), "Don't know")
National_Health_Nutrition$DIQ080[National_Health_Nutrition$DIQ080=='9']  <- "Don't know"

levels(National_Health_Nutrition$DIQ080) <- c(levels(National_Health_Nutrition$DIQ080), "Missing")
National_Health_Nutrition$DIQ080[is.na(National_Health_Nutrition$DIQ080)]  <- "Missing"

#Step 3
colnames(National_Health_Nutrition) [68] <- "Retinopathy"


#Now time to turn all the variables into dummy variables (one hot encoding)

National_Health_Nutrition <- one_hot(setDT(National_Health_Nutrition, keep.rownames=TRUE, key=NULL, check.names=FALSE), dropCols = TRUE, dropUnusedLevels = TRUE)


## Now that the data has been cleaned, it is time to verify that the counts are still the same.
#I will pick a few random variables to check the counts and confirm they are accurate

#National_Health_Nutrition$Taking_Diabetic_Pills
#Should be 643
sum(National_Health_Nutrition$Taking_Diabetic_Pills_Yes)
#Should be 870
sum(National_Health_Nutrition$Taking_Diabetic_Pills_No)
#Should be 1
sum(National_Health_Nutrition$Taking_Diabetic_Pills_Refused)
#Should be 1
sum(National_Health_Nutrition$`Taking_Diabetic_Pills_Don't know`)
#Should be 8060
sum(National_Health_Nutrition$Taking_Diabetic_Pills_Missing)

#National_Health_Nutrition$Reason_Frequent_Urination
#Should be 105
sum(National_Health_Nutrition$Reason_Frequent_Urination_Yes)
#Should be 9470
sum(National_Health_Nutrition$Reason_Frequent_Urination_Missing)

#National_Health_Nutrition$Told_Diabetes
#Should be 856
sum(National_Health_Nutrition$Told_Diabetes_Yes)
#Should be 8568
sum(National_Health_Nutrition$Told_Diabetes_No)
#Should be 147
sum(National_Health_Nutrition$Told_Diabetes_Borderline)
#Should be 4
sum(National_Health_Nutrition$`Told_Diabetes_Don't know`)

#National_Health_Nutrition$Reason_Craving_Sweet
#Should be 11
sum(National_Health_Nutrition$Reason_Craving_Sweet_Yes)
#Should be 9564
sum(National_Health_Nutrition$Reason_Craving_Sweet_Missing)

#National_Health_Nutrition$Checked_A1C_Past_Year
#Should be 641
sum(National_Health_Nutrition$Checked_A1C_Past_Year_Yes)
#Should be 156
sum(National_Health_Nutrition$Checked_A1C_Past_Year_No)
#Should be 56
sum(National_Health_Nutrition$`Checked_A1C_Past_Year_Don't know`)
#Should be 8722
sum(National_Health_Nutrition$Checked_A1C_Past_Year_Missing)

#National_Health_Nutrition_Doctor_Recommended_SBP
#6666 should be 308+8729 (9037)
#7777 should be 2
#9999 should be 200
National_Health_Nutrition%>% count(Doctor_Recommended_SBP)

#National_Health_Nutrition$Reason_Increased_Fatigue
#Should be 134
sum(National_Health_Nutrition$Reason_Increased_Fatigue_Yes)
#Should be 9441
sum(National_Health_Nutrition$Reason_Increased_Fatigue_Missing)

#National_Health_Nutrition$Reason_Family_History
#Should be 1186
sum(National_Health_Nutrition$Reason_Family_History_Yes)
#Should be 8389
sum(National_Health_Nutrition$Reason_Family_History_Missing)

#National_Health_Nutrition$Told_prediabetes
#Should be 513
sum(National_Health_Nutrition$Told_prediabetes_Yes)
#Should be 5521
sum(National_Health_Nutrition$Told_prediabetes_No)
#Should be 11
sum(National_Health_Nutrition$`Told_prediabetes_Don't know`)
#Should be 3530
sum(National_Health_Nutrition$Told_prediabetes_Missing)
