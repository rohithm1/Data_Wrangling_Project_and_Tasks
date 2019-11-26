-- 1
-- renaming the tri_age column to just age
EXEC sp_RENAME 'rmandavilli.demographics.tri_age' , 'Age', 'COLUMN'

-- renaming the gendercode column to just gender
EXEC sp_RENAME 'rmandavilli.demographics.gendercode' , 'Gender', 'COLUMN'

-- renaming the ContactID column to just ID
EXEC sp_RENAME 'rmandavilli.demographics.contactid' , 'ID', 'COLUMN'

-- renaming the address1_Stateorprovince to just State
EXEC sp_RENAME 'rmandavilli.demographics.address1_Stateorprovince ' , 'State', 'COLUMN'

-- renamind the tri_imaginecareenrollmentemailsentdate to just EmailSentDate
EXEC sp_RENAME 'rmandavilli.demographics.tri_imaginecareenrollmentemailsentdate ' , 'EmailSentDate', 'COLUMN'

-- tri_enrollmentcompletedate to just Completedate
EXEC sp_RENAME 'rmandavilli.demographics.tri_enrollmentcompletedate ' , 'Completedate', 'COLUMN'

-- adding the TimeToComplete column 
alter table rmandavilli.demographics
add TimeToComplete INT

--  change all the values in the TimeToComplete column as the difference between the two other dates
update rmandavilli.demographics
set TimeToComplete = DATEDIFF(d,try_convert(date, EmailSentDate,101) , try_convert(date, Completedate,101))

-- 2
-- adding the Enrollment_Status column
alter table rmandavilli.demographics
add Enrollment_Status NVARCHAR(50)

-- for each status with number '167410011' set the status to 'Complete'
update rmandavilli.demographics
set Enrollment_Status = 'Complete'
where tri_imaginecareenrollmentstatus=167410011

-- for each status with number '167410001' set the status to 'Email sent'
update rmandavilli.demographics
set Enrollment_Status = 'Email sent'
where tri_imaginecareenrollmentstatus=167410001

-- for each status with number '167410004' set the status to 'Non responder'
update rmandavilli.demographics
set Enrollment_Status = 'Non responder'
where tri_imaginecareenrollmentstatus=167410004

-- for each status with number '167410005' set the status to 'Facilitated Enrollment'
update rmandavilli.demographics
set Enrollment_Status = 'Facilitated Enrollment'
where tri_imaginecareenrollmentstatus=167410005

-- for each status with number '167410002' set the status to 'Incomplete Enrollments'
update rmandavilli.demographics
set Enrollment_Status = 'Incomplete Enrollments'
where tri_imaginecareenrollmentstatus=167410002

-- for each status with number '167410003' set the status to 'Opted Out'
update rmandavilli.demographics
set Enrollment_Status = 'Opted Out'
where tri_imaginecareenrollmentstatus=167410003

-- for each status with number '167410000' set the status to 'Unprocessed'
update rmandavilli.demographics
set Enrollment_Status = 'Unprocessed'
where tri_imaginecareenrollmentstatus=167410000

-- for each status with number '167410006' set the status to 'Second email'
update rmandavilli.demographics
set Enrollment_Status = 'Second email'
where tri_imaginecareenrollmentstatus=167410006

-- 3
-- adding the Sex column
alter table rmandavilli.demographics
add Sex NVARCHAR(50)

-- for each person with gender '2' set the status to 'female'
update rmandavilli.demographics
set Sex = 'female'
where Gender='2'

-- for each person with gender '1' set the status to 'male'
update rmandavilli.demographics
set Sex = 'male'
where Gender='1'

-- for each person with gender '167410000' set the status to 'other'
update rmandavilli.demographics
set Sex = 'other'
where Gender='167410000'

-- for each person with gender 'NULL' set the status to 'Unknown'
update rmandavilli.demographics
set Sex = 'Unknown'
where Gender='NULL'

-- 4
select * from rmandavilli.demographics

-- Adding the Age_Group Column
alter table rmandavilli.demographics
add Age_Group NVARCHAR(50)

-- create the first group interval for ages  (0-25)
update rmandavilli.demographics
set Age_Group='0-25' 
where Age between 0 and 25

-- create the second group interval for ages  (26-50)
update rmandavilli.demographics
set Age_Group='26-50' 
where Age between 26 and 50

-- create the third group interval for ages  (51-75)
update rmandavilli.demographics
set Age_Group='51-75' 
where Age between 51 and 75

-- create the fourth group interval for ages  (76-100)
update rmandavilli.demographics
set Age_Group='76-100' 
where Age between 76 and 100

