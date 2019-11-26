-- 2)  Merge the tables Demographics, Conditions and TextMessages. 
-- Obtain the final dataset such that we have 1 Row per ID by choosing on the latest date when the text was sent (if sent on multiple days)


SELECT * 
INTO [rmandavilli].dem_cond_text 
from (select A.*, B.* from (select A.*, B.* from Demographics A
                        inner join Conditions B
                        on A.contactid=B.tri_patientid) A
inner join TextMessages B
on A.contactid=B.tri_contactId) A

select * from [rmandavilli].dem_cond_text

-- now delete all the duplicate IDs/grab the most recent date for the dem_cond_text table 

SELECT * 
INTO [rmandavilli].compiled_data 
from (
    select f.*, 
        min(TextSentDate) over (
        partition by contactid
        ) latest
    from [rmandavilli].dem_cond_text f
) A

DELETE FROM [rmandavilli].compiled_data  
WHERE TextSentDate > latest

select * from [rmandavilli].compiled_data


-- Create a table that equivalently mimics one hot encoding using conditional aggregation
SELECT * 
INTO [rmandavilli].one_hot_data
from (
    select contactid,
    count(case when tri_name = 'Activity Monitoring' then 1 end) as Activity_Monitoring,
    count(case when tri_name  = 'Hypertension' then 1 end) as Hypertension,
    count(case when tri_name  = 'Diabetes' then 1 end) as Diabetes,
    count(case when tri_name  = 'Congestive Heart Failure' then 1 end) as Congestive_Heart_Failure,
    count(case when tri_name  = 'COPD' then 1 end) as COPD,
    count(case when SenderName = 'System' then 1 end) as System,
    count(case when SenderName = 'Customer' then 1 end) as Customer,
    count(case when SenderName = 'Clinician' then 1 end) as Clinician
    -- another colors here
    from [rmandavilli].compiled_data 
    group by contactid
) A

select * from [rmandavilli].one_hot_data

-- the previous command got all the counts, now need to turn them all into 1s
update [rmandavilli].one_hot_data
set Hypertension = 1 
where Hypertension > 1

update [rmandavilli].one_hot_data
set Activity_Monitoring = 1 
where Activity_Monitoring > 1

update [rmandavilli].one_hot_data
set Diabetes = 1 
where Diabetes > 1

update [rmandavilli].one_hot_data
set Congestive_Heart_Failure = 1 
where Congestive_Heart_Failure > 1

update [rmandavilli].one_hot_data
set COPD = 1 
where COPD > 1

update [rmandavilli].one_hot_data
set System = 1 
where System > 1

update [rmandavilli].one_hot_data
set Customer = 1 
where Customer > 1

update [rmandavilli].one_hot_data
set Clinician = 1 
where Clinician > 1

select * from [rmandavilli].one_hot_data

-- now merge the one_hot_data table and the compiled_data table
SELECT * 
INTO [rmandavilli].final_data_table
from (
    select B.*, A.Activity_Monitoring, A.Hypertension, A.Diabetes, A.COPD, A.Congestive_Heart_Failure, A.System, A.Customer, A.Clinician from [rmandavilli].one_hot_data A
    inner join [rmandavilli].compiled_data B
    on A.contactid=B.contactid
) A

Select * from [rmandavilli].final_data_table

-- remove the problematic columns
ALTER TABLE [rmandavilli].final_data_table DROP COLUMN tri_name;
ALTER TABLE [rmandavilli].final_data_table DROP COLUMN SenderName;
ALTER TABLE [rmandavilli].final_data_table DROP COLUMN tri_contactId;
ALTER TABLE [rmandavilli].final_data_table DROP COLUMN latest;
ALTER TABLE [rmandavilli].final_data_table DROP COLUMN tri_patientid;

-- now remove any duplicate rows and store into a new data table
SELECT DISTINCT *
  FROM [rmandavilli].final_data_table
;with cte as
(
  select *,
    row_number() over(partition by [contactid]
                      order by  [TextSentDate] desc) rn
  from [rmandavilli].final_data_table
)
delete 
from cte 
where rn > 1;`

DROP TABLE [rmandavilli].dem_cond_text 
DROP TABLE [rmandavilli].compiled_data 
DROP Table [rmandavilli].one_hot_data
DROP Table [rmandavilli].final_data_table





