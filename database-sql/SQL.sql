-- create primary key on bed_moves ;

ALTER TABLE flow.bed_moves
 ADD COLUMN pk_bed_moves SERIAL PRIMARY KEY;

-- Create mrn as primary key on demographics

ALTER TABLE flow.demographics
  ADD CONSTRAINT demographics_pkey  PRIMARY KEY (mrn);

-- the above doesn't work because of null values

SELECT * FROM flow.demographics
  WHERE mrn ISNULL;  -- 1 row is returned

DELETE FROM flow.demographics
  WHERE mrn ISNULL;  -- 1 row deleted

ALTER TABLE flow.demographics
  ADD CONSTRAINT demographics_pkey  PRIMARY KEY (mrn);

-- set foreign key of bed_moves to be mrn on demographics table

ALTER TABLE flow.bed_moves
    ADD CONSTRAINT demographics_fkey FOREIGN KEY (mrn) REFERENCES flow.demographics (mrn);

-- create flag on bed_moves to denote paeds location

ALTER TABLE flow.bed_moves
 ADD COLUMN paed_location BOOLEAN DEFAULT FALSE;

UPDATE flow.bed_moves
  SET paed_location = TRUE
  WHERE room like ('PAEDS%');

SELECT count(*) FROM flow.bed_moves
  WHERE paed_location = TRUE;

-- create table ED_csn_summ, excluding paediatrics csns

 SELECT mrn, 
    csn,
    min(admission) AS ed_arrival_dttm,
    max(discharge) AS ed_discharge_dttm,
    COUNT(*) AS num_ed_rows 
  FROM flow.bed_moves
  WHERE department = 'UCH EMERGENCY DEPT'
  AND csn NOT IN (SELECT DISTINCT 
    csn
  FROM flow.bed_moves
  WHERE paed_location = TRUE)
  GROUP BY mrn, csn; 

--- note that the table below doesn't quite handle coalesce the way I want it
--- if there is any row with a discharge date, this will retun as max(discharge)
--- otherwise NOW() will be returned (this was be corrected below)

CREATE TABLE flow.ED_csn_summ AS
  SELECT mrn, 
    csn,
    min(admission) AS ed_arrival_dttm,
    coalesce(max(discharge), NOW()) AS ed_discharge_dttm, -- this was done when NOW() was 2020-09-03 11:50:48.259701
    COUNT(*) AS num_ed_rows 
  FROM flow.bed_moves
  WHERE department = 'UCH EMERGENCY DEPT'
  AND csn NOT IN (SELECT DISTINCT 
    csn
  FROM flow.bed_moves
  WHERE paed_location = TRUE)
  GROUP BY mrn, csn;


-- add primary key to this new table

ALTER TABLE flow.ED_csn_summ
 ADD COLUMN pk_ED_csn_summ SERIAL PRIMARY KEY;

-- update bed_moves with ED_csn_summ foreign key

ALTER TABLE flow.bed_moves
  ADD COLUMN fk_ED_csn_summ INTEGER;

UPDATE flow.bed_moves b
  SET fk_ed_csn_summ = e.pk_ed_csn_summ
  FROM flow.ED_csn_summ e
  WHERE b.mrn = e.mrn
  AND b.csn = e.csn

-- update ed_csn_summ where max discharge was null 
-- but there was also a row with a discharge date

SELECT * 
  FROM flow.ed_csn_summ 
  WHERE ed_discharge_dttm = '2020-09-03 11:50:48.259701' -- returns 43 rows


  UPDATE flow.ED_csn_summ
  SET ed_discharge_dttm = '2020-09-03 11:50:48.259701'
  WHERE pk_ED_csn_summ in 
    (SELECT fk_ed_csn_summ
      FROM flow.bed_moves
      WHERE department = 'UCH EMERGENCY DEPT'
      and discharge ISNULL 
      and paed_location = FALSE   
      ORDER by csn, admission);

SELECT * 
  FROM flow.ed_csn_summ 
  WHERE ed_discharge_dttm = '2020-09-03 11:50:48.259701' -- returns 167 rows


-- update flowsheets with bed_moves foreign key

ALTER TABLE flow.flowsheets
  ADD COLUMN fk_bed_moves INTEGER;

  -- note: this does not update where discharge is Null
  -- note also that this doesn't create a foreign key constraint

UPDATE flow.flowsheets f
  SET f.fk_bed_moves = b.pk_bed_moves
  FROM flow.bed_moves b
  WHERE f.mrn = b.mrn
  AND f.csn = b.csn
  AND f.flowsheet_datetime > b.admission
  AND f.flowsheet_datetime <= coalesce(b.discharge, NOW())

-- delete flowsheet rows where measurements not taken in ED

SELECT COUNT(*) FROM flow.flowsheets; -- returns 228,861,686

SELECT COUNT(*) FROM flow.flowsheets -- returns 69,674,579
  WHERE csn IN (
    SELECT csn FROM flow.ed_csn_summ
);

SELECT COUNT(f.flowsheet_datetime) -- returns 13,449,131
  FROM flow.flowsheets f,
  flow.bed_moves b     
  WHERE f.fk_bed_moves = b.pk_bed_moves
  and b.department = 'UCH EMERGENCY DEPT';

SELECT COUNT(f.flowsheet_datetime) -- -- returns 13,449,131
  FROM flow.flowsheets f,
  flow.bed_moves b     
  WHERE f.mrn = b.mrn
  AND f.csn = b.csn
  AND f.fk_bed_moves = b.pk_bed_moves
  and b.department = 'UCH EMERGENCY DEPT';

SELECT COUNT(*) -- returns 151,993,204
  FROM 
  flow.flowsheets f,
  flow.bed_moves b 
  WHERE f.mrn = b.mrn
  AND f.csn = b.csn
  AND f.fk_bed_moves = b.pk_bed_moves
  and b.department != 'UCH EMERGENCY DEPT';

SELECT COUNT(*) -- returns 63,296,035
  FROM 
  flow.flowsheets
  WHERE fk_bed_moves ISNULL
 
--- 151993204+13449131+63296035 = 228,738,370

-- So I have accounted for nearly all rows. Options for deleting from flowsheets
  -- (a) use a delete where csn not in ed_csn_summ then remove any non-ED rows
  -- (b) use a more specific delete where I only keep the 151993204 rows with foreign key matches to bed_moves
  --- however these are not quite correct as some with null discharge dates were not picked up in the first update

-- Delete using the wider method

DELETE FROM flow.flowsheets
    WHERE csn NOT IN (
    SELECT csn FROM flow.ed_csn_summ);

  -- OTHER CODE FOR REFERENCE

  -- trying to add num_ed_rows to bed_mpves - couldn't get syntax to work

SELECT csn, COUNT(*) AS num_ed_rows 
  FROM flow.bed_moves
  WHERE department = 'UCH EMERGENCY DEPT'
  GROUP BY csn; 


  WITH b2 as 
    (
    SELECT csn, COUNT(*) AS num_ed_rows 
    FROM flow.bed_moves
    WHERE department = 'UCH EMERGENCY DEPT'
    GROUP BY csn
    )
  UPDATE flow.bed_moves b1
  SET b1.num_ed_rows = b2.num_ed_rows 
  WHERE b1.csn = b2.csn;

-- Creating and then dropping a primary key

ALTER TABLE flow.demographics
  ADD COLUMN pk_demographics SERIAL PRIMARY KEY;

ALTER TABLE flow.demographics
  DROP CONSTRAINT demographics_pkey;

ALTER TABLE flow.demographics
  DROP COLUMN pk_demographics;



SELECT *
  FROM flow.ed_csn_summ e,
  flow.bed_moves b  
  WHERE b.mrn = e.mrn
  AND b.csn = e.csn
  AND b.fk_ed_csn_summ = e.pk_ed_csn_summ
  AND DATE(e.ed_arrival_dttm) > '2020-08-01'
  AND DATE(e.ed_arrival_dttm) <= '2020-08-31'

