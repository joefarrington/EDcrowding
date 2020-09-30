-- SQL to manage materialised tables on flow schema
  -=================================================

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

-- test SQL statement to create table ED_csn_summ, excluding paediatrics csns
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
--- otherwise NOW() will be returned (this was corrected below)
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

SELECT mrn, csn, ed_arrival_dttm, ed_discharge_dttm, num_ed_rows, pk_ed_csn_summ
  FROM flow.ed_csn_summ 
  WHERE ed_discharge_dttm = '2020-09-03 11:50:48.259701' -- returns 167 rows

-- testing a select statement for the update to correct the wrong discharge dates
  SELECT e.ed_discharge_dttm, b.fk_ed_csn_summ, b.admission, b.discharge, coalesce(b.discharge, b.admission + interval '1 days')
      FROM flow.bed_moves b,
           flow.ed_csn_summ e
      WHERE b.department = 'UCH EMERGENCY DEPT'
      and b.discharge ISNULL 
      and b.paed_location = FALSE   
      and b.mrn = e.mrn
      and b.csn = e.csn
      and b.fk_ed_csn_summ = e.pk_ed_csn_summ
      ORDER by b.csn, b.admission;

-- but not sure how to execute this as an update
-- however, I'm recalculating ED_discharge_dttm anyay in the code 

-- update flowsheets with bed_moves foreign key
-- note this was the SQL I ran on flowsheets (plural); Roma since created flowsheet (singular)

ALTER TABLE flow.flowsheets
  ADD COLUMN fk_bed_moves INTEGER;

UPDATE flow.flowsheets f
  SET f.fk_bed_moves = b.pk_bed_moves
  FROM flow.bed_moves b
  WHERE f.mrn = b.mrn
  AND f.csn = b.csn
  AND f.flowsheet_datetime > b.admission
    -- have now updated this to reflect the coalesce suggested by Roma
  AND f.flowsheet_datetime <= coalesce(b.discharge, b.admission + interval '1 days')

-- add foreign key constraint
ALTER TABLE flow.flowsheets
    ADD CONSTRAINT bed_moves_fkey FOREIGN KEY (fk_bed_moves) REFERENCES flow.bed_moves (pk_bed_moves);

-- identify number of flowsheet rows where measurements not taken in ED

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

  -- same query for flowsheet (singular)
SELECT COUNT(f.flowsheet_datetime) -- returns 13,193,096
  FROM flow.flowsheet f,
  flow.bed_moves b     
  WHERE f.fk_bed_moves = b.pk_bed_moves
  and b.department = 'UCH EMERGENCY DEPT'

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

-- Delete using the wider method - this was my original query
-- DELETE FROM flow.flowsheets
    --WHERE csn NOT IN (
    --SELECT csn FROM flow.ed_csn_summ);

  -- this was the query Roma ran - EXPLAIN is useful to stop it running by mistake!
EXPLAIN DELETE FROM flow.flowsheets
  WHERE NOT EXISTS  (
  SELECT csn FROM flow.ed_csn_summ);

-- then this one to remove the remaining flowsheets
EXPLAIN DELETE FROM flow.flowsheets f
  USING flow.bed_moves b     
  WHERE f.mrn = b.mrn
  AND f.csn = b.csn
  AND f.fk_bed_moves = b.pk_bed_moves
  and b.department <> 'UCH EMERGENCY DEPT'

  -- now trying the foreign key on flowsheets
SELECT f.mrn, f.csn,f.flowsheet_datetime, b.hl7_location FROM flow.flowsheet f,
  flow.bed_moves b
  WHERE b.csn = f.csn
  AND b.mrn = f.mrn
  AND b.pk_bed_moves = f.fk_bed_moves
  ORDER BY f.mrn, f.csn, f.flowsheet_datetime
  
  
  -- NOW UPDATING labs table 

ALTER TABLE flow.lab
  ADD COLUMN fk_bed_moves INTEGER;

UPDATE flow.lab l
  SET fk_bed_moves = b.pk_bed_moves
  FROM flow.bed_moves b
  WHERE l.mrn = b.mrn
  AND l.csn = b.csn
  AND l.result_datetime > b.admission
    -- using the coalesce suggested by Roma
  AND l.result_datetime <= coalesce(b.discharge, b.admission + interval '1 days')

-- add foreign key constraint
ALTER TABLE flow.lab
    ADD CONSTRAINT bed_moves_fkey FOREIGN KEY (fk_bed_moves) REFERENCES flow.bed_moves (pk_bed_moves);

  -- now trying the foreign key on lab table
SELECT l.mrn, l.csn,l.result_datetime, b.hl7_location, l.mapped_name
  FROM flow.lab l,
  flow.bed_moves b
  WHERE b.csn = l.csn
  AND b.mrn = l.mrn
  AND b.pk_bed_moves = l.fk_bed_moves
  ORDER BY l.mrn, l.csn, l.result_datetime


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

-- removing the number ED rows on bed_moves
ALTER TABLE flow.bed_moves
  DROP COLUMN num_ed_rows;

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
  AND DATE(e.ed_arrival_dttm) <= '2020-08-31';



  EXPLAIN DELETE FROM flow.flowsheets
  WHERE csn NOT IN (
    SELECT csn FROM flow.ed_csn_summ;

--read 14.1 of postgres manual for 
 EXPLAIN ANALYZE SELECT * FROM flow.flowsheets f LIMIT 10;

-- the queries Roma ran 

EXPLAIN DELETE FROM flow.flowsheets f
  USING flow.bed_moves b     
  WHERE f.mrn = b.mrn
  AND f.csn = b.csn
  AND f.fk_bed_moves = b.pk_bed_moves
  and b.department <> 'UCH EMERGENCY DEPT'

EXPLAIN UPDATE flow.flowsheets f
SET f.fk_bed_moves = b.pk_bed_moves
FROM flow.bed_moves b
WHERE f.mrn = b.mrn
AND f.csn = b.csn
AND f.flowsheet_datetime > b.admission
AND f.flowsheet_datetime <= coalesce(b.discharge, b.admission + interval '1 days');

EXPLAIN ALTER TABLE flow.flowsheets
ADD CONSTRAINT bed_moves_fkey
FOREIGN KEY (fk_bed_moves)
REFERENCES flow.bed_moves (pk_bed_moves);

EXPLAIN DELETE -- COUNT(f.flowsheet_datetime) -- -- returns 13,449,131
FROM flow.flowsheets f
USING flow.bed_moves b
WHERE f.mrn = b.mrn
AND f.csn = b.csn
AND f.fk_bed_moves = b.pk_bed_moves
and b.department <> 'UCH EMERGENCY DEPT';

VACUUM flow.flowsheets;