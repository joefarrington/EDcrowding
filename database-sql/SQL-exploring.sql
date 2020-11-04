select distinct(d.mrn), d.sex, d.birthdate, d.death_date, d.death_indicator
    from flow.demographics d,
    flow.ed_csn_summ e
    where e.num_ed_rows > 0
    and d.mrn = e.mrn;

  select count(*) from star.flowsheets
    where flowsheet_datetime > '2020-08-31 23:59:59';

select count(*) from star.demographics;

select * from flow.demographics where mrn = '03072196';
select * from star.encounters where mrn = '03072196' order by admission;
select * from star.encounters where csn = '1022090055';
select * from star.bed_moves where csn = '1022094806';

  select date(admission), count(distinct mrn) from flow.bed_moves 
    where department = 'UCH T00 AMBULATORY ECU'
    group by date(admission)

select min(admission) from star.encounters;


  select * from flow.bed_moves where mrn = '03072196' order by admission; --- example of patient who went on to AECU
  select * from flow.bed_moves where mrn = '40769448' order by admission; --- example of patient who didn't

select * from star.encounters where csn = '1015381809'; -- no records at all in the encounters view
select * from star.demographics where mrn = '40769448';


  select * from flow.bed_moves where mrn = '41103012' order by admission;
  select * from flow.demographics where mrn = '41103012' 

    select * from flow.bed_moves where mrn = '21167678' order by admission;
  select * from flow.demographics where mrn = '21167678'   ;

Select department, min(admission) from flow.bed_moves 
  group by department
  order by min(admission) DESC;
 

Select * from flow.bed_moves where fk_ed_csn_summ is not null;
  
Select room, min(admission) from star.bed_moves 
  where department = 'UCH EMERGENCY DEPT'
  and admission > '2020-09-01' 
  group by room
  order by min(admission) DESC
 