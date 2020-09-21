#------------------------------------------------------------
#------------------------------------------------------------
#simplify column names for flowsheet
# The column names for flowsheets are too long
# In this program, I gave a shortening to all these names.
#
# 1. reading matrix from dm_with_demog_flowsheet_labs
# 2. simplify column names for flowsheet using glossary below
#------------------------------------------------------------
#------------------------------------------------------------

# changing flowsheet column names into simplified names

dm = read.csv('F:/Saved/ENOCKUNG/ED project/dm_with_demog_flowsheet_labs.csv')

col_needing_simplify = colnames(dm)[which(nchar(colnames(dm))>10)]

simp_code = c('hl7','oxygen','temperature','resp','NEWS','bp','oxy_conc','pain_move','resp_assist',
              'tidal_vol','vent','pain_rest','oxy_deliv','GCS','IBP','epi_infusion','RASS','oxygen_flow',
              'level_block','PEEP','bromage','non-IBP')

colnames(dm)[which(nchar(colnames(dm))>10)] = simp_code
print(colnames(dm))

write.csv(dm,'F:/Saved/ENOCKUNG/ED project/dm_with_everything_simp_col.csv')
# oxygen - "Oxygen.saturation.in.Blood"
# temperature - "Body.temperature"
# resp - "Respiratory.rate"                   
# NEWS - "National.early.warning.score"
# bp - "BLOOD.PRESSURE"
# oxy_conc - "Oxygen.concentration.breathed"
# pain_move - "PAIN.SCORE.AT.MOVEMENT"
# resp_assist - "Respiratory.assist.status"
# tidal_vol - "Tidal.volume"
# vent - "VENTILATOR.MODE"
# pain_rest - "PAIN.SCORE.AT.REST"
# oxy_deliv - "OXYGEN.DELIVERY.METHOD"
# GCS - "Glasgow.coma.score.total"
# IBP - "Invasive.mean.arterial.pressure"
# epi_infusion - "EPIDURAL.INFUSION.RATE"
# RASS - "RICHMOND.AGITATION.SEDATION.SCORE"
# oxygen_flow - "Delivered.oxygen.flow.rate"
# level_block - "LEVEL.OF.BLOCK"
# PEEP - "Positive.end.expiratory.pressure"
# bromage - "BROMAGE.RIGHT.LEG"
# non-IBP - "Non.invasive.mean.arterial.pressure"