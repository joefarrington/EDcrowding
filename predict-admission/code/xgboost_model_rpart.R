library(tidymodels)
library(discrim)
library(dplyr)
library(lubridate)
library(xgboost)
library(parsnip)
library(caret)
library(tune)
library(dials)
library(scales)
library(yardstick)
library(tree)
library(reprtree)
library(party)
library(rpart)
library(rpart.plot)
library(randomForest)


#------------------------------------------------------------
#------------------------------------------------------------
#model training and prediction
# given the patient information, including current stage, flowsheet, labs, a model is trained with a certain period
# of history and then prediction is performed on the current state of the ED. Prediction will yield a TRUE/FALSE for
# each patient as well as a probability.
#
#
#------------------------------------------------------------
#------------------------------------------------------------
mat_fromfile = read.csv('F:/Saved/ENOCKUNG/ED project/dm_with_everything_simp_col.csv')
matrix = mat_fromfile
matrix$X=NULL
matrix = matrix[order(matrix$discharge),] # order matrix according to date
matrix$admission = strptime(matrix$admission,format = '%d/%m/%Y %H:%M')
matrix$discharge = strptime(matrix$discharge,format = '%d/%m/%Y %H:%M')
matrix = matrix %>% filter(difftime(discharge,admission)>0)

# remove columns that are not needed e.g. csn, mrn, ...
remove_col = c('mrn','department','hl7')#,'admission','discharge')
matrix = matrix[,setdiff(colnames(matrix),remove_col)]

# normalize age

# change label to factor ( or else classification does not work)
matrix$ADM = as.factor(matrix$ADM)

# insert date and assemble particular design matrix
D = strptime('20/07/2020 21:44',format = '%d/%m/%Y %H:%M')

# look at 30 days in the past e.g. 60*60*24*30 seconds
days = 30 
matrix$X.1=NULL
matrix$X = NULL


# design matrix ------------------------------------------------------------------------------------
# historical data are entries in which the latest appearance (in terms of discharge date ) must have happend before D
# so as to ensure that we realistically know whether he or she is admitted
latest_by_csn = matrix %>% group_by(csn) %>% arrange(desc(discharge), .by_group = TRUE) %>% filter(row_number()==1) %>% ungroup()
latest_by_csn = latest_by_csn %>% filter(difftime(discharge,D)<0 & difftime(discharge,D-60*60*24*days)>0)

dm = matrix %>% filter(is.element(csn,latest_by_csn$csn))
dm$csn = NULL
dm_colnames = setdiff(names(dm),c('ADM'))

# column names separate to room, demog, flow, labs so you can choose the dependence in the formula later
# also, if some columns have only one element e.g. patients have no readings for certain labs, then eliminate those
## columns from consideration

room = c('room')
demog = c('age','sex')
flow = dm_colnames[6:28]
labs = dm_colnames[29:length(dm_colnames)]

# for each column find out how many unique values there are
count = dm %>% summarise_all(n_distinct)
# isolate those that only have one unique value, which is NA
one_factor_cols = colnames(count[,count[1,]==1])

# remove column names that correspond to only one value
room <<- setdiff(room,one_factor_cols)
demog <<- setdiff(demog,one_factor_cols)
flow <<- setdiff(flow,one_factor_cols)
labs <- setdiff(labs,one_factor_cols)

# ------------------------------------------------------------------------------------------------------

# entries in matrix currently in ED for prediction to be applied
m_at_D = matrix %>% filter(difftime(admission,D)<0,
                           difftime(discharge,D)>0,
                           difftime(discharge,admission)>0)

# predict room by room, meaning filter out dm and matrix_during_date by rooms currently with patient
current_rooms = unique(m_at_D$room)
# for each room in current_rooms, filter out dm
current_room = current_rooms[1]
dm_room = dm %>% filter(dm$room == current_room)
m_at_D_room = m_at_D %>% filter(room == current_room)

# New Features --------------------------------------------
# There may be new packages out there that automatically create new features
# but not in the model currently used. The recommended way is to just create 
# our own based on certain guidelines. For example, here I add a column to the
# matrices containing the number of flow readings the patient has had.

col_index = which(is.element(colnames(dm_room),flow))
count_labs = function(x){sum((is.na(x[col_index])))}
dm_room$flow_count = apply(dm_room,1,function(x){sum((!is.na(x[col_index])))})
m_at_D_room$flow_count = apply(m_at_D_room,1,function(x){sum((is.na(x[col_index])))})
# -----------------------------------------------------------


# train data

formula = as.formula('ADM~1+age+sex')

# rpart ( includes variable importance ) ----------------
tree = rpart(formula, data = dm_room, method = 'class',  maxdepth = 4, cp=-1, na.action = na.rpart)
rpart.plot(tree,box.palette = "gray",nn=TRUE)
tree$variable.importance
# -------------------------------------------------------

output = classification_metrics(fit1,m_at_D_room)
output %>% conf_mat(truth,.pred_class)
write.csv(output,'F:/Saved/ENOCKUNG/ED project/prediction.csv')