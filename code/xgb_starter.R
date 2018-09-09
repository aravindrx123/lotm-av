################################################################################################################################################
#Set path and load required libraries
PATH<-"C:/Users/am16222/Documents/LOTM"
setwd(PATH)

source('helper_functions.R')
source("dependencies.R")
library(tidyr)

df <- read.csv("./data/train.csv")
campaign_data <- read.csv("./data/campaign_data.csv")

val <- read.csv("./data/test_BDIfz5B.csv")

#List of old users
old_users <- unique(val$user_id[val$user_id %in% df$user_id])
################################################################################################################################################

#Day sent
df$date <- as.Date(substr(df$send_date,start = 1,stop=10),"%d-%m-%Y")
df$day_of_week <- weekdays(as.Date(df$date))

#Time sent
df$time <- as.numeric(gsub(":","",substr(df$send_date,start = 12,stop=16)))

df$send_date <- NULL

##############################################################################################################################################
#Text features
campaign_data$email_body = unlist(lapply(campaign_data$email_body,FUN = function(x){str_replace_all(x, "[\r\n]" , " ")}))
campaign_data$subject = unlist(lapply(campaign_data$subject,FUN = function(x){str_replace_all(x, "[\r\n]" , " ")}))

campaign_data$email_length = unlist(lapply(campaign_data$email_body, FUN = function(x){nchar(as.character(x))}))
campaign_data$subject_length = unlist(lapply(campaign_data$subject, FUN = function(x){nchar(as.character(x))}))

##############################################################################################################################################
#Account age
acc_start <- df %>% group_by(user_id) %>% summarise(acc_start = min(date))
df <- merge(df,acc_start,all.x=T)
df$acc_age <- as.numeric(df$date-df$acc_start)
##############################################################################################################################################
#Days since last email
df <- df %>% arrange(user_id,date)
df  <- data.frame(df %>%
                  group_by(user_id) %>%
                  mutate(lag.value = lag(acc_age, n = 1, default = 0)))

df$days_since_last_email <- df$acc_age - df$lag.value
df$lag.value <- NULL

##############################################################################################################################################
#ix of email sent
df<- df %>% arrange(user_id,date)
df$ix <- ave(df$campaign_id,df$user_id, FUN = seq_along)
##############################################################################################################################################
#Number of previous clicks
df <- df %>% arrange(user_id,date)
df$num_previous_clicks <- ave(df$is_click,df$user_id, FUN = cumsum)  - df$is_click 

#Number of previous opens
df <- df %>% arrange(user_id,date)
df$num_previous_opens <- ave(df$is_open,df$user_id, FUN = cumsum)  - df$is_open

##############################################################################################################################################
# Merge with campaign data
df <- merge(df, campaign_data[,!names(campaign_data) %in% c('email_body','subject','email_url','email_clean')],all.x=T)

##############################################################################################################################################
temp <- df

#Remove users who have no opens in train data

dec0_open <- df %>% group_by(user_id) %>% summarise(num_opens = sum(is_open)) %>% filter(num_opens == 0)
dec0_click <- df %>% group_by(user_id) %>% summarise(num_clicks = sum(is_click)) %>% filter(num_clicks == 0)

df <- df[!df$user_id %in% dec0_open$user_id,]

#Calculate Lag features
#
source('lag_feats.R')
#


df$days_since_open = df$acc_age-df$days_seq
df$days_seq <- NULL

df$days_since_click <- df$acc_age-df$days_seq2 
df$days_seq2 <- NULL

#Remove date and account start date column
df$date <- NULL
df$acc_start <- NULL

# Remove users having no clicks
df <- df[!df$user_id %in% dec0_click$user_id,]

###############################################################################################################################################

#Pre-processing

feats <- setdiff(names(df),c("campaign_id","id","user_id","is_open","is_click" ))

#Categorical features
factor_feats <- c('communication_type','day_of_week','last_open_type','last_click_type')
#Numeric feats
numeric_feats <- feats[!feats %in% factor_feats]

Train_Columns <- feats

idcol <- 'id'
response <- 'is_click'

df <- df[,c(idcol,Train_Columns,response)]

#OHE factor variables
for (f in factor_feats){
  x_dummy = acm.disjonctif(df[f])
  df[f] = NULL
  df = cbind(df, x_dummy)
}

## XGBOOST
cv.nround <- 300
cv.nfold <- 3
MAX_DEPTH<-5
eta <- 0.05

#XGBOOST model parameters
param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc",
              "colsample_bytree"=0.9,
              "subsample"=0.8)

###############################################################################################################################################
#Use 3 fold cv to determine optimal boosting rounds
set.seed(5)
cv.g.gdbt<-xgb.cv(param=param, data=as.matrix(df[,!names(df) %in% c(idcol,response)]), label = df[,response], 
                  max.depth=MAX_DEPTH, eta=eta, nfold = cv.nfold, nrounds = cv.nround,
                  verbose = ifelse(interactive(), 1, 0))

# Final MOdel
maxIter<-which.max(as.matrix(cv.g.gdbt$evaluation_log)[,4]-as.matrix(cv.g.gdbt$evaluation_log)[,5])

mod = xgboost::xgboost(param=param, data=as.matrix(df[,!names(df) %in% c(idcol,response)]), label = df[,response], 
                       max.depth=MAX_DEPTH, eta=eta, nround = maxIter,
                       verbose = ifelse(interactive(), 1, 0))

#Variable importance
imp <- xgb.importance(model = mod, names(datatrain[,!names(datatrain)%in% c(idcol,response)]))

##################################################################################################################################################
#prepare test data

val$date <- as.Date(substr(val$send_date,start = 1,stop=10),"%d-%m-%Y")
val$day_of_week <- weekdays(as.Date(val$date))

#Time sent
val$time <- as.numeric(gsub(":","",substr(val$send_date,start = 12,stop=16)))

val$send_date <- NULL

##################################################################################################################################################
#Get user history to impute for lag features
history <- temp %>% 
            group_by(user_id) %>% 
            summarise(clicks = max(num_previous_clicks),
                      opens = max(num_previous_opens),
                      num_emails = max(ix),
                      last_open_day = max(acc_age))

temp <- temp %>% arrange(user_id) %>% arrange(desc(ix))
history2<- temp %>% group_by(user_id)  %>% filter(row_number()==1) %>% select(user_id,communication_type)
names(history2)[2] <- 'last_open_type'

history3 <- temp %>% 
  arrange(user_id) %>% 
  arrange(desc(ix)) %>% 
  filter(is_click==1) %>%
  group_by(user_id) %>%
  filter(row_number()==1) %>%
  select(user_id,communication_type)
names(history3)[2] <- 'last_click_type'

history4 <- temp %>%
  filter(is_click==1) %>%
  group_by(user_id) %>% 
  summarise(last_click_day = max(acc_age))

history <- Reduce(function(x, y) merge(x, y, all.x=T), list(history, history2, history3, history4))

##################################################################################################################################################

acc_start_new <- val %>% filter(!user_id %in% old_users) %>% group_by(user_id) %>% summarise(acc_start_new = min(date))
val <- merge(val,acc_start,all.x=T)
val <- merge(val,acc_start_new,all.x=T)
val$acc_age <- ifelse(val$user_id %in% old_users,val$date - val$acc_start,val$date - val$acc_start_new)

#Days since last email
val <- val %>% arrange(user_id,date)
val  <- data.frame(val %>%
                     group_by(user_id) %>%
                     mutate(lag.value = lag(acc_age, n = 1, default = 0)))

val$days_since_last_email <- val$acc_age - val$lag.value
val$lag.value <- NULL

val <- merge(val,history,all.x=T)

#Email ix
val<- val %>% arrange(user_id,date)
val$ix <- ave(val$campaign_id,val$user_id, FUN = seq_along)
#Update email ix for old users
val$ix[val$user_id %in% old_users] <- val$ix[val$user_id %in% old_users]+val$num_emails[val$user_id %in% old_users]

#number of previous clicks and opens
val$num_previous_clicks <- val$clicks
val$num_previous_opens <- val$opens
val$days_since_open <- val$acc_age-val$last_open_day
val$days_since_click <-val$acc_age-val$last_click_day 

val <- merge(val, campaign_data[,!names(campaign_data) %in% c('email_body','subject','email_url','email_clean')])

#Remove
val$num_emails <- NULL
val$clicks <- NULL
val$opens <- NULL
val$last_open_day <- NULL
val$last_click_day <- NULL
val$acc_start <- NULL
val$acc_start_new <- NULL
val$date <- NULL

val$last_open_type <- as.character(val$last_open_type)
val$last_open_type[is.na(val$last_open_type)] <- 'None'
val$last_open_type <- as.factor(val$last_open_type)


val$last_click_type <- as.character(val$last_click_type)
val$last_click_type[is.na(val$last_click_type)] <- 'None'
val$last_click_type <- as.factor(val$last_click_type)

##################################################################################################################################################
#Decile 0 users - click
dec0_ix <- which(val$user_id %in% dec0_click$user_id)

val <- val[,c(idcol,Train_Columns)]

#OHE factor variables
for (f in factor_feats){
  x_dummy = acm.disjonctif(val[f])
  val[f] = NULL
  val = cbind(val, x_dummy)
}

val$day_of_week.Saturday <- 0
val$day_of_week.Sunday <- 0

#Prediction submission
val_prob <- predict(mod,as.matrix(val[,!names(val) %in% c(idcol,response)]))

#Predict 0 for dec0 users
val_prob[dec0_ix] <- 0

#Write to csv
out <- data.frame(id = val$id, is_click = val_prob)
write.csv(out,"./sub19_xgb.csv",row.names = F)

