setwd("~/Dropbox/cpd_excessive_force")
library(lubridate)
library(tidyverse)
library(dplyr)
complaints=read.csv("complaints.csv")
units=read.csv("units.csv")
officers=read.csv("officers.csv")
officers$OfficerID=officers$link_UID
officers_comp=read.csv("officers_comp.csv")

officers_merge=merge(officers_comp,officers,by="OfficerID",all.x=T)
officers=officers_merge

units$unit_end_date=as.character(units$unit_end_date)
units$unit_end_date[units$unit_end_date==""]="1/1/20"
units$start=mdy(units$unit_start_date)
units$end=mdy(units$unit_end_date)

officers$appointed_date=as.character(officers$appointed_date)
officers$resignation_date=as.character(officers$resignation_date)
officers$resignation_date[officers$resignation_date==""]="1/1/20"

officers$start=mdy(officers$appointed_date)
officers$end=mdy(officers$resignation_date)

officers=officers[!is.na(officers$start),]

complaints$start=ymd(complaints$StartDate)

start=ymd("2009-12-01")
for(its in 1:84){
    
    start=start+months(1)
    end=start+months(1)
    
    units_temp=units[units$start<start&units$end>start,]
    units_temp$OfficerID=units_temp$link_UID
    officers_temp=officers[officers$start<start,]
    complaints_temp=complaints[complaints$start<start,]
    complaints_label=complaints[complaints$start>=start&complaints$start<=end,]
    
    complaints_temp$count=1
    complaints_temp$pp10=exp(-.1*as.numeric(start-complaints_temp$start))
    complaints_temp$pp100=exp(-.01*as.numeric(start-complaints_temp$start))
    complaints_temp$pp1000=exp(-.001*as.numeric(start-complaints_temp$start))
    complaints_temp$count_year=as.numeric(as.numeric(start-complaints_temp$start)<365)
    
    complaints_agg = complaints_temp %>% group_by(OfficerID) %>% 
      dplyr::summarize(count=sum(count),pp10=sum(pp10),pp100=sum(pp100),pp1000=sum(pp1000),count_year=sum(count_year))
    
    complaints_label$label=1
    label_agg = complaints_label %>% group_by(OfficerID) %>% 
      dplyr::summarize(label=sum(label))
    
    unit_agg=units_temp %>% group_by(OfficerID) %>% dplyr::summarize(unit=mean(unit))
    
    complaints_merge=merge(complaints_agg,officers_temp,by="OfficerID",all.x=T)
    complaints_merge=merge(complaints_merge,unit_agg,by="OfficerID",all.x=T)
    complaints_merge$age=as.numeric(year(start))-as.numeric(complaints_merge$birth_year)
    
    complaints_merge=cbind(complaints_merge[,1:6],complaints_merge[,c("Gender","Race","age","unit")])
    complaints_merge=merge(complaints_merge,label_agg,by="OfficerID",all.x=T)
    complaints_merge$label[is.na(complaints_merge$label)]=0
    
    complaints_merge$start=start
    complaints_merge$end=end
    complaints_merge$month_id=its
    #network features###############
    unique_officers = unique(complaints_temp$OfficerID)
    for(current_officer in unique_officers){
      net_officer_complaints_temp = filter(complaints_temp, OfficerID == current_officer)
      officer_CRIDs = unique(net_officer_complaints_temp$CRID)
      co_complains_temp = filter(complaints_temp, CRID %in% officer_CRIDs) 
      
      co_complains_agg = co_complains_temp %>% group_by(CRID) %>% 
        dplyr::summarize(co_count=sum(count),co_pp10=sum(pp10),co_pp100=sum(pp100),co_pp1000=sum(pp1000),co_count_year=sum(count_year))
      num_officer_complains = nrow(co_complains_agg)
      #print(sum(co_complains_agg$co_count))
      complaints_merge$co_count[complaints_merge$OfficerID==current_officer] = sum(co_complains_agg$co_count) - (num_officer_complains*complaints_merge$count)
      complaints_merge$co_count_year[complaints_merge$OfficerID==current_officer] = sum(co_complains_agg$co_count_year) - (num_officer_complains*complaints_merge$count_year)
      complaints_merge$co_pp10[complaints_merge$OfficerID==current_officer] = sum(co_complains_agg$co_pp10) - (num_officer_complains*complaints_merge$pp10)
      complaints_merge$co_pp100[complaints_merge$OfficerID==current_officer] = sum(co_complains_agg$co_pp100) - (num_officer_complains*complaints_merge$pp100)
      complaints_merge$co_pp1000[complaints_merge$OfficerID==current_officer] = sum(co_complains_agg$co_pp1000) - (num_officer_complains*complaints_merge$pp1000)
      unique_net_officers = unique(co_complains_temp$OfficerID)
      complaints_merge$co_officers = length(unique_net_officers) - 1
      #break
    }
    #network features###############
    
    if(its==1){
      ltr=complaints_merge
    }else{
      ltr=rbind(ltr,complaints_merge)
    }
    print(its)
    #break
}

ltr$binary=as.numeric(ltr$label>0)
ltr$Female=as.numeric(ltr$Gender=="F")


write.csv(ltr, file ="ltr.csv")
ltr = read.csv("ltr.csv")


##########################################
model=glm(binary~count+pp10+pp100+pp1000+count_year+Female+Race+age,data=ltr[ltr$month_id<15,],family="binomial")
model$coefficients
coef(summary(model))
ltr$pred=predict(model,ltr,type="response")

library(pROC)
library(randomForest)

rocc <- roc(ltr$binary[ltr$month_id>=15&ltr$Race=="White"], ltr$pred[ltr$month_id>=15&ltr$Race=="White"])
rocc2 <- roc(ltr$binary[ltr$month_id>=15&ltr$Race=="Black"], ltr$pred[ltr$month_id>=15&ltr$Race=="Black"])


auc(rocc)
auc(rocc2)

sum(ltr$pred>.01&ltr$binary==1,na.rm=T)/sum(ltr$binary==1,na.rm=T)
sum(ltr$pred>.01&ltr$binary==1,na.rm=T)/sum(ltr$binary==1,na.rm=T)

library(MLmetrics)
 
roc_val = roc(ltr$binary, ltr$pred)
auc_val = auc(roc_val)

prediction = max(min(ltr$pred, 1-10^-15,na.rm = TRUE), 10^-15)
ll = LogLoss(y_pred = prediction, y_true = ltr$binary)

#Residual sum of squares:
RSS = c(crossprod(model$residuals))

#Mean squared error:
MSE = RSS / length(model$residuals)

#Root MSE:
RMSE = sqrt(MSE)

print("auc    logloss       rmse         mse")
sprintf("%f    %f       %f         %f", auc_val, ll, RMSE, MSE)


library(h2o)
h2o.init()

ltr$White=as.numeric(ltr$Race=="White")
ltr$Black=as.numeric(ltr$Race=="Black")
ltr$Hispanic=as.numeric(ltr$Race=="Hispanic")

ltr_feat=ltr[,c("count","pp10","pp100","pp1000","count_year","Female","White","Black","Hispanic","binary")]
ltr_feat$binary=as.factor(ltr_feat$binary)
test=as.h2o(ltr_feat[ltr$month_id>=15,])
train=as.h2o(ltr_feat[ltr$month_id<15,])
y="binary"
x <- setdiff(names(train), y)

aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  leaderboard_frame = test,
                  max_models = 25,
                  seed = 1)

lb <- aml@leaderboard
print(lb, n = nrow(lb))
aml_pred <- h2o.predict(aml, test)  # predict(aml, test) also works

aml_result <- as.data.frame(aml_pred)
write.csv(aml_result, file="results/aml_pred.csv")
################################

net_model=glm(binary~co_count+co_pp10+co_pp100+co_pp1000+co_count_year+count+pp10+pp100+pp1000+count_year+Female+Race+age,data=ltr[ltr$month_id<15,],family="binomial")
#net_model$coefficients

coef(summary(net_model))
ltr$co_pred=predict(net_model,ltr,type="response")

write.csv(ltr, file ="results/ltr_with_glm_predictions_15_months.csv") 
#library(pROC)
#library(randomForest)

co_rocc <- roc(ltr$binary[ltr$month_id>=15&ltr$Race=="White"], ltr$co_pred[ltr$month_id>=15&ltr$Race=="White"])
co_rocc2 <- roc(ltr$binary[ltr$month_id>=15&ltr$Race=="Black"], ltr$co_pred[ltr$month_id>=15&ltr$Race=="Black"])


auc(co_rocc)
auc(co_rocc2)

sum(ltr$co_pred>.01&ltr$binary==1,na.rm=T)/sum(ltr$binary==1,na.rm=T)
sum(ltr$co_pred>.01&ltr$binary==1,na.rm=T)/sum(ltr$binary==1,na.rm=T)

#library(MLmetrics)

co_roc_val = roc(ltr$binary, ltr$co_pred)
co_auc_val = auc(co_roc_val)

co_prediction = max(min(ltr$co_pred, 1-10^-15,na.rm = TRUE), 10^-15)
co_ll = LogLoss(y_pred = co_prediction, y_true = ltr$binary)

#Residual sum of squares:
co_RSS = c(crossprod(net_model$residuals))

#Mean squared error:
co_MSE = co_RSS / length(net_model$residuals)

#Root MSE:
co_RMSE = sqrt(co_MSE)

print("auc    logloss       rmse         mse")
sprintf("%f    %f       %f         %f", co_auc_val, co_ll, co_RMSE, co_MSE)


library(h2o)
h2o.init()

ltr$White=as.numeric(ltr$Race=="White")
ltr$Black=as.numeric(ltr$Race=="Black")
ltr$Hispanic=as.numeric(ltr$Race=="Hispanic")

co_ltr_feat=ltr[,c("co_count","co_pp10","co_pp100","co_pp1000","co_count_year", "count","pp10","pp100","pp1000","count_year","Female","White","Black","Hispanic","binary")]
co_ltr_feat$binary=as.factor(co_ltr_feat$binary)

start.time <- Sys.time()



co_test=as.h2o(co_ltr_feat[ltr$month_id>=15,])
co_train=as.h2o(co_ltr_feat[ltr$month_id<15,])
y="binary"
co_x <- setdiff(names(co_train), y)

co_aml <- h2o.automl(x = co_x, y = y,
                  training_frame = co_train,
                  leaderboard_frame = co_test,
                  max_models = 20,
                  seed = 1)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
co_lb <- co_aml@leaderboard
print(co_lb, n = nrow(co_lb))

# To generate predictions on a test set, you can make predictions
# directly on the `"H2OAutoML"` object or on the leader model
# object directly
co_aml_pred <- h2o.predict(co_aml, co_test)  # predict(aml, test) also works

# or:
#co_pred <- h2o.predict(aml@leader, co_test)
co_aml_result <- as.data.frame(co_aml_pred)
write.csv(co_aml_result, file="results/co_aml_pred.csv")

########################Generating binary features for risk slim#####################
ltr_slim = read.csv("ltr.csv", stringsAsFactors = FALSE)

# Taking care of missing data
ltr_slim$Gender[is.na(ltr_slim$Gender)] <- "Unknown"
ltr_slim$Race[is.na(ltr_slim$Race)] <- "Unknown"
#ltr_slim$Female[is.na(ltr_slim$Female)] <- 2
ltr_slim$age = ifelse(is.na(ltr_slim$age),
                     ave(ltr_slim$age, FUN = function(x) mean(x, na.rm = TRUE)),
                     ltr_slim$age) 

# Encoding categorical data
#ltr_slim$Gender = factor(ltr_slim$Gender,
#                         levels = c('F', 'M', 'Unknown'),
#                         labels = c(1, 0, 2))
#Races = unique(ltr_slim$Race) #c("White", "Asian/Pacific","Black" ,"Hispanic","Native American/Alaskan Native","Unknown")  
#ltr_slim$Race = factor(ltr_slim$Race,
#                           levels = c("White", "Asian/Pacific","Black" ,"Hispanic","Native American/Alaskan Native","Unknown"),
#                           labels = c(1, 2, 3, 4, 5,6))

#ltr_slim = ltr_slim[,c("binary","co_count","co_pp10","co_pp100","co_pp1000","co_count_year", "count","pp10","pp100","pp1000","count_year","Female","Race")]
ltr_slim[is.na(ltr_slim)] <- 0

#Binary features for gender
ltr_slim$Female=as.numeric(ltr_slim$Gender=="F")
#Binary features for race
ltr_slim$White = as.numeric(ltr_slim$Race =="White")
ltr_slim$Black = as.numeric(ltr_slim$Race =="Black")
ltr_slim$Hispanic = as.numeric(ltr_slim$Race =="Hispanic")

# {
#   ltr_slim$White_m <- ifelse((ltr_slim$White=1 & ltr_slim$Female==0), 1, 0) 
#   ltr_slim$White_f <- ifelse((ltr_slim$White=1 & ltr_slim$Female==1), 1, 0)
#   
#   ltr_slim$WBlack_m <- ifelse((ltr_slim$Black=1 & ltr_slim$Female==0), 1, 0) 
#   ltr_slim$WBlack_f <- ifelse((ltr_slim$Black=1 & ltr_slim$Female==1), 1, 0)
#   
#   ltr_slim$Hispanic_m <- ifelse((ltr_slim$Hispanic=1 & ltr_slim$Female==0), 1, 0) 
#   ltr_slim$Hispanic_f <- ifelse((ltr_slim$Hispanic=1 & ltr_slim$Female==1), 1, 0)
# }

#get the range of each column
ranges= data.frame(min=sapply(ltr_slim,min),max=sapply(ltr_slim,max))

#Binary features for age
{
ltr_slim$Age_l30 <- ifelse(ltr_slim$age<30, 1, 0)
ltr_slim$Age_l40_g30 <- ifelse((ltr_slim$age >=30 & ltr_slim$age <40), 1, 0)
# ltr_slim$Age_l50_g30 <- ifelse((ltr_slim$age >=30 & ltr_slim$age <=50), 1, 0)
# ltr_slim$Age_l60_g30 <- ifelse((ltr_slim$age >=30 & ltr_slim$age <=60), 1, 0)
# ltr_slim$Age_l70_g30 <- ifelse((ltr_slim$age >=30 & ltr_slim$age <=70), 1, 0)
# ltr_slim$Age_g30 <- ifelse(ltr_slim$age>=30, 1, 0)

# ltr_slim$Age_l40 <- ifelse(ltr_slim$age<=40, 1, 0)
ltr_slim$Age_l50_g40 <- ifelse((ltr_slim$age >=40 & ltr_slim$age <50), 1, 0)
# ltr_slim$Age_l60_g40 <- ifelse((ltr_slim$age >=40 & ltr_slim$age <=60), 1, 0)
# ltr_slim$Age_l70_g40 <- ifelse((ltr_slim$age >=40 & ltr_slim$age <=70), 1, 0)
# ltr_slim$Age_g40 <- ifelse(ltr_slim$age>=40, 1, 0)

# ltr_slim$Age_l50 <- ifelse(ltr_slim$age<=50, 1, 0)
# ltr_slim$Age_l60_g50 <- ifelse((ltr_slim$age >=50 & ltr_slim$age <=60), 1, 0)
# ltr_slim$Age_l70_g50 <- ifelse((ltr_slim$age >=50 & ltr_slim$age <=70), 1, 0)
ltr_slim$Age_g50 <- ifelse(ltr_slim$age>=50, 1, 0)
  
  # #MALE interaction
  # ltr_slim$Age_l30_m <- ifelse((ltr_slim$age<=30 & ltr_slim$Female==0), 1, 0) 
  # ltr_slim$Age_l40_g30_m <- ifelse((ltr_slim$age >=30 & ltr_slim$age <=40 & ltr_slim$Female==0), 1, 0) 
  # ltr_slim$Age_l50_g30_m <- ifelse((ltr_slim$age >=30 & ltr_slim$age <=50 & ltr_slim$Female==0), 1, 0) 
  # ltr_slim$Age_l60_g30_m <- ifelse((ltr_slim$age >=30 & ltr_slim$age <=60 & ltr_slim$Female==0), 1, 0) 
  # ltr_slim$Age_l70_g30_m <- ifelse((ltr_slim$age >=30 & ltr_slim$age <=70 & ltr_slim$Female==0), 1, 0) 
  # ltr_slim$Age_g30_m <- ifelse((ltr_slim$age>=30 & ltr_slim$Female==0), 1, 0) 
  # 
  # ltr_slim$Age_l40_m <- ifelse(ltr_slim$age<=40 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$Age_l50_g40_m <- ifelse((ltr_slim$age >=40 & ltr_slim$age <=50 & ltr_slim$Female==0), 1, 0) 
  # ltr_slim$Age_l60_g40_m <- ifelse((ltr_slim$age >=40 & ltr_slim$age <=60 & ltr_slim$Female==0), 1, 0) 
  # ltr_slim$Age_l70_g40_m <- ifelse((ltr_slim$age >=40 & ltr_slim$age <=70 & ltr_slim$Female==0), 1, 0) 
  # ltr_slim$Age_g40_m <- ifelse(ltr_slim$age>=40, 1, 0) 
  # 
  # ltr_slim$Age_l50_m <- ifelse(ltr_slim$age<=50 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$Age_l60_g50_m <- ifelse((ltr_slim$age >=50 & ltr_slim$age <=60 & ltr_slim$Female==0), 1, 0) 
  # ltr_slim$Age_l70_g50_m <- ifelse((ltr_slim$age >=50 & ltr_slim$age <=70 & ltr_slim$Female==0), 1, 0) 
  # ltr_slim$Age_g50_m <- ifelse(ltr_slim$age>=50, 1, 0)
  # 
  # #Female interaction
  # ltr_slim$Age_l30_f <- ifelse((ltr_slim$age<=30 & ltr_slim$Female==1), 1, 0) 
  # ltr_slim$Age_l40_g30_f <- ifelse((ltr_slim$age >=30 & ltr_slim$age <=40 & ltr_slim$Female==1), 1, 0) 
  # ltr_slim$Age_l50_g30_f <- ifelse((ltr_slim$age >=30 & ltr_slim$age <=50 & ltr_slim$Female==1), 1, 0) 
  # ltr_slim$Age_l60_g30_f <- ifelse((ltr_slim$age >=30 & ltr_slim$age <=60 & ltr_slim$Female==1), 1, 0) 
  # ltr_slim$Age_l70_g30_f <- ifelse((ltr_slim$age >=30 & ltr_slim$age <=70 & ltr_slim$Female==1), 1, 0) 
  # ltr_slim$Age_g30_f <- ifelse((ltr_slim$age>=30 & ltr_slim$Female==1), 1, 0) 
  # 
  # ltr_slim$Age_l40_f <- ifelse(ltr_slim$age<=40 & ltr_slim$Female==1, 1, 0) 
  # ltr_slim$Age_l50_g40_f <- ifelse((ltr_slim$age >=40 & ltr_slim$age <=50 & ltr_slim$Female==1), 1, 0) 
  # ltr_slim$Age_l60_g40_f <- ifelse((ltr_slim$age >=40 & ltr_slim$age <=60 & ltr_slim$Female==1), 1, 0) 
  # ltr_slim$Age_l70_g40_f <- ifelse((ltr_slim$age >=40 & ltr_slim$age <=70 & ltr_slim$Female==1), 1, 0) 
  # ltr_slim$Age_g40_f <- ifelse(ltr_slim$age>=40 & ltr_slim$Female==1, 1, 0) 
  # 
  # ltr_slim$Age_l50_f <- ifelse(ltr_slim$age<=50 & ltr_slim$Female==1, 1, 0) 
  # ltr_slim$Age_l60_g50_f <- ifelse((ltr_slim$age >=50 & ltr_slim$age <=60 & ltr_slim$Female==1), 1, 0) 
  # ltr_slim$Age_l70_g50_f <- ifelse((ltr_slim$age >=50 & ltr_slim$age <=70 & ltr_slim$Female==1), 1, 0) 
  # ltr_slim$Age_g50_f <- ifelse(ltr_slim$age>=50 & ltr_slim$Female==1, 1, 0) 
}
#Binary features for number of complains
{
ltr_slim$count_l1 <- ifelse(ltr_slim$count<=1, 1, 0)
ltr_slim$count_l2 <- ifelse(ltr_slim$count<=2, 1, 0)
ltr_slim$count_l3 <- ifelse(ltr_slim$count<=3, 1, 0)
ltr_slim$count_l4 <- ifelse(ltr_slim$count<=4, 1, 0)
ltr_slim$count_l5 <- ifelse(ltr_slim$count<=5, 1, 0)

ltr_slim$count_l2_g1 <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=2), 1, 0)
ltr_slim$count_l3_g1 <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=3), 1, 0)
ltr_slim$count_l4_g1 <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=4), 1, 0)
ltr_slim$count_l5_g1 <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=5), 1, 0)
ltr_slim$count_g1 <- ifelse((ltr_slim$count >=1), 1, 0)

ltr_slim$count_l3_g2 <- ifelse((ltr_slim$count >=2 & ltr_slim$count <=3), 1, 0)
ltr_slim$count_l4_g2 <- ifelse((ltr_slim$count >=2 & ltr_slim$count <=4), 1, 0)
ltr_slim$count_l5_g2 <- ifelse((ltr_slim$count >=2 & ltr_slim$count <=5), 1, 0)
ltr_slim$count_g2 <- ifelse((ltr_slim$count >=2), 1, 0)

ltr_slim$count_l4_g3 <- ifelse((ltr_slim$count >=3 & ltr_slim$count <=4), 1, 0)
ltr_slim$count_l5_g3 <- ifelse((ltr_slim$count >=3 & ltr_slim$count <=5), 1, 0)
ltr_slim$count_g3 <- ifelse((ltr_slim$count >=3), 1, 0)

ltr_slim$count_l5_g4 <- ifelse((ltr_slim$count >=4 & ltr_slim$count <=5), 1, 0)
ltr_slim$count_g4 <- ifelse((ltr_slim$count >=4), 1, 0)

ltr_slim$count_g5 <- ifelse((ltr_slim$count >=5), 1, 0)

ltr_slim$count_l10_g5 <- ifelse((ltr_slim$count >5 & ltr_slim$count <=10), 1, 0)
ltr_slim$count_l57_g10 <- ifelse((ltr_slim$count >10 & ltr_slim$count <=57), 1, 0)

  # #Male interaction
  # ltr_slim$count_l1_m <- ifelse(ltr_slim$count<=1 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$count_l2_m <- ifelse(ltr_slim$count<=2 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$count_l3_m <- ifelse(ltr_slim$count<=3 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$count_l4_m <- ifelse(ltr_slim$count<=4 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$count_l5_m <- ifelse(ltr_slim$count<=5 & ltr_slim$Female==0, 1, 0) 
  # 
  # ltr_slim$count_l2_g1_m <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=2 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_l3_g1_m <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=3 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_l4_g1_m <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=4 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_l5_g1_m <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_g1_m <- ifelse((ltr_slim$count >=1 & ltr_slim$Female==0), 1, 0) 
  # 
  # ltr_slim$count_l3_g2_m <- ifelse((ltr_slim$count >=2 & ltr_slim$count <=3 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_l4_g2_m <- ifelse((ltr_slim$count >=2 & ltr_slim$count <=4 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_l5_g2_m <- ifelse((ltr_slim$count >=2 & ltr_slim$count <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_g2_m <- ifelse((ltr_slim$count >=2 & ltr_slim$Female==0), 1, 0) 
  # 
  # ltr_slim$count_l4_g3_m <- ifelse((ltr_slim$count >=3 & ltr_slim$count <=4 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_l5_g3_m <- ifelse((ltr_slim$count >=3 & ltr_slim$count <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_g3_m <- ifelse((ltr_slim$count >=3 & ltr_slim$Female==0), 1, 0) 
  # 
  # ltr_slim$count_l5_g4_m <- ifelse((ltr_slim$count >=4 & ltr_slim$count <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_g4_m <- ifelse((ltr_slim$count >=4 & ltr_slim$Female==0), 1, 0)
  # 
  # ltr_slim$count_g5_m <- ifelse((ltr_slim$count >=5 & ltr_slim$Female==0), 1, 0)
  # 
  # ltr_slim$count_l10_g5_m <- ifelse((ltr_slim$count >5 & ltr_slim$count <=10 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_l57_g10_m <- ifelse((ltr_slim$count >10 & ltr_slim$count <=57 & ltr_slim$Female==0), 1, 0)
  # 
  # #Female interaction
  # ltr_slim$count_l1_f <- ifelse(ltr_slim$count<=1 &  ltr_slim$Female==1, 1, 0) 
  # ltr_slim$count_l2_f <- ifelse(ltr_slim$count<=2 &  ltr_slim$Female==1, 1, 0) 
  # ltr_slim$count_l3_f <- ifelse(ltr_slim$count<=3 &  ltr_slim$Female==1, 1, 0) 
  # ltr_slim$count_l4_f <- ifelse(ltr_slim$count<=4 &  ltr_slim$Female==1, 1, 0) 
  # ltr_slim$count_l5_f <- ifelse(ltr_slim$count<=5 &  ltr_slim$Female==1, 1, 0) 
  # 
  # ltr_slim$count_l2_g1_f <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=2 &  ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_l3_g1_f <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=3 &  ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_l4_g1_f <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=4 &  ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_l5_g1_f <- ifelse((ltr_slim$count >=1 & ltr_slim$count <=5 &  ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_g1_f <- ifelse((ltr_slim$count >=1 &  ltr_slim$Female==1), 1, 0) 
  # 
  # ltr_slim$count_l3_g2_f <- ifelse((ltr_slim$count >=2 & ltr_slim$count <=3 &  ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_l4_g2_f <- ifelse((ltr_slim$count >=2 & ltr_slim$count <=4 &  ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_l5_g2_f <- ifelse((ltr_slim$count >=2 & ltr_slim$count <=5 &  ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_g2_f <- ifelse((ltr_slim$count >=2 &  ltr_slim$Female==1), 1, 0) 
  # 
  # ltr_slim$count_l4_g3_f <- ifelse((ltr_slim$count >=3 & ltr_slim$count <=4 &  ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_l5_g3_f <- ifelse((ltr_slim$count >=3 & ltr_slim$count <=5 &  ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_g3_f <- ifelse((ltr_slim$count >=3 &  ltr_slim$Female==1), 1, 0) 
  # 
  # ltr_slim$count_l5_g4_f <- ifelse((ltr_slim$count >=4 & ltr_slim$count <=5 &  ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_g4_f <- ifelse((ltr_slim$count >=4 &  ltr_slim$Female==1), 1, 0)
  # 
  # ltr_slim$count_g5_f <- ifelse((ltr_slim$count >=5 &  ltr_slim$Female==1), 1, 0)
  # 
  # ltr_slim$count_l10_g5_f <- ifelse((ltr_slim$count >5 & ltr_slim$count <=10 &  ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_l57_g10_f <- ifelse((ltr_slim$count >10 & ltr_slim$count <=57 & ltr_slim$Female==1), 1, 0)
}
#Binary features for number of complains in the network
{
ltr_slim$co_count_l1 <- ifelse(ltr_slim$co_count<=1, 1, 0)
ltr_slim$co_count_l2 <- ifelse(ltr_slim$co_count<=2, 1, 0)
ltr_slim$co_count_l3 <- ifelse(ltr_slim$co_count<=3, 1, 0)
ltr_slim$co_count_l4 <- ifelse(ltr_slim$co_count<=4, 1, 0)
ltr_slim$co_count_l5 <- ifelse(ltr_slim$co_count<=5, 1, 0)

ltr_slim$co_count_l2_g1 <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=2), 1, 0)
ltr_slim$co_count_l3_g1 <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=3), 1, 0)
ltr_slim$co_count_l4_g1 <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=4), 1, 0)
ltr_slim$co_count_l5_g1 <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=5), 1, 0)
ltr_slim$co_count_g1 <- ifelse((ltr_slim$co_count >=1), 1, 0)

ltr_slim$co_count_l3_g2 <- ifelse((ltr_slim$co_count >=2 & ltr_slim$co_count <=3), 1, 0)
ltr_slim$co_count_l4_g2 <- ifelse((ltr_slim$co_count >=2 & ltr_slim$co_count <=4), 1, 0)
ltr_slim$co_count_l5_g2 <- ifelse((ltr_slim$co_count >=2 & ltr_slim$co_count <=5), 1, 0)
ltr_slim$co_count_g2 <- ifelse((ltr_slim$co_count >=2), 1, 0)

ltr_slim$co_count_l4_g3 <- ifelse((ltr_slim$co_count >=3 & ltr_slim$co_count <=4), 1, 0)
ltr_slim$co_count_l5_g3 <- ifelse((ltr_slim$co_count >=3 & ltr_slim$co_count <=5), 1, 0)
ltr_slim$co_count_g3 <- ifelse((ltr_slim$co_count >=3), 1, 0)

ltr_slim$co_count_l5_g4 <- ifelse((ltr_slim$co_count >=4 & ltr_slim$co_count <=5), 1, 0)
ltr_slim$co_count_g4 <- ifelse((ltr_slim$co_count >=4), 1, 0)

ltr_slim$co_count_g5 <- ifelse((ltr_slim$co_count >=5), 1, 0)

ltr_slim$co_count_l10_g5 <- ifelse((ltr_slim$co_count >5 & ltr_slim$co_count <=10), 1, 0)
ltr_slim$co_count_g10 <- ifelse((ltr_slim$co_count >10), 1, 0)

  # #Male interaction
  # ltr_slim$co_count_l1_m <- ifelse(ltr_slim$co_count<=1 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$co_count_l2_m <- ifelse(ltr_slim$co_count<=2 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$co_count_l3_m <- ifelse(ltr_slim$co_count<=3 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$co_count_l4_m <- ifelse(ltr_slim$co_count<=4 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$co_count_l5_m <- ifelse(ltr_slim$co_count<=5 & ltr_slim$Female==0, 1, 0) 
  # 
  # ltr_slim$co_count_l2_g1_m <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=2 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$co_count_l3_g1_m <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=3 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$co_count_l4_g1_m <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=4 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$co_count_l5_g1_m <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$co_count_g1_m <- ifelse((ltr_slim$co_count >=1 & ltr_slim$Female==0), 1, 0) 
  # 
  # ltr_slim$co_count_l3_g2_m <- ifelse((ltr_slim$co_count >=2 & ltr_slim$co_count <=3 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$co_count_l4_g2_m <- ifelse((ltr_slim$co_count >=2 & ltr_slim$co_count <=4 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$co_count_l5_g2_m <- ifelse((ltr_slim$co_count >=2 & ltr_slim$co_count <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$co_count_g2_m <- ifelse((ltr_slim$co_count >=2 & ltr_slim$Female==0), 1, 0) 
  # 
  # ltr_slim$co_count_l4_g3_m <- ifelse((ltr_slim$co_count >=3 & ltr_slim$co_count <=4 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$co_count_l5_g3_m <- ifelse((ltr_slim$co_count >=3 & ltr_slim$co_count <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$co_count_g3_m <- ifelse((ltr_slim$co_count >=3 & ltr_slim$Female==0), 1, 0) 
  # 
  # ltr_slim$co_count_l5_g4_m <- ifelse((ltr_slim$co_count >=4 & ltr_slim$co_count <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$co_count_g4_m <- ifelse((ltr_slim$co_count >=4 & ltr_slim$Female==0), 1, 0)
  # 
  # ltr_slim$co_count_g5_m <- ifelse((ltr_slim$co_count >=5 & ltr_slim$Female==0), 1, 0)
  # 
  # ltr_slim$co_count_l10_g5_m <- ifelse((ltr_slim$co_count >5 & ltr_slim$co_count <=10 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$co_count_g10_m <- ifelse((ltr_slim$co_count >10 & ltr_slim$Female==0), 1, 0) 
  # 
  # #Female interaction
  # ltr_slim$co_count_l1_f <- ifelse(ltr_slim$co_count<=1 & ltr_slim$Female==1, 1, 0) 
  # ltr_slim$co_count_l2_f <- ifelse(ltr_slim$co_count<=2 & ltr_slim$Female==1, 1, 0) 
  # ltr_slim$co_count_l3_f <- ifelse(ltr_slim$co_count<=3 & ltr_slim$Female==1, 1, 0) 
  # ltr_slim$co_count_l4_f <- ifelse(ltr_slim$co_count<=4 & ltr_slim$Female==1, 1, 0) 
  # ltr_slim$co_count_l5_f <- ifelse(ltr_slim$co_count<=5 & ltr_slim$Female==1, 1, 0) 
  # 
  # ltr_slim$co_count_l2_g1_f <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=2 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$co_count_l3_g1_f <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=3 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$co_count_l4_g1_f <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=4 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$co_count_l5_g1_f <- ifelse((ltr_slim$co_count >=1 & ltr_slim$co_count <=5 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$co_count_g1_f <- ifelse((ltr_slim$co_count >=1 & ltr_slim$Female==1), 1, 0) 
  # 
  # ltr_slim$co_count_l3_g2_f <- ifelse((ltr_slim$co_count >=2 & ltr_slim$co_count <=3 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$co_count_l4_g2_f <- ifelse((ltr_slim$co_count >=2 & ltr_slim$co_count <=4 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$co_count_l5_g2_f <- ifelse((ltr_slim$co_count >=2 & ltr_slim$co_count <=5 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$co_count_g2_f <- ifelse((ltr_slim$co_count >=2 & ltr_slim$Female==1), 1, 0) 
  # 
  # ltr_slim$co_count_l4_g3_f <- ifelse((ltr_slim$co_count >=3 & ltr_slim$co_count <=4 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$co_count_l5_g3_f <- ifelse((ltr_slim$co_count >=3 & ltr_slim$co_count <=5 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$co_count_g3_f <- ifelse((ltr_slim$co_count >=3 & ltr_slim$Female==1), 1, 0) 
  # 
  # ltr_slim$co_count_l5_g4_f <- ifelse((ltr_slim$co_count >=4 & ltr_slim$co_count <=5 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$co_count_g4_f <- ifelse((ltr_slim$co_count >=4 & ltr_slim$Female==1), 1, 0)
  # 
  # ltr_slim$co_count_g5_f <- ifelse((ltr_slim$co_count >=5 & ltr_slim$Female==1), 1, 0)
  # 
  # ltr_slim$co_count_l10_g5_f <- ifelse((ltr_slim$co_count >5 & ltr_slim$co_count <=10 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$co_count_g10_f <- ifelse((ltr_slim$co_count >10 & ltr_slim$Female==1), 1, 0) 
}
#Binary features for number of officers in the network
{
ltr_slim$co_officers_l1 <- ifelse(ltr_slim$co_officers<=1, 1, 0)
ltr_slim$co_officers_l2 <- ifelse(ltr_slim$co_officers<=2, 1, 0)
ltr_slim$co_officers_l3 <- ifelse(ltr_slim$co_officers<=3, 1, 0)
ltr_slim$co_officers_l4 <- ifelse(ltr_slim$co_officers<=4, 1, 0)
ltr_slim$co_officers_l5 <- ifelse(ltr_slim$co_officers<=5, 1, 0)

ltr_slim$co_officers_l2_g1 <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=2), 1, 0)
ltr_slim$co_officers_l3_g1 <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=3), 1, 0)
ltr_slim$co_officers_l4_g1 <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=4), 1, 0)
ltr_slim$co_officers_l5_g1 <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=5), 1, 0)
ltr_slim$co_officers_g1 <- ifelse((ltr_slim$co_officers >=1), 1, 0)

ltr_slim$co_officers_l3_g2 <- ifelse((ltr_slim$co_officers >=2 & ltr_slim$co_officers <=3), 1, 0)
ltr_slim$co_officers_l4_g2 <- ifelse((ltr_slim$co_officers >=2 & ltr_slim$co_officers <=4), 1, 0)
ltr_slim$co_officers_l5_g2 <- ifelse((ltr_slim$co_officers >=2 & ltr_slim$co_officers <=5), 1, 0)
ltr_slim$co_officers_g2 <- ifelse((ltr_slim$co_officers >=2), 1, 0)

ltr_slim$co_officers_l4_g3 <- ifelse((ltr_slim$co_officers >=3 & ltr_slim$co_officers <=4), 1, 0)
ltr_slim$co_officers_l5_g3 <- ifelse((ltr_slim$co_officers >=3 & ltr_slim$co_officers <=5), 1, 0)
ltr_slim$co_officers_g3 <- ifelse((ltr_slim$co_officers >=3), 1, 0)

ltr_slim$co_officers_l5_g4 <- ifelse((ltr_slim$co_officers >=4 & ltr_slim$co_officers <=5), 1, 0)
ltr_slim$co_officers_g4 <- ifelse((ltr_slim$co_officers >=4), 1, 0)

# #Male interaction
# ltr_slim$co_officers_l1_m <- ifelse(ltr_slim$co_officers<=1 & ltr_slim$Female==0, 1, 0) 
# ltr_slim$co_officers_l2_m <- ifelse(ltr_slim$co_officers<=2 & ltr_slim$Female==0, 1, 0) 
# ltr_slim$co_officers_l3_m <- ifelse(ltr_slim$co_officers<=3 & ltr_slim$Female==0, 1, 0) 
# ltr_slim$co_officers_l4_m <- ifelse(ltr_slim$co_officers<=4 & ltr_slim$Female==0, 1, 0) 
# ltr_slim$co_officers_l5_m <- ifelse(ltr_slim$co_officers<=5 & ltr_slim$Female==0, 1, 0) 
# 
# ltr_slim$co_officers_l2_g1_m <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=2 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_officers_l3_g1_m <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=3 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_officers_l4_g1_m <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=4 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_officers_l5_g1_m <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=5 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_officers_g1_m <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$Female==0), 1, 0) 
# 
# ltr_slim$co_officers_l3_g2_m <- ifelse((ltr_slim$co_officers >=2 & ltr_slim$co_officers <=3 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_officers_l4_g2_m <- ifelse((ltr_slim$co_officers >=2 & ltr_slim$co_officers <=4 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_officers_l5_g2_m <- ifelse((ltr_slim$co_officers >=2 & ltr_slim$co_officers <=5 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_officers_g2_m <- ifelse((ltr_slim$co_officers >=2 & ltr_slim$Female==0), 1, 0) 
# 
# ltr_slim$co_officers_l4_g3_m <- ifelse((ltr_slim$co_officers >=3 & ltr_slim$co_officers <=4 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_officers_l5_g3_m <- ifelse((ltr_slim$co_officers >=3 & ltr_slim$co_officers <=5 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_officers_g3_m <- ifelse((ltr_slim$co_officers >=3 & ltr_slim$Female==0), 1, 0) 
# 
# ltr_slim$co_officers_l5_g4_m <- ifelse((ltr_slim$co_officers >=4 & ltr_slim$co_officers <=5 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_officers_g4_m <- ifelse((ltr_slim$co_officers >=4 & ltr_slim$Female==0), 1, 0)
# 
# #Female interaction
# ltr_slim$co_officers_l1_f <- ifelse(ltr_slim$co_officers<=1 & ltr_slim$Female==1, 1, 0) 
# ltr_slim$co_officers_l2_f <- ifelse(ltr_slim$co_officers<=2 & ltr_slim$Female==1, 1, 0) 
# ltr_slim$co_officers_l3_f <- ifelse(ltr_slim$co_officers<=3 & ltr_slim$Female==1, 1, 0) 
# ltr_slim$co_officers_l4_f <- ifelse(ltr_slim$co_officers<=4 & ltr_slim$Female==1, 1, 0) 
# ltr_slim$co_officers_l5_f <- ifelse(ltr_slim$co_officers<=5 & ltr_slim$Female==1, 1, 0) 
# 
# ltr_slim$co_officers_l2_g1_f <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=2 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_officers_l3_g1_f <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=3 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_officers_l4_g1_f <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=4 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_officers_l5_g1_f <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$co_officers <=5 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_officers_g1_f <- ifelse((ltr_slim$co_officers >=1 & ltr_slim$Female==1), 1, 0) 
# 
# ltr_slim$co_officers_l3_g2_f <- ifelse((ltr_slim$co_officers >=2 & ltr_slim$co_officers <=3 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_officers_l4_g2_f <- ifelse((ltr_slim$co_officers >=2 & ltr_slim$co_officers <=4 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_officers_l5_g2_f <- ifelse((ltr_slim$co_officers >=2 & ltr_slim$co_officers <=5 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_officers_g2_f <- ifelse((ltr_slim$co_officers >=2 & ltr_slim$Female==1), 1, 0) 
# 
# ltr_slim$co_officers_l4_g3_f <- ifelse((ltr_slim$co_officers >=3 & ltr_slim$co_officers <=4 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_officers_l5_g3_f <- ifelse((ltr_slim$co_officers >=3 & ltr_slim$co_officers <=5 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_officers_g3_f <- ifelse((ltr_slim$co_officers >=3 & ltr_slim$Female==1), 1, 0) 
# 
# ltr_slim$co_officers_l5_g4_f <- ifelse((ltr_slim$co_officers >=4 & ltr_slim$co_officers <=5 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_officers_g4_f <- ifelse((ltr_slim$co_officers >=4 & ltr_slim$Female==1), 1, 0)

}
#Binary features for number of complains in a year
{
ltr_slim$count_year_l1 <- ifelse(ltr_slim$count_year<=1, 1, 0)
ltr_slim$count_year_l2 <- ifelse(ltr_slim$count_year<=2, 1, 0)
ltr_slim$count_year_l3 <- ifelse(ltr_slim$count_year<=3, 1, 0)
ltr_slim$count_year_l4 <- ifelse(ltr_slim$count_year<=4, 1, 0)
ltr_slim$count_year_l5 <- ifelse(ltr_slim$count_year<=5, 1, 0)

ltr_slim$count_year_l2_g1 <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=2), 1, 0)
ltr_slim$count_year_l3_g1 <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=3), 1, 0)
ltr_slim$count_year_l4_g1 <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=4), 1, 0)
ltr_slim$count_year_l5_g1 <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=5), 1, 0)
ltr_slim$count_year_g1 <- ifelse((ltr_slim$count_year >=1), 1, 0)

ltr_slim$count_year_l3_g2 <- ifelse((ltr_slim$count_year >=2 & ltr_slim$count_year <=3), 1, 0)
ltr_slim$count_year_l4_g2 <- ifelse((ltr_slim$count_year >=2 & ltr_slim$count_year <=4), 1, 0)
ltr_slim$count_year_l5_g2 <- ifelse((ltr_slim$count_year >=2 & ltr_slim$count_year <=5), 1, 0)
ltr_slim$count_year_g2 <- ifelse((ltr_slim$count_year >=2), 1, 0)

ltr_slim$count_year_l4_g3 <- ifelse((ltr_slim$count_year >=3 & ltr_slim$count_year <=4), 1, 0)
ltr_slim$count_year_l5_g3 <- ifelse((ltr_slim$count_year >=3 & ltr_slim$count_year <=5), 1, 0)
ltr_slim$count_year_g3 <- ifelse((ltr_slim$count_year >=3), 1, 0)

ltr_slim$count_year_l5_g4 <- ifelse((ltr_slim$count_year >=4 & ltr_slim$count_year <=5), 1, 0)
ltr_slim$count_year_g4 <- ifelse((ltr_slim$count_year >=4), 1, 0)

ltr_slim$count_year_g5 <- ifelse((ltr_slim$count_year >=5), 1, 0)

ltr_slim$count_year_l10_g5 <- ifelse((ltr_slim$count_year >5 & ltr_slim$count_year <=10), 1, 0)
ltr_slim$count_year_g10 <- ifelse((ltr_slim$count_year >10), 1, 0)

  # #Male interaction
  # ltr_slim$count_year_l1_m <- ifelse(ltr_slim$count_year<=1 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$count_year_l2_m <- ifelse(ltr_slim$count_year<=2 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$count_year_l3_m <- ifelse(ltr_slim$count_year<=3 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$count_year_l4_m <- ifelse(ltr_slim$count_year<=4 & ltr_slim$Female==0, 1, 0) 
  # ltr_slim$count_year_l5_m <- ifelse(ltr_slim$count_year<=5 & ltr_slim$Female==0, 1, 0) 
  # 
  # ltr_slim$count_year_l2_g1_m <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=2 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_year_l3_g1_m <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=3 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_year_l4_g1_m <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=4 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_year_l5_g1_m <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_year_g1_m <- ifelse((ltr_slim$count_year >=1 & ltr_slim$Female==0), 1, 0) 
  # 
  # ltr_slim$count_year_l3_g2_m <- ifelse((ltr_slim$count_year >=2 & ltr_slim$count_year <=3 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_year_l4_g2_m <- ifelse((ltr_slim$count_year >=2 & ltr_slim$count_year <=4 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_year_l5_g2_m <- ifelse((ltr_slim$count_year >=2 & ltr_slim$count_year <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_year_g2_m <- ifelse((ltr_slim$count_year >=2 & ltr_slim$Female==0), 1, 0) 
  # 
  # ltr_slim$count_year_l4_g3_m <- ifelse((ltr_slim$count_year >=3 & ltr_slim$count_year <=4 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_year_l5_g3_m <- ifelse((ltr_slim$count_year >=3 & ltr_slim$count_year <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_year_g3_m <- ifelse((ltr_slim$count_year >=3 & ltr_slim$Female==0), 1, 0) 
  # 
  # ltr_slim$count_year_l5_g4_m <- ifelse((ltr_slim$count_year >=4 & ltr_slim$count_year <=5 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_year_g4_m <- ifelse((ltr_slim$count_year >=4 & ltr_slim$Female==0), 1, 0)
  # 
  # ltr_slim$count_year_g5_m <- ifelse((ltr_slim$count_year >=5 & ltr_slim$Female==0), 1, 0)
  # 
  # ltr_slim$count_year_l10_g5_m <- ifelse((ltr_slim$count_year >5 & ltr_slim$count_year <=10 & ltr_slim$Female==0), 1, 0)  
  # ltr_slim$count_year_g10_m <- ifelse((ltr_slim$count_year >10 & ltr_slim$Female==0), 1, 0) 
  # 
  # #Female interaction
  # ltr_slim$count_year_l1_f <- ifelse(ltr_slim$count_year<=1 & ltr_slim$Female==1, 1, 0) 
  # ltr_slim$count_year_l2_f <- ifelse(ltr_slim$count_year<=2 & ltr_slim$Female==1, 1, 0) 
  # ltr_slim$count_year_l3_f <- ifelse(ltr_slim$count_year<=3 & ltr_slim$Female==1, 1, 0) 
  # ltr_slim$count_year_l4_f <- ifelse(ltr_slim$count_year<=4 & ltr_slim$Female==1, 1, 0) 
  # ltr_slim$count_year_l5_f <- ifelse(ltr_slim$count_year<=5 & ltr_slim$Female==1, 1, 0) 
  # 
  # ltr_slim$count_year_l2_g1_f <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=2 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_year_l3_g1_f <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=3 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_year_l4_g1_f <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=4 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_year_l5_g1_f <- ifelse((ltr_slim$count_year >=1 & ltr_slim$count_year <=5 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_year_g1_f <- ifelse((ltr_slim$count_year >=1 & ltr_slim$Female==1), 1, 0) 
  # 
  # ltr_slim$count_year_l3_g2_f <- ifelse((ltr_slim$count_year >=2 & ltr_slim$count_year <=3 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_year_l4_g2_f <- ifelse((ltr_slim$count_year >=2 & ltr_slim$count_year <=4 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_year_l5_g2_f <- ifelse((ltr_slim$count_year >=2 & ltr_slim$count_year <=5 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_year_g2_f <- ifelse((ltr_slim$count_year >=2 & ltr_slim$Female==1), 1, 0) 
  # 
  # ltr_slim$count_year_l4_g3_f <- ifelse((ltr_slim$count_year >=3 & ltr_slim$count_year <=4 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_year_l5_g3_f <- ifelse((ltr_slim$count_year >=3 & ltr_slim$count_year <=5 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_year_g3_f <- ifelse((ltr_slim$count_year >=3 & ltr_slim$Female==1), 1, 0) 
  # 
  # ltr_slim$count_year_l5_g4_f <- ifelse((ltr_slim$count_year >=4 & ltr_slim$count_year <=5 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_year_g4_f <- ifelse((ltr_slim$count_year >=4 & ltr_slim$Female==1), 1, 0)
  # 
  # ltr_slim$count_year_g5_f <- ifelse((ltr_slim$count_year >=5 & ltr_slim$Female==1), 1, 0)
  # 
  # ltr_slim$count_year_l10_g5_f <- ifelse((ltr_slim$count_year >5 & ltr_slim$count_year <=10 & ltr_slim$Female==1), 1, 0)  
  # ltr_slim$count_year_g10_f <- ifelse((ltr_slim$count_year >10 & ltr_slim$Female==1), 1, 0) 
}
#Binary features for number of complains in a year in the network
{
ltr_slim$co_count_year_l1 <- ifelse(ltr_slim$co_count_year<=1, 1, 0)
ltr_slim$co_count_year_l2 <- ifelse(ltr_slim$co_count_year<=2, 1, 0)
ltr_slim$co_count_year_l3 <- ifelse(ltr_slim$co_count_year<=3, 1, 0)
ltr_slim$co_count_year_l4 <- ifelse(ltr_slim$co_count_year<=4, 1, 0)
ltr_slim$co_count_year_l5 <- ifelse(ltr_slim$co_count_year<=5, 1, 0)

ltr_slim$co_count_year_l2_g1 <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=2), 1, 0)
ltr_slim$co_count_year_l3_g1 <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=3), 1, 0)
ltr_slim$co_count_year_l4_g1 <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=4), 1, 0)
ltr_slim$co_count_year_l5_g1 <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=5), 1, 0)
ltr_slim$co_count_year_g1 <- ifelse((ltr_slim$co_count_year >=1), 1, 0)

ltr_slim$co_count_year_l3_g2 <- ifelse((ltr_slim$co_count_year >=2 & ltr_slim$co_count_year <=3), 1, 0)
ltr_slim$co_count_year_l4_g2 <- ifelse((ltr_slim$co_count_year >=2 & ltr_slim$co_count_year <=4), 1, 0)
ltr_slim$co_count_year_l5_g2 <- ifelse((ltr_slim$co_count_year >=2 & ltr_slim$co_count_year <=5), 1, 0)
ltr_slim$co_count_year_g2 <- ifelse((ltr_slim$co_count_year >=2), 1, 0)

ltr_slim$co_count_year_l4_g3 <- ifelse((ltr_slim$co_count_year >=3 & ltr_slim$co_count_year <=4), 1, 0)
ltr_slim$co_count_year_l5_g3 <- ifelse((ltr_slim$co_count_year >=3 & ltr_slim$co_count_year <=5), 1, 0)
ltr_slim$co_count_year_g3 <- ifelse((ltr_slim$co_count_year >=3), 1, 0)

ltr_slim$co_count_year_l5_g4 <- ifelse((ltr_slim$co_count_year >=4 & ltr_slim$co_count_year <=5), 1, 0)
ltr_slim$co_count_year_g4 <- ifelse((ltr_slim$co_count_year >=4), 1, 0)

ltr_slim$co_count_year_g5 <- ifelse((ltr_slim$co_count_year >=5), 1, 0)

ltr_slim$co_count_year_l10_g5 <- ifelse((ltr_slim$co_count_year >5 & ltr_slim$co_count_year <=10), 1, 0)
ltr_slim$co_count_year_g10 <- ifelse((ltr_slim$co_count_year >10), 1, 0)

ltr_slim$co_count_year_l20_g10 <- ifelse((ltr_slim$co_count_year >10 & ltr_slim$co_count_year <=20), 1, 0)
ltr_slim$co_count_year_l70_g20 <- ifelse((ltr_slim$co_count_year >20 & ltr_slim$co_count_year <=70), 1, 0)

# #Male interaction
# ltr_slim$co_count_year_l1_m <- ifelse(ltr_slim$co_count_year<=1 & ltr_slim$Female==0, 1, 0) 
# ltr_slim$co_count_year_l2_m <- ifelse(ltr_slim$co_count_year<=2 & ltr_slim$Female==0, 1, 0) 
# ltr_slim$co_count_year_l3_m <- ifelse(ltr_slim$co_count_year<=3 & ltr_slim$Female==0, 1, 0) 
# ltr_slim$co_count_year_l4_m <- ifelse(ltr_slim$co_count_year<=4 & ltr_slim$Female==0, 1, 0) 
# ltr_slim$co_count_year_l5_m <- ifelse(ltr_slim$co_count_year<=5 & ltr_slim$Female==0, 1, 0) 
# 
# ltr_slim$co_count_year_l2_g1_m <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=2 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_l3_g1_m <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=3 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_l4_g1_m <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=4 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_l5_g1_m <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=5 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_g1_m <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$Female==0), 1, 0) 
# 
# ltr_slim$co_count_year_l3_g2_m <- ifelse((ltr_slim$co_count_year >=2 & ltr_slim$co_count_year <=3 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_l4_g2_m <- ifelse((ltr_slim$co_count_year >=2 & ltr_slim$co_count_year <=4 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_l5_g2_m <- ifelse((ltr_slim$co_count_year >=2 & ltr_slim$co_count_year <=5 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_g2_m <- ifelse((ltr_slim$co_count_year >=2 & ltr_slim$Female==0), 1, 0) 
# 
# ltr_slim$co_count_year_l4_g3_m <- ifelse((ltr_slim$co_count_year >=3 & ltr_slim$co_count_year <=4 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_l5_g3_m <- ifelse((ltr_slim$co_count_year >=3 & ltr_slim$co_count_year <=5 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_g3_m <- ifelse((ltr_slim$co_count_year >=3 & ltr_slim$Female==0), 1, 0) 
# 
# ltr_slim$co_count_year_l5_g4_m <- ifelse((ltr_slim$co_count_year >=4 & ltr_slim$co_count_year <=5 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_g4_m <- ifelse((ltr_slim$co_count_year >=4 & ltr_slim$Female==0), 1, 0)
# 
# ltr_slim$co_count_year_g5_m <- ifelse((ltr_slim$co_count_year >=5 & ltr_slim$Female==0), 1, 0)
# 
# ltr_slim$co_count_year_l10_g5_m <- ifelse((ltr_slim$co_count_year >5 & ltr_slim$co_count_year <=10 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_g10_m <- ifelse((ltr_slim$co_count_year >10 & ltr_slim$Female==0), 1, 0)  
# ltr_slim$co_count_year_l20_g10_m <- ifelse((ltr_slim$co_count_year >10 & ltr_slim$co_count_year <=20 & ltr_slim$Female==0), 1, 0) 
# ltr_slim$co_count_year_l70_g20_m <- ifelse((ltr_slim$co_count_year >20 & ltr_slim$co_count_year <=70 & ltr_slim$Female==0), 1, 0)
# 
# #Female interaction
# ltr_slim$co_count_year_l1_f <- ifelse(ltr_slim$co_count_year<=1 & ltr_slim$Female==1, 1, 0) 
# ltr_slim$co_count_year_l2_f <- ifelse(ltr_slim$co_count_year<=2 & ltr_slim$Female==1, 1, 0) 
# ltr_slim$co_count_year_l3_f <- ifelse(ltr_slim$co_count_year<=3 & ltr_slim$Female==1, 1, 0) 
# ltr_slim$co_count_year_l4_f <- ifelse(ltr_slim$co_count_year<=4 & ltr_slim$Female==1, 1, 0) 
# ltr_slim$co_count_year_l5_f <- ifelse(ltr_slim$co_count_year<=5 & ltr_slim$Female==1, 1, 0) 
# 
# ltr_slim$co_count_year_l2_g1_f <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=2 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_count_year_l3_g1_f <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=3 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_count_year_l4_g1_f <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=4 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_count_year_l5_g1_f <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$co_count_year <=5 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_count_year_g1_f <- ifelse((ltr_slim$co_count_year >=1 & ltr_slim$Female==1), 1, 0) 
# 
# ltr_slim$co_count_year_l3_g2_f <- ifelse((ltr_slim$co_count_year >=2 & ltr_slim$co_count_year <=3 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_count_year_l4_g2_f <- ifelse((ltr_slim$co_count_year >=2 & ltr_slim$co_count_year <=4 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_count_year_l5_g2_f <- ifelse((ltr_slim$co_count_year >=2 & ltr_slim$co_count_year <=5 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_count_year_g2_f <- ifelse((ltr_slim$co_count_year >=2 & ltr_slim$Female==1), 1, 0) 
# 
# ltr_slim$co_count_year_l4_g3_f <- ifelse((ltr_slim$co_count_year >=3 & ltr_slim$co_count_year <=4 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_count_year_l5_g3_f <- ifelse((ltr_slim$co_count_year >=3 & ltr_slim$co_count_year <=5 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_count_year_g3_f <- ifelse((ltr_slim$co_count_year >=3 & ltr_slim$Female==1), 1, 0) 
# 
# ltr_slim$co_count_year_l5_g4_f <- ifelse((ltr_slim$co_count_year >=4 & ltr_slim$co_count_year <=5 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_count_year_g4_f <- ifelse((ltr_slim$co_count_year >=4 & ltr_slim$Female==1), 1, 0)
# 
# ltr_slim$co_count_year_g5_f <- ifelse((ltr_slim$co_count_year >=5 & ltr_slim$Female==1), 1, 0)
# 
# ltr_slim$co_count_year_l10_g5_f <- ifelse((ltr_slim$co_count_year >5 & ltr_slim$co_count_year <=10 & ltr_slim$Female==1), 1, 0)  
# ltr_slim$co_count_year_g10_f <- ifelse((ltr_slim$co_count_year >10 & ltr_slim$Female==1), 1, 0) 
# ltr_slim$co_count_year_l20_g10_f <- ifelse((ltr_slim$co_count_year >10 & ltr_slim$co_count_year <=20 & ltr_slim$Female==1), 1, 0) 
# ltr_slim$co_count_year_l70_g20_f <- ifelse((ltr_slim$co_count_year >20 & ltr_slim$co_count_year <=70 & ltr_slim$Female==1), 1, 0)
}

columns = names(ltr_slim)

risk_slim_vars = columns[!columns %in% c("X","OfficerID","count","pp10","pp100","pp1000","count_year","Gender","Race","age","unit",
                                         "label","start","end","month_id","co_count","co_count_year","co_pp10","co_pp100","co_pp1000",    
                                         "co_officers")]#, "Hispanic", "White", "Black","Female")]


options(scipen=999)
ltr_slim = ltr_slim[,risk_slim_vars]
write.csv(ltr_slim ,file="ltr_binary_vars_all_no_interaction_data.csv", row.names = FALSE)
#write.csv(co_ltr_slim ,file="co_ltr_slim_data.csv", row.names = FALSE)
options(scipen=0)

###################score percentages###############

# +----------------------------------------------+-------------------+-----------+
# | Pr(Y = +1) = 1.0/(1.0 + exp(-(-4 + score))   |                   |           |
# | ============================================ | ================= | ========= |
# | count_year_l3_g1                             |          1 points |   + ..... |
# | Age_l50_g40                                  |         -1 points |   + ..... |
# | count_l2                                     |         -1 points |   + ..... |
# | Age_g50                                      |         -3 points |   + ..... |
# | ============================================ | ================= | ========= |
# | ADD POINTS FROM ROWS 1 to 4                  |             SCORE |   = ..... |
# +----------------------------------------------+-------------------+-----------+
                        

ltr_slim$score = 1* ltr_slim$count_year_l3_g1 -1* ltr_slim$Age_l50_g40 -1* ltr_slim$count_l2 -3 * ltr_slim$Age_g50
ltr_slim$risk = 1.0/(1.0 + exp(-(-4 + ltr_slim$score)))
risk_columns = c("binary", "count_year_l3_g1", "Age_l50_g40", "count_l2", "Age_g50", "score", "risk" )

options(scipen=999) 
write.csv(ltr_slim[,risk_columns] ,file="results/risk-slim/ltr_with_score_risk.csv", row.names = FALSE) 
options(scipen=0)

ltr_with_score_risk = read.csv("results/risk-slim/ltr_with_score_risk.csv", stringsAsFactors = FALSE)

ranges= data.frame(min=sapply(ltr_with_score_risk,min),max=sapply(ltr_with_score_risk,max))


score_5 = ltr_with_score_risk[ltr_with_score_risk$score==-5,]
score_4 = ltr_with_score_risk[ltr_with_score_risk$score==-4,]
score_3 = ltr_with_score_risk[ltr_with_score_risk$score==-3,]
score_2 = ltr_with_score_risk[ltr_with_score_risk$score==-2,]
score_1 = ltr_with_score_risk[ltr_with_score_risk$score==-1,]
score0 = ltr_with_score_risk[ltr_with_score_risk$score==0,]
score1 = ltr_with_score_risk[ltr_with_score_risk$score==1,]

score_5_percentage = (nrow(score_5[score_5$binary==1,])/nrow(score_5))*100
score_4_percentage = (nrow(score_4[score_4$binary==1,])/nrow(score_4))*100
score_3_percentage = (nrow(score_3[score_3$binary==1,])/nrow(score_3))*100
score_2_percentage = (nrow(score_2[score_2$binary==1,])/nrow(score_2))*100
score_1_percentage = (nrow(score_1[score_1$binary==1,])/nrow(score_1))*100
score0_percentage = (nrow(score0[score0$binary==1,])/nrow(score0))*100
score1_percentage = (nrow(score1[score1$binary==1,])/nrow(score1))*100


sprintf("%f %f  %f  %f  %f %f %f", score_5_percentage, score_4_percentage, score_3_percentage, score_2_percentage, score_1_percentage, score0_percentage, score1_percentage)


# +----------------------------------------------+-------------------+-----------+
# | Pr(Y = +1) = 1.0/(1.0 + exp(-(-5 + score))   |                   |           |
# | ============================================ | ================= | ========= |
# | count_year_g1                                |          1 points |   + ..... |
# | co_officers_l3_g2                            |         -1 points |   + ..... |
# | Age_g50                                      |         -2 points |   + ..... |
# | ============================================ | ================= | ========= |
# | ADD POINTS FROM ROWS 1 to 3                  |             SCORE |   = ..... |
# +----------------------------------------------+-------------------+-----------+
#                         
ltr_slim$score = 1* ltr_slim$count_year_g1 -1* ltr_slim$co_officers_l3_g2 -2 * ltr_slim$Age_g50
ltr_slim$risk = 1.0/(1.0 + exp(-(-5 + ltr_slim$score)))
risk_columns = c("binary", "count_year_g1", "co_officers_l3_g2", "Age_g50", "score", "risk" )

options(scipen=999) 
write.csv(ltr_slim[,risk_columns] ,file="results/risk-slim/ltr_with_score_risk2.csv", row.names = FALSE) 
options(scipen=0)

ltr_with_score_risk = read.csv("results/risk-slim/ltr_with_score_risk2.csv", stringsAsFactors = FALSE)

ranges= data.frame(min=sapply(ltr_with_score_risk,min),max=sapply(ltr_with_score_risk,max))

score_3 = ltr_with_score_risk[ltr_with_score_risk$score==-3,]
score_2 = ltr_with_score_risk[ltr_with_score_risk$score==-2,]
score_1 = ltr_with_score_risk[ltr_with_score_risk$score==-1,]
score0 = ltr_with_score_risk[ltr_with_score_risk$score==0,]
score1 = ltr_with_score_risk[ltr_with_score_risk$score==1,]

score_3_percentage = (nrow(score_3[score_3$binary==1,])/nrow(score_3))*100
score_2_percentage = (nrow(score_2[score_2$binary==1,])/nrow(score_2))*100
score_1_percentage = (nrow(score_1[score_1$binary==1,])/nrow(score_1))*100
score0_percentage = (nrow(score0[score0$binary==1,])/nrow(score0))*100
score1_percentage = (nrow(score1[score1$binary==1,])/nrow(score1))*100


sprintf(" %f  %f  %f %f %f", score_3_percentage, score_2_percentage, score_1_percentage, score0_percentage, score1_percentage)


# score -2   3 people y=1,0,1
# 66%
# score -1 5 people y=1,0,0,0,1
# 40%
