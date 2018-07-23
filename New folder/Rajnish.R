rm(list = ls())
setwd("C:/Users/Rajneesh/Desktop/New Folder")

load(".Rdata")
loadhistory()

library(data.table)
library(tidyr)
library(rpart)
library(ROSE)
library(ggplot2)
library(DMwR)
test<-read.csv(file.choose())
train<- read.csv(file.choose())
sample.sub<- read.csv(file.choose())

setDT(train)
setDT(test)

#missing values and info.
str(train)
sapply(train, function(x) sum(is.na(x))/length(x))*100
sapply(test, function(x) sum(is.na(x))/length(x))*100

resp<- train$renewal
table(train$renewal) #anomaly problem!!! :O

#Since id is not used in analysis, remove it sideways

id<- c(train$id, test$id)
test.id<- test$id
train.id<- train$id
test$id<- NULL
train$id<- NULL

all<- rbind(test,train)

formula<- resp~ perc_premium_paid_by_cash_credit+age_in_days+Income+Count_3.6_months_late+Count_6.12_months_late+Count_more_than_12_months_late+application_underwriting_score+no_of_premiums_paid+sourcing_channel+residence_area_type+premium

table(resp)

x.train <- knnImputation(train)
anyNA(x.train)
#oversampling,undersampling, ROSE method,and over-under combined
data_over<- ovun.sample(renewal~., data= x.train,method = "over", N=(74855*2 ))$data
data_under<- ovun.sample(renewal~., data= x.train,method = "under", N=(4998*2 ),seed=1)$data
data_ROSE<-ROSE(renewal~.,data=x.train)$data
data_both<- ovun.sample(renewal~., data= x.train,method = "both",seed=1)$data

#tree formation

tree.rose <- rpart(renewal~ ., data = data_ROSE)
tree.over <- rpart(renewal~ ., data = data_over)
tree.under <- rpart(renewal~ ., data = data_under)
tree.both <- rpart(renewal~ ., data = data_both)

#predicting on test data

pred.tree.rose <- predict(tree.rose, newdata = test)
pred.tree.over <- predict(tree.over, newdata = test)
pred.tree.under <- predict(tree.under, newdata = test)
pred.tree.both <- predict(tree.both, newdata = test)

#predicted probabilities
renew.rose<-pred.tree.rose[,1]
renew.over<-pred.tree.over[,1]
renew.under<-pred.tree.under[,1]
renew.both<-pred.tree.both[,1]

#check it!
unique(renew.rose)
unique(renew.over)
unique(renew.under)
unique(renew.both)

threshold<-0.5
max.percent.improve<-17.2931 #by given functions

i<-0 #loop variable

rose.prob.deficit<-0#percent improvement needed = threshold-actual probability (if< max.percent.improve)
over.prob.deficit<-0
under.prob.deficit<-0
both.prob.deficit<-0

rose.incentive<-0 #incentive to be given
over.incentive<-0
under.incentive<-0
both.incentive<-0

rose.prob<-0 # for revenue, 0 if not possible renew, 1 if w/o incentive, else actual prob.
over.prob<-0
under.prob<-0
both.prob<-0

incentive <- function(x=0){(-400*log(1+0.5*log(1-5*x)))}#combine effort & %increase function and take inverse

for(i in 1:nrow(test)){
  
  #ROSE
  ifelse(renew.rose[i]>threshold,
         (rose.prob.deficit[i]<-0) & (rose.prob[i]<-1),
         (rose.prob.deficit[i]<-(threshold-renew.rose[i])) & (rose.prob[i]<-renew.rose[i]))
  
  ifelse(renew.rose[i]>threshold,
         rose.incentive[i]<-0,
         rose.incentive[i]<-incentive(rose.prob.deficit[i]))
  
  if(rose.prob.deficit[i]>max.percent.improve|is.nan(rose.incentive[i])==TRUE){(rose.incentive[i]<-0) & (rose.prob[i]<-0)}
  
  #over
  ifelse(renew.over[i]>threshold,
         (over.prob.deficit[i]<-0) & (over.prob[i]<-1),
         (over.prob.deficit[i]<-threshold-renew.over[i]) & (over.prob[i]<-renew.over[i]))
  
  ifelse(renew.over[i]>threshold,
         over.incentive[i]<-0,
         over.incentive[i]<-incentive(over.prob.deficit[i]))
  
  if(over.prob.deficit[i]>max.percent.improve|is.nan(over.incentive[i])==TRUE){(over.incentive[i]<-0) & (over.prob[i]<-0)}
  
  #under
  ifelse(renew.under[i]>threshold,
         (under.prob.deficit[i]<-0) & (under.prob[i]<-1),
         (under.prob.deficit[i]<-threshold-renew.under[i]) & (under.prob[i]<-renew.under[i]))
  
  ifelse(renew.under[i]>threshold,
         under.incentive[i]<-0,
         under.incentive[i]<-incentive(under.prob.deficit[i]))
  
  if(under.prob.deficit[i]>max.percent.improve|is.nan(under.incentive[i])==TRUE ){(under.incentive[i]<-0) & (under.prob[i]<-0)}
  
  #both
  ifelse(renew.both[i]>threshold,
         (both.prob.deficit[i]<-0) & (both.prob[i]<-1),
         (both.prob.deficit[i]<-threshold-renew.both[i]) & (both.prob[i]<-renew.both[i]))
  
  ifelse(renew.both[i]>threshold,
         both.incentive[i]<-0,
         both.incentive[i]<-incentive(both.prob.deficit[i]))
  
  if(both.prob.deficit[i]>max.percent.improve|is.nan(both.incentive[i])==TRUE){(both.incentive[i]<-0) & (both.prob[i]<-0)}
  
}
         
table(rose.incentive) 
table(over.incentive)
table(under.incentive)
table(rose.incentive)

#submitting
rose1<- cbind(id = test.id, renewal = renew.rose,incentives = rose.incentive)
over1<- cbind(id = test.id,  renewal = renew.over,incentives = over.incentive)
under1<- cbind(id = test.id, renewal = renew.under,incentives = under.incentive)
both1<- cbind(id = test.id, renewal = renew.both,incentives = both.incentive)

write.csv(rose1, file= "rose.csv",row.names = FALSE)
write.csv(over1, file= "over.csv",row.names = FALSE)         
write.csv(under1, file= "under.csv",row.names = FALSE)
write.csv(both1, file= "both.csv",row.names = FALSE)
  
  