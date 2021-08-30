library(readr)
hmeq <- read_csv("C:/Baruch College/Baruch Fall 2020/FIN 9855 Risk 2/Project/hmeq.csv")
# Variables 'REASON' and 'JOB' are numerical which are to be converted to factor
hmeq$REASON<-as.factor(hmeq$REASON)
hmeq$JOB<-as.factor(hmeq$JOB)
str(hmeq)
#Checking missing value and deleting them as they are in small quantity
summary(hmeq)
table(is.na(hmeq))
hmeq<-na.omit(hmeq)

# Check for outliers
boxplot(hmeq)
# From the box plot we can see some extreme outliers in value and mortgage which we need to remove
hmeq<-hmeq[hmeq$MORTDUE<4e+05,]
hmeq<-hmeq[hmeq$VALUE<3e+05,]
boxplot(hmeq)

# Splitting the data
library(caret)
split=createDataPartition(hmeq$BAD,p=0.6,list = FALSE)
train<-hmeq[split,]
test<-hmeq[-split,]
dim(train)
dim(test)
library(scorecardModelUtils)
#Model Bulding
library(MASS)
model<-glm(BAD ~ ., data=train,family = binomial(link = "logit"))
logit_stepwise=stepAIC(model,direction="both")
exp(logit_stepwise$coefficients)

#Predict on Train
pred=predict(logit_stepwise,train)
pred_train=ifelse(pred>0.5,1,0)
confusionMatrix(as.factor(train$BAD),as.factor(pred_train))
library(ROCR)
library(Metrics)
pr<-prediction(pred_train,train$BAD)
auc<-performance(pr,measure = "auc")
prf<-performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)
# predict on test
pred_2=predict(logit_stepwise,test)
pred_test=ifelse(pred_2>0.5,1,0)
confusionMatrix(as.factor(test$BAD),as.factor(pred_test))
pr_2<-prediction(pred_test,test$BAD)
auc_test<-performance(pr_2,measure = "auc")
prf_test<-performance(pr_2,measure = "tpr",x.measure = "fpr")
plot(prf_test)
#Score the entire dataset
library(dplyr)
test$predict_logit=predict(logit_stepwise,test,type = 'response')
train$predict_logit=predict(logit_stepwise,train,type = 'response')
train$sample='train'
test$sample='test'
data_whole=rbind(train,test)               
data_score=data_whole%>%
  dplyr::select(BAD,DEROG,DELINQ,CLAGE,NINQ,CLNO,DEBTINC,sample,predict_logit)

# Define a scaling Function
scaled_score <- function(logit, odds, offset =500, pdo = 20)
{
  b = pdo/log(2)
  a = offset - b*log(odds)
  round(a + b*log((1-logit)/logit))
}
# 2.3 Define scoring parameters in line with objectives 
data_score$score<-
  scaled_score(data_score$predict_logit, 72, 660, 40)
hist(x =data_score$score,col = "blue" )

# Model Validation
# Gini Index and ROC curve
library(optiRum)
gini_train=optiRum::giniCoef(train$predict_logit,train$BAD)
print(gini_train)
# ROC curve
library(pROC)
par(pty="s")
plot(roc(train$BAD,train$predict_logit,
         direction="<",smooth = TRUE),
     col="blue", lwd=2, main="ROC Curve")

