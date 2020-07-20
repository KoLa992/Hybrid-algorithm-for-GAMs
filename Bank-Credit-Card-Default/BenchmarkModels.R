# ---------------------------------------------
# Load and preprocess Data
# ---------------------------------------------

load("train.Rda")
load("test.Rda")

DefaultTrain_X<-as.matrix(train[,1:23])
DefaultTrain_Y<-as.matrix(train[,24])[,1]

DefaultTest_X<-as.matrix(test[,1:23])
DefaultTest_Y<-as.matrix(test[,24])[,1]

#recode factors to dummies
library(dummies)
tempor<-dummy.data.frame(as.data.frame(DefaultTrain_X), names = c("SEX","EDUCATION", "MARRIAGE") , sep = ".")
tempor<-as.data.frame(lapply(tempor, function(x) as.numeric(as.character(x))))
tempor$SEX.female<-NULL
tempor$EDUCATION.other<-NULL
tempor$MARRIAGE.other<-NULL
DefaultTrain_X<-as.matrix(tempor)

tempor<-dummy.data.frame(as.data.frame(DefaultTest_X), names = c("SEX","EDUCATION", "MARRIAGE") , sep = ".")
tempor<-as.data.frame(lapply(tempor, function(x) as.numeric(as.character(x))))
tempor$SEX.female<-NULL
tempor$EDUCATION.other<-NULL
tempor$MARRIAGE.other<-NULL
DefaultTest_X<-as.matrix(tempor)

DefaultTrain_Y<-as.integer(DefaultTrain_Y)
DefaultTest_Y<-as.integer(DefaultTest_Y)

# ---------------------------------------------
# Application of the mRMR algorithm
# ---------------------------------------------

eleje<-Sys.time()
# some technical preprocessing steps
train_v2<-train
train_v2$SEX<-ordered(train_v2$SEX, levels = c("male", "female"))
train_v2$EDUCATION<-ordered(train_v2$EDUCATION, levels = c("university", "graduate school", "high school", "other"))
train_v2$MARRIAGE<-ordered(train_v2$MARRIAGE, levels = c("married", "single", "other"))
train_v2$default_nextMonth<-ordered(train_v2$default_nextMonth, levels = c("1", "0"))

library(mRMRe)
ensemble<-mRMR.ensemble(data = mRMR.data(data = train_v2), target_indices = c(24), solution_count = 1, feature_count = 6)
ensemble@filters
#ensemble@scores
#ensemble@causality_list

# build mRMR model
gam.mRMR<-gam(default_nextMonth ~ s(AGE) + s(PAY_0, k=6) + s(PAY_2, k=6) + s(PAY_5, k=5) +
                s(PAY_AMT1, k=15) + s(PAY_AMT6),
              family="binomial",data=train, method="REML")
vege<-Sys.time()
futasido<-difftime(vege,eleje, units = "min")

# Diagnostics and performance of the GAM proposed by mRMR
summary(gam.mRMR)
concurvity(gam.mRMR)
plot(gam.mRMR,pages=1,scheme=1,unconditional=TRUE)

DefaultTest_mRMR_PredY<-predict(gam.mRMR,newdata=test, type = "response")

library(pROC)
g <- roc(test$default_nextMonth ~ DefaultTest_mRMR_PredY)
g$auc # 0.7441

# ---------------------------------------------
# Application of the HSIC-Lasso algorithm
# ---------------------------------------------

# Create and export the training data in the format accepted by the HSIC-Lasso Python implementation.

train_v2<-as.data.frame(DefaultTrain_X)
train_v2$class<-DefaultTrain_Y

write.csv(train_v2, file = "trainDefault.csv", row.names=FALSE, quote=FALSE)

# Python script run on Repl.it

#from pyHSICLasso import HSICLasso
#import time
#eleje = time.time()
#hsic_lasso = HSICLasso()
#hsic_lasso.input("trainDefault.csv")
#hsic_lasso.classification()
#hsic_lasso.dump()
#print(hsic_lasso.get_features())
#vege = time.time()
#print("--- %s seconds ---" % (time.time() - start_time))

# Build HSIC-Lasso model from Python code output

#['PAY_0', 'PAY_2', 'AGE', 'PAY_4', 'PAY_3']

gam.HSIC<-gam(default_nextMonth ~ s(AGE) +
                s(PAY_0, k=6) + s(PAY_2, k=6) + s(PAY_3, k=6) + s(PAY_4, k=5),
              family="binomial",data=train, method="REML")

# Diagnostics and performance of the GAM proposed by HSIC-Lasso
summary(gam.HSIC)
concurvity(gam.HSIC)
plot(gam.HSIC,pages=1,scheme=1,unconditional=TRUE)
gam.check(gam.HSIC)

DefaultTest_HSIC_PredY<-predict(gam.HSIC,newdata=test,type="response")

library(pROC)
g <- roc(test$default_nextMonth ~ DefaultTest_HSIC_PredY)
g$auc # 0.7409

# ---------------------------------------------
# Application of the CART algorithm
# ---------------------------------------------

library(rpart)
library(rpart.plot)

eleje <- Sys.time()
model_dt<- rpart(default_nextMonth ~.,data=train, method="class")
vege<-Sys.time()
futasido<-difftime(vege, eleje, units = "mins")
futasido

# Show the decision tree
rpart.plot(model_dt)

# Evaluate on the test data
pred.test.dt <- predict(model_dt,test,type = "prob")

library(pROC)
g <- roc(test$default_nextMonth ~ pred.test.dt[,2])
g$auc # 0.6413

# -----------------------------------------------------------------------
# Application of the Random Forest algorithm with RFE feature selection
# -----------------------------------------------------------------------

library(randomForest)
library(caret)

control <- rfeControl(functions=rfFuncs, method="cv", number=5)

# some technical type conversion in the target
train$default_nextMonth<-as.numeric(train$default_nextMonth)
train$default_nextMonth[train$default_nextMonth==1] <- 0
train$default_nextMonth[train$default_nextMonth==2] <- 1

# run the RFE algorithm
eleje <- Sys.time()
results <- rfe(train[,1:23], as.matrix(train[,24]), sizes = c(1:23), rfeControl=control)
vege <- Sys.time()
futasido <- difftime(vege, eleje, units = "mins")
futasido

# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

# Evaluate on the test data
pred_rf <- predict(results,test,type = "prob")

g <- roc(test$default_nextMonth ~ pred_rf)
g$auc # Best AUC out of 20 --> 0.7591
