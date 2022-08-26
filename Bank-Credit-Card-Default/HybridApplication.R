# ---------------------------------------------
# Load the functions of the Hybrid algorithm
# ---------------------------------------------

# In case we need the parent directorcy
parts <- unlist(strsplit(getwd(), .Platform$file.sep))
ParentDirectory <- do.call(file.path, as.list(parts[1:length(parts) - 1]))

source("HybridFunctions_Parallelized_withOutput.R")

# ---------------------------------------------
# Load and preprocess Data and set parameters
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

#create subsamples for faster runtimes
smp_size <- 5000
replicNumber <- 100

ind <- sample(seq_len(nrow(DefaultTrain_X)), size = smp_size, replace = FALSE)
DT_Xrnd_AKT<-DefaultTrain_X[ind,]
DT_Yrnd_AKT<-DefaultTrain_Y[ind]

#define factor list
faktorok<-c("SEX.male","EDUCATION.graduate.school","EDUCATION.high.school",
            "EDUCATION.university","MARRIAGE.married","MARRIAGE.single")

#delete unnecesary object and run garbage collection to free up some RAM
train<-NULL
test<-NULL
tempor<-NULL

#gcinfo(TRUE)
gc()

# -------------------------------------------------
# Hybrid algorithm run 20 times in periods of five
# -------------------------------------------------
runnumber=20*5
eredmenyek<-matrix(nrow = runnumber,ncol = 6)

library(parallel)
magok<-detectCores()

for (i in 1:runnumber) {
  eleje<-Sys.time()
  best<-Hibrid(26,60,6,0.9,0.05,0.1,0.35,125,DT_Xrnd_AKT,DT_Yrnd_AKT,"binomial",faktorok,1,magok-1)
  vege<-Sys.time()
  futasido<-vege-eleje
  eredmenyek[i,1]=best[1]
  eredmenyek[i,2]=best[2]
  eredmenyek[i,3]=best[3]
  eredmenyek[i,4]=best[4]
  eredmenyek[i,5]=best[5]
  eredmenyek[i,6]=futasido
}

write.csv(eredmenyek, file = "resHibrid_Default.csv", sep = ";", dec = ',', quote = FALSE,
          row.names = FALSE)

#get the best modell --> row index of "eredmenyek" needs to be updated!!!
best.mod<-ModellEpit_B(as.numeric(strsplit(eredmenyek[25,1],",")[[1]]),DT_Xrnd_AKT,DT_Yrnd_AKT,
                       "binomial",faktorok,5)

# Diagnostics and performance of the GAM proposed by the Hybrid algorithm
summary(best.mod)
par(mfrow = c(2, 2), mar = c(3, 3, 2, 1), mgp = c(1.5, 0.5, 0))
plot(best.mod, page = 1, scheme = 1)

concurvity(best.mod)
gam.check(best.mod)

DefaultTest<-as.data.frame(DefaultTest_X)
DefaultTest$target<-DefaultTest_Y

DefaultTest$PredProb <- predict(best.mod, newdata=DefaultTest, type="response")

library(pROC)
g <- roc(target ~ PredProb, data = DefaultTest)
g$auc # Best AUC out of 20 --> 0.7488

# Outlier effect - Cook distance

cook <- cooks.distance(best.mod)
length(cook[cook > 1]) # Some influential observations

# Refit without influential observations

best.mod<-ModellEpit_B(as.numeric(strsplit(eredmenyek[25,1],",")[[1]]),
                       DT_Xrnd_AKT[cook < 1,],DT_Yrnd_AKT[cook < 1],"binomial",faktorok,5)

summary(best.mod)
plot(best.mod,page=1,scheme=1) # No Visible Difference

DefaultTest$PredProb <- predict(best.mod, newdata=DefaultTest, type="response")

library(pROC)
g <- roc(target ~ PredProb, data = DefaultTest)
g$auc # 0.7447 --> Slight difference
