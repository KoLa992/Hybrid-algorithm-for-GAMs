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
# Full GAM model
# ---------------------------------------------

library(mgcv)
library(pROC)
library(parallel)

magok <- detectCores()
cl <- makeCluster(magok-1)

eleje<-Sys.time()
gam.mod<-bam(default_nextMonth ~ s(LIMIT_BAL) + s(AGE) + SEX + EDUCATION + MARRIAGE +
               s(PAY_0, k=6) + s(PAY_2, k=6) + s(PAY_3, k=6) + s(PAY_4, k=5) + s(PAY_5, k=5) + s(PAY_6, k=5) + s(BILL_AMT1) + s(BILL_AMT2) + s(BILL_AMT3) +
               s(BILL_AMT4) + s(BILL_AMT5) + s(BILL_AMT6) + s(PAY_AMT1, k=15) + s(PAY_AMT2) + s(PAY_AMT3) + s(PAY_AMT4) +
               s(PAY_AMT5) + s(PAY_AMT6),
             family="binomial",data=train, method="fREML", cluster=cl)
vege<-Sys.time()
futasido<-difftime(vege,eleje, units = "min")
futasido

summary(gam.mod)
concurvity(gam.mod)
gam.check(gam.mod)

plot(gam.mod,pages=1,scheme=1,unconditional=TRUE)

DefaultTest_FullGamPredY<-predict(gam.mod,newdata=test, type = "response")
g <- roc(test$default_nextMonth ~ DefaultTest_FullGamPredY)
g$auc # 0.7689

# ---------------------------------------------
# Double penalty selection GAM
# ---------------------------------------------

eleje<-Sys.time()
gam.sel<-bam(default_nextMonth ~ s(LIMIT_BAL) + s(AGE) + SEX + EDUCATION + MARRIAGE +
               s(PAY_0, k=6) + s(PAY_2, k=6) + s(PAY_3, k=6) + s(PAY_4, k=5) + s(PAY_5, k=5) + s(PAY_6, k=5) + s(BILL_AMT1) + s(BILL_AMT2) + s(BILL_AMT3) +
               s(BILL_AMT4) + s(BILL_AMT5) + s(BILL_AMT6) + s(PAY_AMT1, k=15) + s(PAY_AMT2) + s(PAY_AMT3) + s(PAY_AMT4) +
               s(PAY_AMT5) + s(PAY_AMT6),
             family="binomial",data=train, method="fREML", cluster=cl, select = TRUE)
vege<-Sys.time()
futasido<-difftime(vege,eleje, units = "min")
futasido

summary(gam.sel)
concurvity(gam.sel)
gam.check(gam.sel)

plot(gam.sel,pages=1,scheme=1,unconditional=TRUE) #Out: BILL_AMT5+6 and PAY_AMT5

DefaultTest_SelGamPredY<-predict(gam.sel,newdata=test, type = "response")
g <- roc(test$default_nextMonth ~ DefaultTest_SelGamPredY)
g$auc # 0.7681

# ---------------------------------------------
# Double penalty selection GAM v2
# ---------------------------------------------

eleje<-Sys.time()
gam.sel<-bam(default_nextMonth ~ s(LIMIT_BAL, bs="ts") + s(AGE, bs="ts") + SEX + EDUCATION + MARRIAGE +
               s(PAY_0, k=6, bs="ts") + s(PAY_2, k=6, bs="ts") + s(PAY_3, k=6, bs="ts") + s(PAY_4, k=5, bs="ts") + s(PAY_5, k=5, bs="ts") + s(PAY_6, k=5, bs="ts") +
               s(BILL_AMT1, bs="ts") + s(BILL_AMT2, bs="ts") + s(BILL_AMT3, bs="ts") + s(BILL_AMT4, bs="ts") + s(BILL_AMT5, bs="ts") + s(BILL_AMT6, bs="ts") +
               s(PAY_AMT1, k=15, bs="ts") + s(PAY_AMT2, bs="ts") + s(PAY_AMT3, bs="ts") + s(PAY_AMT4, bs="ts") + s(PAY_AMT5, bs="ts") + s(PAY_AMT6, bs="ts"),
             family="binomial",data=train, method="fREML", cluster=cl, select = TRUE)
vege<-Sys.time()
futasido<-difftime(vege,eleje, units = "min")
futasido

summary(gam.sel)
concurvity(gam.sel)
gam.check(gam.sel)

plot(gam.sel,pages=1,scheme=1,unconditional=TRUE) #Out: BILL_AMT6, PAY_AMT5

DefaultTest_SelGamPredY<-predict(gam.sel,newdata=test, type = "response")
g <- roc(test$default_nextMonth ~ DefaultTest_SelGamPredY)
g$auc # 0.7667

# ---------------------------------------------
# Non-negative garrote
# ---------------------------------------------
library(ncvreg)

eleje<-Sys.time()
DefaultTrainGarotte_X<-DefaultTrain_X
for (i in 1:ncol(DefaultTrainGarotte_X)) {
  if (min(DefaultTrainGarotte_X[,i])<0) {
    DefaultTrainGarotte_X[,i]<-DefaultTrainGarotte_X[,i]-min(DefaultTrainGarotte_X[,i])+1
  }
}

DefaultTrainGarotte_X[,1]<-predict(smooth.spline(as.matrix(DefaultTrainGarotte_X[,1]),as.matrix(train[,24])), as.matrix(DefaultTrainGarotte_X[,1]))$y
for (i in 8:ncol(DefaultTrain_X)) {
  DefaultTrainGarotte_X[,i]<-predict(smooth.spline(as.matrix(DefaultTrainGarotte_X[,i]),as.matrix(train[,24])), as.matrix(DefaultTrainGarotte_X[,i]))$y
}

garSCAD<-cv.ncvreg(DefaultTrainGarotte_X,DefaultTrain_Y,family = "binomial", penalty = "SCAD")
summary(garSCAD)
BestLamdba<-garSCAD$lambda.min
garSCAD_fixedLamdba<-ncvreg(DefaultTrain_X,DefaultTrain_Y,family = "binomial", penalty = "SCAD",lambda = BestLamdba)
garSCAD_fixedLamdba$beta
vege<-Sys.time()
futasido<-difftime(vege,eleje, units = "min")
futasido
# -PAY_6 -BILL_AMT5

gam.gar<-bam(default_nextMonth ~ s(LIMIT_BAL) + s(AGE) + SEX + EDUCATION + MARRIAGE +
               s(PAY_0, k=6) + s(PAY_2, k=6) + s(PAY_3, k=6) + s(PAY_4, k=5) + s(PAY_5, k=5) + s(BILL_AMT1) + s(BILL_AMT2) + s(BILL_AMT3) +
               s(BILL_AMT4) + s(BILL_AMT6) + s(PAY_AMT1, k=15) + s(PAY_AMT2) + s(PAY_AMT3) + s(PAY_AMT4) +
               s(PAY_AMT5) + s(PAY_AMT6),
             family="binomial",data=train, method="fREML", cluster=cl)
summary(gam.gar)
concurvity(gam.gar)
gam.check(gam.gar)

plot(gam.gar,pages=1,scheme=1,unconditional=TRUE)

DefaultTest_GarGamPredY<-predict(gam.gar,newdata=test, type = "response")
g <- roc(test$default_nextMonth ~ DefaultTest_GarGamPredY)
g$auc # 0.7671

# ---------------------------------------------
# Cosso method
# ---------------------------------------------

library(cosso)

C.obj=cosso(DefaultTrain_X,DefaultTrain_Y, family="Binomial", scale=TRUE) #Error: cannot allocate vector of size 46.6 Gb

# Subsample solution
smp_size <- 5000

ind <- sample(seq_len(nrow(train)), size = smp_size, replace = FALSE)
DT_Xrnd_AKT<-DefaultTrain_X[ind,]
DT_Yrnd_AKT<-DefaultTrain_Y[ind]

eleje<-Sys.time()
tryCatch({
  C.obj=cosso(DT_Xrnd_AKT,DT_Yrnd_AKT, family="Binomial", scale=TRUE)
  tuneRes<-tune.cosso(C.obj,10,plot.it = FALSE)
  M_AKT <- tuneRes$OptM
},
error=function(cond) {
  message(cond)
},
warning=function(cond) {
  message(cond)
},
finally={
  message("iteration")
})
vege<-Sys.time()
futasido<-difftime(vege,eleje, units = "min")
futasido

plot.cosso(C.obj,M_AKT,plottype = "Functionals")

DefaultTest_X_Scaled<-apply(DefaultTest_X, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

DefaultTest_PredCossoY<-predict.cosso(C.obj,DefaultTest_X_Scaled,M_AKT,type="fit",scale=TRUE)
DefaultTest_PredCossoY<-1/(1+exp(-DefaultTest_PredCossoY))
g <- pROC::roc(test$default_nextMonth ~ DefaultTest_PredCossoY)
g$auc # 0.7011

# concurvity check

gam.cosso<-bam(default_nextMonth ~ s(LIMIT_BAL) + EDUCATION +
               s(PAY_0, k=6) + s(PAY_2, k=6) + s(PAY_3, k=6) + s(PAY_4, k=5) + s(PAY_6, k=5),
             family="binomial",data=train, method="fREML", cluster=cl)
concurvity(gam.cosso)

# ---------------------------------------------
# Stepwise
# ---------------------------------------------

library(gam)

eleje <- Sys.time()
gam.step<-gam(default_nextMonth ~ LIMIT_BAL + AGE + SEX + EDUCATION + MARRIAGE +
                PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 +
                BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 +
                PAY_AMT5 + PAY_AMT6, family="binomial", data=train)
stepwise<-step.Gam(gam.step, scope=list("x"=~1+s(LIMIT_BAL, 10) + s(AGE, 10) + s(SEX, 10) + s(EDUCATION, 10) + s(MARRIAGE, 10) +
                                          s(PAY_0, 6) + s(PAY_2, 6) + s(PAY_3, 6) + s(PAY_4, 5) + s(PAY_5, 5) + s(PAY_6, 5) + s(BILL_AMT1, 10) + s(BILL_AMT2, 10) + s(BILL_AMT3, 10) +
                                          s(BILL_AMT4, 10) + s(BILL_AMT5, 10) + s(BILL_AMT6, 10) + s(PAY_AMT1, 15) + s(PAY_AMT2, 10) + s(PAY_AMT3, 10) + s(PAY_AMT4, 10) +
                                          s(PAY_AMT5, 10) + s(PAY_AMT6, 10)))
# Full modell selected
vege <- Sys.time()
futasido <- difftime(vege,eleje, units = "min")
futasido

# ---------------------------------------------
# Backward from full gam model
# ---------------------------------------------

# manually: always leaving the variable with the highest p-value in the summary

eleje <- Sys.time()
gam.bw<-bam(default_nextMonth ~ s(LIMIT_BAL) + SEX + MARRIAGE +
              s(PAY_0, k=6) + s(PAY_2, k=6) + s(PAY_3, k=6) + s(PAY_4, k=5) + s(PAY_6, k=5) +
              s(BILL_AMT1) + s(BILL_AMT3) +
              s(PAY_AMT1, k=15) + s(PAY_AMT2) + s(PAY_AMT4),
            family="binomial",data=train, method="fREML", cluster=cl)
vege <- Sys.time()
futasido <- difftime(vege,eleje, units = "min")
futasido

summary(gam.bw)
concurvity(gam.bw) #minden PAY except 6; minden AMT except PAY_AMT4 | worst esetben ezek is
plot(gam.bw,pages=1,scheme=1,unconditional=TRUE)

DefaultTest_BwGamPredY<-predict(gam.bw,newdata=test, type = "response")
g <- pROC::roc(test$default_nextMonth ~ DefaultTest_BwGamPredY)
g$auc # 0.7683

# ---------------------------------------------
# Modified Backfitting
# ---------------------------------------------

library(R2BayesX)

eleje<-Sys.time()
gamBayesX<-bayesx(default_nextMonth ~ s(LIMIT_BAL) + s(AGE) + SEX + EDUCATION + MARRIAGE +
                    s(PAY_0, k=6) + s(PAY_2, k=6) + s(PAY_3, k=6) + s(PAY_4, k=5) + s(PAY_5, k=5) + s(PAY_6, k=5) + s(BILL_AMT1) + s(BILL_AMT2) + s(BILL_AMT3) +
                    s(BILL_AMT4) + s(BILL_AMT5) + s(BILL_AMT6) + s(PAY_AMT1, k=15) + s(PAY_AMT2) + s(PAY_AMT3) + s(PAY_AMT4) +
                    s(PAY_AMT5) + s(PAY_AMT6),
                  family="binomial",data=train, method="STEP")
vege<-Sys.time()
futasido <- difftime(vege,eleje, units = "min")
futasido

summary(gamBayesX)
plot(gamBayesX, trem=1)

DefaultTest_PredBayesX_Y<-predict(gamBayesX,newdata = test, type = "response")
DefaultTest_PredBayesX_Y<-as.vector(DefaultTest_PredBayesX_Y$mu)
g <- pROC::roc(test$default_nextMonth ~ DefaultTest_PredBayesX_Y)
g$auc # 0.7649

# check concurvity for BayesX
gam.bayesx<-bam(default_nextMonth ~ s(LIMIT_BAL) + AGE + SEX + EDUCATION + MARRIAGE +
                  s(PAY_0, k=6) + s(PAY_2, k=6) + PAY_3 + s(PAY_4, k=5) + s(PAY_5, k=5) + s(PAY_6, k=5) + s(BILL_AMT1) +
                  s(PAY_AMT1, k=15) + s(PAY_AMT2) + s(PAY_AMT4) +
                  + PAY_AMT6,
                family="binomial",data=train, method="fREML", cluster=cl)

concurvity(gam.bayesx) # All non-linear terms except for LIMIT_BAL (and PAY_6 for observed coefficients)

# ---------------------------------------------
# GAMBoost
# ---------------------------------------------

library(GAMBoost)

Boosting<-optimGAMBoostPenalty(DefaultTrain_X,DefaultTrain_Y,family=binomial()) #Error: cannot allocate vector of size 1.8 Gb

# Subsample solution
smp_size <- 5000

ind <- sample(seq_len(nrow(train)), size = smp_size, replace = FALSE)
DT_Xrnd_AKT<-DefaultTrain_X[ind,]
DT_Yrnd_AKT<-DefaultTrain_Y[ind]

eleje<-Sys.time()
Boosting<-optimGAMBoostPenalty(DT_Xrnd_AKT,DT_Yrnd_AKT,family=binomial(), parallel=TRUE)
futasido <- difftime(vege,eleje, units = "min")
futasido

getGAMBoostSelected(Boosting) # 1  2  5  6  7  8  9 10 11 12 14 21 24 25 26

BoostingFinal<-GAMBoost(DT_Xrnd_AKT,DT_Yrnd_AKT,penalty = Boosting$penalty, family=binomial(),stepno = 51)

getGAMBoostSelected(BoostingFinal) #1  2  5  6  7  8  9 10 11 12 14 21 24 25 26

DefaultTest_PredGAMBoostY<-predict(BoostingFinal,newdata = DefaultTest_X)
DefaultTest_PredGAMBoostY<-1/(1+exp(-DefaultTest_PredGAMBoostY))
g <- pROC::roc(test$default_nextMonth ~ DefaultTest_PredGAMBoostY)
g$auc # 0.7609

# check concurvity for GAMBoost
gam.boost<-bam(default_nextMonth ~ s(LIMIT_BAL) + SEX + MARRIAGE + s(AGE) +
                 s(PAY_0, k=6) + s(PAY_2, k=6) + s(PAY_3, k=6) + s(PAY_4, k=5) + s(PAY_6, k=5) +
                 s(PAY_AMT1, k=15) + s(PAY_AMT4) + s(PAY_AMT5) + s(PAY_AMT6),
               family="binomial",data=train, method="fREML", cluster=cl)

concurvity(gam.boost)
