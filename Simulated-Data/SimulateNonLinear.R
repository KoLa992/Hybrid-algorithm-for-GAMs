
# Simulation

s <- 0.05

set.seed(1992)
X1 <- runif(1000)
X2 <- runif(1000)
X3 <- runif(1000)
X4 <- X2^3 + X3^2 + rnorm(1000, mean = 0, sd = s)
X5 <- X3^2 + rnorm(1000, mean = 0, sd = s)
X6 <- X2^2 + X4^3 + rnorm(1000, mean = 0, sd = s)
X7 <- X1*X2 + rnorm(1000, mean = 0, sd = s)

Y <- 2*X1^2 + 2*X5^3 + 2*sin(X6) + rnorm(1000, mean = 0, sd = 0.5)

SimData <- as.data.frame(cbind(X1, X2, X3, X4, X5, X6, X7, Y))

pairs(SimData)

#------------------------------------------------------------------------------------------------------------------------------

smp_size <- floor(0.7 * nrow(SimData))

set.seed(1992)
train_ind <- sample(seq_len(nrow(SimData)), size = smp_size)

train <- SimData[train_ind, ]
test <- SimData[-train_ind, ]


Train_X<-as.matrix(train[,1:7])
Train_Y<-as.matrix(train[,8])[,1]

Test_X<-as.matrix(test[,1:7])
Test_Y<-as.matrix(test[,8])[,1]

# Packages
library(mgcv)

#------------------------------------------------------------------------------------------------------------------------------

# Full GAM model

eleje <- Sys.time()
gam.mod<-gam(Y ~ s(X1) + s(X2) + s(X3) + s(X4) + s(X5) + s(X6) + s(X7),
             family="gaussian",data=train, method="REML")
vege <- Sys.time()
futasido <- vege-eleje
futasido

summary(gam.mod)
concurvity(gam.mod)
plot(gam.mod,pages=1,scheme=1,unconditional=TRUE)

Test_FullGamPredY<-predict(gam.mod,newdata=test)
Rsquared_FullBoostedGAM<-cor(Test_Y,Test_FullGamPredY)^2
Rsquared_FullBoostedGAM # 84.99298%

# Double penalty selection GAM
eleje <- Sys.time()
gam.sel<-gam(Y ~ s(X1) + s(X2) + s(X3) + s(X4) + s(X5) + s(X6) + s(X7),
             family="gaussian",data=train, method="REML",select = TRUE)
vege <- Sys.time()
futasido <- vege-eleje
futasido

summary(gam.sel)
concurvity(gam.sel)

Test_SelGamPredY<-predict(gam.sel,newdata=test)
Rsquared_SelGAM<-cor(Test_Y,Test_SelGamPredY)^2
Rsquared_SelGAM # 85.1516%

plot(gam.sel,pages=1,scheme=1,unconditional=TRUE)
gam.check(gam.sel)

# Double penalty selection GAM v2
eleje <- Sys.time()
gam.sel<-gam(Y ~ s(X1, bs="ts") + s(X2, bs="ts") + s(X3, bs="ts") + s(X4, bs="ts") +
               s(X5, bs="ts") + s(X6, bs="ts") + s(X7, bs="ts"),
             family="gaussian",data=train, method="REML",select = TRUE)
vege <- Sys.time()
futasido <- vege-eleje
futasido

summary(gam.sel)
concurvity(gam.sel)

Test_SelGamPredY<-predict(gam.sel,newdata=test)
Rsquared_SelGAM<-cor(Test_Y,Test_SelGamPredY)^2
Rsquared_SelGAM # 85.20429%

plot(gam.sel,pages=1,scheme=1,unconditional=TRUE)
k.check(gam.sel)

#: Non-negative garrote
library(ncvreg)

eleje<-Sys.time()
TrainGarotte_X<-Train_X
for (i in 1:ncol(TrainGarotte_X)) {
  if (min(TrainGarotte_X[,i])<0) {
    TrainGarotte_X[,i]<-TrainGarotte_X[,i]-min(TrainGarotte_X[,i])+1
  }
}

TrainGarotte_X[,1]<-predict(smooth.spline(as.matrix(TrainGarotte_X[,1]),as.matrix(train[,8])), as.matrix(TrainGarotte_X[,1]))$y
for (i in 2:ncol(Train_X)) {
  TrainGarotte_X[,i]<-predict(smooth.spline(as.matrix(TrainGarotte_X[,i]),as.matrix(train[,8])), as.matrix(TrainGarotte_X[,i]))$y
}

garSCAD<-cv.ncvreg(TrainGarotte_X,Train_Y,family = "gaussian", penalty = "SCAD")
summary(garSCAD)
BestLamdba<-garSCAD$lambda.min
garSCAD_fixedLamdba<-ncvreg(TrainGarotte_X,Train_Y,family = "gaussian", penalty = "SCAD",lambda = BestLamdba)
garSCAD_fixedLamdba$beta
vege<-Sys.time()
futasido<-vege-eleje
futasido
# X1, X2, X5, X6, X7

# Prediction
TestGarotte_X<-Test_X
for (i in 1:ncol(TestGarotte_X)) {
  if (min(TestGarotte_X[,i])<0) {
    TestGarotte_X[,i]<-TestGarotte_X[,i]-min(TestGarotte_X[,i])+1
  }
}

TestGarotte_X[,1]<-predict(smooth.spline(as.matrix(TestGarotte_X[,1]),as.matrix(test[,8])), as.matrix(TestGarotte_X[,1]))$y
for (i in 2:ncol(Test_X)) {
  TestGarotte_X[,i]<-predict(smooth.spline(as.matrix(TestGarotte_X[,i]),as.matrix(test[,8])), as.matrix(TestGarotte_X[,i]))$y
}

Test_GarrottePredY<-predict(garSCAD,TestGarotte_X, type="response", lambda=BestLamdba)
Rsquared_Garrotte<-cor(Test_Y,Test_GarrottePredY)^2
Rsquared_Garrotte # 85.5032%

#check concurvity for Garrote
gam.cosso<-gam(Y ~ s(X1) + s(X2) + s(X5) + s(X6) + s(X7),family="gaussian",data=train, method="REML")
summary(gam.cosso)
concurvity(gam.cosso)

#Cosso method
library(cosso)

eleje <- Sys.time()
C.obj=cosso(Train_X,Train_Y,family="Gaussian",scale=TRUE)
C.obj$wt
Kmat<-C.obj$Kmat

tune.cosso(C.obj,10,plot.it = TRUE) #optimal M=3.45
vege <- Sys.time()
futasido <- vege-eleje
futasido

plot.cosso(C.obj,3.45,plottype = "Functionals") # X1, X4, X5, X6

Test_X_Scaled<-apply(Test_X, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

Test_PredCossoY<-predict.cosso(C.obj,Test_X_Scaled,3.95,type="fit",scale=TRUE)

Rsquared_Cosso<-cor(Test_Y,Test_PredCossoY)^2
Rsquared_Cosso # 81.50509%

#check concurvity for cosso
gam.cosso<-gam(Y ~ s(X1) + s(X4) + s(X5) + s(X6),family="gaussian",data=train, method="REML")
summary(gam.cosso)
concurvity(gam.cosso)

# Stepwise selection
#library(gam)
eleje <- Sys.time()
gam.step<-gam::gam(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7,family="gaussian",data=train)
stepwise<-gam::step.Gam(gam.step, scope=list("X1"=~1+gam::s(X1,10),
                                             "X2"=~1+gam::s(X2,10),
                                             "X3"=~1+gam::s(X3,10),
                                             "X4"=~1+gam::s(X4,10),
                                             "X5"=~1+gam::s(X5,10),
                                             "X6"=~1+gam::s(X6,10),
                                             "X7"=~1+gam::s(X7,10)))
# Full modell selected
vege <- Sys.time()
futasido <- vege-eleje
futasido

# Backward from full gam model (manually: always leaving the variable with the highest p-value in the summary)
futasido <- 0

eleje <- Sys.time()
gam.bw<-mgcv::gam(Y ~ s(X1) + s(X4) + s(X5) + s(X6),
            family="gaussian",data=train, method="REML")
vege <- Sys.time()
futasido <- futasido+(vege-eleje)
futasido

summary(gam.bw)
concurvity(gam.bw)
plot(gam.bw,pages=1,scheme=1,unconditional=TRUE)

Test_FullGamBwPredY<-predict(gam.bw,newdata=test)
Rsquared_BackwGAM<-cor(Test_Y,Test_FullGamBwPredY)^2
Rsquared_BackwGAM # 85.10512%

# GAM boosting
library(GAMBoost)

eleje <- Sys.time()
Boosting<-optimGAMBoostPenalty(Train_X,Train_Y,family=gaussian())
summary(Boosting)
Boosting$selected[200]
Boosting$penalty


BoostingFinal<-GAMBoost(Train_X,Train_Y,penalty = Boosting$penalty, family=gaussian())
summary(BoostingFinal)
plot(BoostingFinal)
plot(BoostingFinal$AIC)
getGAMBoostSelected(BoostingFinal) #full model
vege <- Sys.time()
futasido <- vege-eleje
futasido

Test_PredGAMBoostY<-predict(BoostingFinal,newdata = Test_X)
Rsquared_GAMBoost<-cor(Test_Y,Test_PredGAMBoostY)^2
Rsquared_GAMBoost # 79.73929%

#Modified backfitting
library(R2BayesX)

eleje <- Sys.time()
gamBayesX<-bayesx(Y ~ s(X1) + s(X2) + s(X3) + s(X4) + s(X5) + s(X6) + s(X7),
                  family="gaussian",data=train, method="STEP")
vege <- Sys.time()
futasido <- vege-eleje
futasido

summary(gamBayesX)
plot(gamBayesX, trem=1)
Test_PredBayesX_Y<-predict(gamBayesX,newdata = test)
Rsquared_BayesX<-cor(Test_Y,Test_PredBayesX_Y)^2
Rsquared_BayesX # 84.25666%

#check concurvity for BayesX
gam.bayesx<-mgcv::gam(Y ~ s(X1) + s(X2) + s(X3) + s(X4) + s(X5),family="gaussian",data=train, method="REML")
summary(gam.bayesx)
k.check(gam.bayesx)

concurvity(gam.bayesx)

# mRMR with known feature count for global optima

library(mRMRe)

eleje <- Sys.time()
ensemble<-mRMR.ensemble(data = mRMR.data(data = train), target_indices = c(8), solution_count = 1, feature_count = 3)
ensemble@filters
ensemble@scores
ensemble@causality_list
vege <- Sys.time()
futasido <- vege-eleje
futasido

# build mRMR model
gam.mRMR<-gam(Y ~ s(X1) + 	s(X5) + 	s(X7),
              family="gaussian",data=train, method="REML")

# Diagnostics and performance of the GAM proposed by mRMR
summary(gam.mRMR)
k.check(gam.mRMR)

plot(gam.mRMR,pages=1,scheme=1,unconditional=TRUE)

concurvity(gam.mRMR)

Test_FullmRMR_PredY<-predict(gam.mRMR,newdata=test)
Rsquared_mRMR<-cor(Test_Y,Test_FullmRMR_PredY)^2
Rsquared_mRMR # 47.89369%

# Export the training data in the format accepted by the HSIC-Lasso Python implementation.

write.csv(train, file = "trainSim.csv", row.names=FALSE, quote = FALSE)

# Python script run on Repl.com

#from pyHSICLasso import HSICLasso
#import time
#eleje = time.time()
#hsic_lasso = HSICLasso()
#hsic_lasso.input("trainSim.csv")
#hsic_lasso.regression()
#hsic_lasso.dump()
#print(hsic_lasso.get_features())
#vege = time.time()
#print("--- %s seconds ---" % (time.time() - eleje))

# Build HSIC Lasso model from Python code output

#['X1', 'X4', 'X7']

gam.HSIC<-gam(Y ~ s(X1) + s(X4) + s(X7),family="gaussian",data=train, method="REML")

# Diagnostics and performance of the GAM proposed by HSIC-Lasso
summary(gam.HSIC)
k.check(gam.HSIC)

plot(gam.HSIC,pages=1,scheme=1,unconditional=TRUE)

concurvity(gam.HSIC)

Test_FullHSIC_PredY<-predict(gam.HSIC,newdata=test)
Rsquared_HSIC<-cor(Test_Y,Test_FullHSIC_PredY)^2
Rsquared_HSIC # 76.84672%

#------------------------------------------------------------------------------------------------------------------------------
# Generate all possible feature subsets

source("HybridFunctions.R")

populacio<-expand.grid(replicate(7, 0:1, simplify = FALSE))
egyed.akt=rbinom(7,1,1/2)
ujpopulacio<-matrix(nrow = nrow(populacio),ncol = 4)

# Compute GAMs for every feature subset generated before

for (i in 1:nrow(populacio)){
  for (j in 1:7) {
    egyed.akt[j]=as.numeric(populacio[i,j])
  }
  print(egyed.akt)
  flush.console()
  if(sum(egyed.akt)==0){
    ujpopulacio[i,1]=toString(egyed.akt)
    ujpopulacio[i,2]=-200000
    ujpopulacio[i,3]=0
    ujpopulacio[i,4]=0
  } else{
    mod.out<-ModellEpit(egyed.akt,Train_X,Train_Y)
    ujpopulacio[i,1]=toString(egyed.akt)
    ujpopulacio[i,2]=mod.out[1]
    ujpopulacio[i,3]=mod.out[2]
    ujpopulacio[i,4]=mod.out[3]
  }
}

# Get the list of feature subsets where the GAMs satisfy every constraint
ujpopulacio<-as.data.frame(ujpopulacio)
legjobbak<-ujpopulacio[((as.numeric(ujpopulacio[,3])==1) & (as.numeric(ujpopulacio[,4])==1)),]
legjobbak<-legjobbak[order(legjobbak[,2],decreasing=TRUE),]


# Get the best GAM with constraints
best=as.character(legjobbak$V1[1])
best.mod<-ModellEpit_B(as.numeric(strsplit(best,",")[[1]]),Train_X,Train_Y)

# Diagnostics and performance of the best GAM
summary(best.mod)
plot(best.mod,page=1)

concurvity(best.mod)
k.check(best.mod)

Test_HybridPredY<-predict(best.mod,newdata=test)
Rsquared_Hybrid<-cor(Test_Y,Test_HybridPredY)^2
Rsquared_Hybrid # 85.30665%

# Get the best GAM withOUT constraints
rendesPopulacio<-ujpopulacio[order(ujpopulacio[,2],decreasing=TRUE),]

best=as.character(rendesPopulacio$V1[1])
best.mod<-ModellEpit_B(as.numeric(strsplit(best,",")[[1]]),Train_X,Train_Y)

# Diagnostics and performance of the best GAM
summary(best.mod)
plot(best.mod,page=1)

concurvity(best.mod)
k.check(best.mod)

Test_HybridNoCrPredY<-predict(best.mod,newdata=test)
Rsquared_HybridNoCr<-cor(Test_Y,Test_HybridNoCrPredY)^2
Rsquared_HybridNoCr # 85.02785%

#------------------------------------------------------------------------------------------------------------------------------
# Mass testing Hybrid

source("HybridFunctions.R")

runnumber=30
eredmenyek<-matrix(nrow = runnumber,ncol = 6)

for (i in 1:runnumber) {
  eleje<-Sys.time()
  best<-Hibrid(7,13,10,0.9,0.05,0.1,0.35,Train_X,Train_Y)
  vege<-Sys.time()
  futasido<-vege-eleje
  eredmenyek[i,1]=best[1]
  eredmenyek[i,2]=best[2]
  eredmenyek[i,3]=best[3]
  eredmenyek[i,4]=best[4]
  eredmenyek[i,5]=best[5]
  eredmenyek[i,6]=futasido
}

write.csv(eredmenyek, file = "resHibrid_20220423.csv", row.names=FALSE, quote = FALSE)
