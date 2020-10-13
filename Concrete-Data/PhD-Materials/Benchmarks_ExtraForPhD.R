library(mgcv)
# ---------------------------------------------
# Load and preprocess Data and set parameters
# ---------------------------------------------

load("train.Rda")
load("test.Rda")

ConcreteTrain_X<-as.matrix(train[,1:8])
ConcreteTrain_Y<-as.matrix(train[,9])[,1]

ConcreteTest_X<-as.matrix(test[,1:8])
ConcreteTest_Y<-as.matrix(test[,9])[,1]

# Full GAM model
eleje <- Sys.time()
gam.mod<-gam(CompressiveStrength ~ s(Cement) + 	s(BlastFurnaceSlag) + 	s(FlyAsh, k=20) + 	s(Water, k=20) + 	s(Superplasticizer, k=20) + 	s(CoarseAggregate) + 	s(FineAggregate, k=20) + 	s(Age, k=8),family="gaussian",data=train, method="REML")
vege <- Sys.time()
futasido <- vege-eleje
futasido

summary(gam.mod)
concurvity(gam.mod)
plot(gam.mod,pages=1,scheme=1,unconditional=TRUE)

ConcreteTest_FullGamPredY<-predict(gam.mod,newdata=test)
Rsquared_FullBoostedGAM<-cor(ConcreteTest_Y,ConcreteTest_FullGamPredY)^2 #88.119%

# Double penalty selection GAM
eleje <- Sys.time()
gam.sel<-gam(CompressiveStrength ~ s(Cement) + 	s(BlastFurnaceSlag) + 	s(FlyAsh, k=20) + 	s(Water, k=20) + 	s(Superplasticizer, k=20) + 	s(CoarseAggregate) + 	s(FineAggregate, k=30) + 	s(Age, k=8),family="gaussian",data=train, method="REML",select = TRUE)
vege <- Sys.time()
futasido <- vege-eleje
futasido

summary(gam.sel)
concurvity(gam.sel)

ConcreteTest_SelGamPredY<-predict(gam.sel,newdata=test)
Rsquared_SelGAM<-cor(ConcreteTest_Y,ConcreteTest_SelGamPredY)^2 #88.077%

plot(gam.sel,pages=1,scheme=1,unconditional=TRUE)
gam.check(gam.sel)

# Double penalty selection GAM v2
eleje <- Sys.time()
gam.sel<-gam(CompressiveStrength ~ s(Cement, bs="ts") + 	s(BlastFurnaceSlag, bs="ts") + 	s(FlyAsh, k=20, bs="ts") + 	s(Water, k=20, bs="ts") + 	s(Superplasticizer, k=20, bs="ts") + 	s(CoarseAggregate, bs="ts") + 	s(FineAggregate, k=30, bs="ts") + 	s(Age, k=8, bs="ts"),family="gaussian",data=train, method="REML",select = TRUE)
vege <- Sys.time()
futasido <- vege-eleje
futasido

summary(gam.sel)
concurvity(gam.sel)

ConcreteTest_SelGamPredY<-predict(gam.sel,newdata=test)
Rsquared_SelGAM<-cor(ConcreteTest_Y,ConcreteTest_SelGamPredY)^2 #87.694%

plot(gam.sel,pages=1,scheme=1,unconditional=TRUE) #leaves the full model
gam.check(gam.sel)

#5: Non-negative garrote
library(ncvreg)

eleje<-Sys.time()
ConcreteTrainGarotte_X<-ConcreteTrain_X
for (i in 1:ncol(ConcreteTrainGarotte_X)) {
  if (min(ConcreteTrainGarotte_X[,i])<0) {
    ConcreteTrainGarotte_X[,i]<-ConcreteTrainGarotte_X[,i]-min(ConcreteTrainGarotte_X[,i])+1
  }
}

ConcreteTrainGarotte_X[,1]<-predict(smooth.spline(as.matrix(ConcreteTrainGarotte_X[,1]),as.matrix(train[,9])), as.matrix(ConcreteTrainGarotte_X[,1]))$y
for (i in 2:ncol(ConcreteTrain_X)) {
  ConcreteTrainGarotte_X[,i]<-predict(smooth.spline(as.matrix(ConcreteTrainGarotte_X[,i]),as.matrix(train[,9])), as.matrix(ConcreteTrainGarotte_X[,i]))$y
}

garSCAD<-cv.ncvreg(ConcreteTrainGarotte_X,ConcreteTrain_Y,family = "gaussian", penalty = "SCAD")
summary(garSCAD)
BestLamdba<-garSCAD$lambda.min
garSCAD_fixedLamdba<-ncvreg(ConcreteTrainGarotte_X,ConcreteTrain_Y,family = "gaussian", penalty = "SCAD",lambda = BestLamdba)
garSCAD_fixedLamdba$beta
vege<-Sys.time()
futasido<-vege-eleje
futasido
# full model

# Prediction
ConcreteTestGarotte_X<-ConcreteTest_X
for (i in 1:ncol(ConcreteTestGarotte_X)) {
  if (min(ConcreteTestGarotte_X[,i])<0) {
    ConcreteTestGarotte_X[,i]<-ConcreteTestGarotte_X[,i]-min(ConcreteTestGarotte_X[,i])+1
  }
}

ConcreteTestGarotte_X[,1]<-predict(smooth.spline(as.matrix(ConcreteTestGarotte_X[,1]),as.matrix(test[,9])), as.matrix(ConcreteTestGarotte_X[,1]))$y
for (i in 2:ncol(ConcreteTest_X)) {
  ConcreteTestGarotte_X[,i]<-predict(smooth.spline(as.matrix(ConcreteTestGarotte_X[,i]),as.matrix(test[,9])), as.matrix(ConcreteTestGarotte_X[,i]))$y
}

ConcreteTest_GarrottePredY<-predict(garSCAD,ConcreteTestGarotte_X, type="response", lambda=BestLamdba)
Rsquared_Garrotte<-cor(ConcreteTest_Y,ConcreteTest_GarrottePredY)^2 #78.200%

#Cosso method
library(cosso)

eleje <- Sys.time()
C.obj=cosso(ConcreteTrain_X,ConcreteTrain_Y,family="Gaussian",scale=TRUE)
C.obj$wt
Kmat<-C.obj$Kmat

tune.cosso(C.obj,10,plot.it = TRUE) #optimal M=4.45
vege <- Sys.time()
futasido <- vege-eleje
futasido

plot.cosso(C.obj,4.45,plottype = "Functionals")

ConcreteTest_X_Scaled<-apply(ConcreteTest_X, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

ConcreteTest_PredCossoY<-predict.cosso(C.obj,ConcreteTest_X_Scaled,4.45,type="fit",scale=TRUE)

Rsquared_Cosso<-cor(ConcreteTest_Y,ConcreteTest_PredCossoY)^2 #85.045%

#check concurvity for cosso
gam.cosso<-gam(CompressiveStrength ~ s(Cement) + 	s(BlastFurnaceSlag) + 	s(FlyAsh, k=20) + 	s(Water, k=20) + 	s(Age, k=8),family="gaussian",data=train, method="REML")
summary(gam.cosso)
concurvity(gam.cosso)#flyash + water||Cement+BlastFurnaceSlag
gam.check(gam.cosso)

# Stepwise selection
library(gam)
eleje <- Sys.time()
gam.step<-gam(CompressiveStrength ~ Cement + BlastFurnaceSlag + 	FlyAsh + 	Water + 	Superplasticizer + 	CoarseAggregate + 	FineAggregate + 	Age,family="gaussian",data=train)
stepwise<-step.Gam(gam.step, scope=list("x"=~1+s(Cement,10)+s(BlastFurnaceSlag,10)+s(FlyAsh,10)+s(Water,10)+s(Superplasticizer,10)+s(CoarseAggregate,10)+s(FineAggregate,10)+s(Age,8)))
# Full modell selected
vege <- Sys.time()
futasido <- vege-eleje
futasido

# Backward from full gam model (manually: always leaving the variable with the highest p-value in the summary)
futasido <- 0

eleje <- Sys.time()
gam.bw<-gam(CompressiveStrength ~ s(Cement) + 	s(BlastFurnaceSlag) + 	s(FlyAsh, k=20) + 	s(Water, k=20) + 	s(Superplasticizer, k=20) + 	s(FineAggregate, k=20) + 	s(Age, k=8),family="gaussian",data=train, method="REML")
vege <- Sys.time()
futasido <- futasido+(vege-eleje)
futasido

summary(gam.bw)
concurvity(gam.bw)
plot(gam.bw,pages=1,scheme=1,unconditional=TRUE)

ConcreteTest_FullGamBwPredY<-predict(gam.bw,newdata=test)
Rsquared_BackwGAM<-cor(ConcreteTest_Y,ConcreteTest_FullGamBwPredY)^2 #88.107%

# GAM boosting
library(GAMBoost)

eleje <- Sys.time()
Boosting<-optimGAMBoostPenalty(ConcreteTrain_X,ConcreteTrain_Y,family=gaussian())
summary(Boosting)
Boosting$selected[200]
Boosting$penalty


BoostingFinal<-GAMBoost(ConcreteTrain_X,ConcreteTrain_Y,penalty = Boosting$penalty, family=gaussian())
summary(BoostingFinal)
plot(BoostingFinal)
plot(BoostingFinal$AIC)
getGAMBoostSelected(BoostingFinal) #full model
vege <- Sys.time()
futasido <- vege-eleje
futasido

ConcreteTest_PredGAMBoostY<-predict(BoostingFinal,newdata = ConcreteTest_X)
Rsquared_GAMBoost<-cor(ConcreteTest_Y,ConcreteTest_PredGAMBoostY)^2 #89.239%

#Modified backfitting
library(R2BayesX)

eleje <- Sys.time()
gamBayesX<-bayesx(CompressiveStrength ~ s(Cement) + 	s(BlastFurnaceSlag) + 	s(FlyAsh, k=20) + 	s(Water, k=20) + 	s(Superplasticizer, k=20) + 	s(CoarseAggregate) + 	s(FineAggregate, k=20) + 	s(Age, k=8),family="gaussian",data=train, method="STEP")
vege <- Sys.time()
futasido <- vege-eleje
futasido

summary(gamBayesX)
plot(gamBayesX, trem=1)
ConcreteTest_PredBayesX_Y<-predict(gamBayesX,newdata = test)
Rsquared_BayesX<-cor(ConcreteTest_Y,ConcreteTest_PredBayesX_Y)^2 #87.952%

#check concurvity for BayesX
gam.bayesx<-gam(CompressiveStrength ~ s(Cement) + 	s(BlastFurnaceSlag) + 	s(Superplasticizer, k=20) + 	s(CoarseAggregate) + 	s(Age, k=8),family="gaussian",data=train, method="REML")
summary(gam.bayesx)
gam.check(gam.bayesx)

concurvity(gam.bayesx)
