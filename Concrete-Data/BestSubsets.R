# ---------------------------------------------
# Load the functions of the Hybrid algorithm
# ---------------------------------------------
# We use the "ModellEpit" and "ModellEpit_B" functions that compute the GAM,
# corresponding to a solution given by its binary representation.

# In case we need the parent directorcy
parts <- unlist(strsplit(getwd(), .Platform$file.sep))
ParentDirectory <- do.call(file.path, as.list(parts[1:length(parts) - 1]))

source("HybridFunctions.R")

# ---------------------------------------------
# Load and preprocess Data and set parameters
# ---------------------------------------------

load("train.Rda")
load("test.Rda")

ConcreteTrain_X<-as.matrix(train[,1:8])
ConcreteTrain_Y<-as.matrix(train[,9])[,1]

ConcreteTest_X<-as.matrix(test[,1:8])
ConcreteTest_Y<-as.matrix(test[,9])[,1]

# Generate all possible feature subsets

populacio<-expand.grid(replicate(8, 0:1, simplify = FALSE))
egyed.akt=rbinom(8,1,1/2)
ujpopulacio<-matrix(nrow = nrow(populacio),ncol = 4)

# Compute GAMs for every feature subset generated before

for (i in 1:nrow(populacio)){
  for (j in 1:8) {
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
    mod.out<-ModellEpit(egyed.akt,ConcreteTrain_X,ConcreteTrain_Y)
    ujpopulacio[i,1]=toString(egyed.akt)
    ujpopulacio[i,2]=mod.out[1]
    ujpopulacio[i,3]=mod.out[2]
    ujpopulacio[i,4]=mod.out[3]
  }
}

# Get the list of feature subsets where the GAMs satisfy every constraint
ujpopulacio<-as.data.frame(ujpopulacio)
legjobbak<-ujpopulacio[!((as.numeric(ujpopulacio[,3])==1) | (as.numeric(ujpopulacio[,4])==1)),]
legjobbak<-legjobbak[order(legjobbak[,2],decreasing=TRUE),]


# Get the best GAM with constraints
best=as.character(legjobbak$V1[1])
best.mod<-ModellEpit_B(as.numeric(strsplit(best,",")[[1]]),ConcreteTrain_X,ConcreteTrain_Y)

# Diagnostics and performance of the best GAM
summary(best.mod)
plot(best.mod,page=1)

concurvity(best.mod)
gam.check(best.mod)

ConcreteTest_HybridPredY<-predict(best.mod,newdata=test)
Rsquared_Hybrid<-cor(ConcreteTest_Y,ConcreteTest_HybridPredY)^2 #84.479%

# Get the best GAM withOUT constraints
rendesPopulacio<-ujpopulacio[order(ujpopulacio[,2],decreasing=TRUE),]

best=as.character(rendesPopulacio$V1[23])
best.mod<-ModellEpit_B(as.numeric(strsplit(best,",")[[1]]),ConcreteTrain_X,ConcreteTrain_Y)

# Diagnostics and performance of the best GAM
summary(best.mod)
plot(best.mod,page=1)

concurvity(best.mod)
gam.check(best.mod)

ConcreteTest_HybridPredY<-predict(best.mod,newdata=test)
Rsquared_Hybrid<-cor(ConcreteTest_Y,ConcreteTest_HybridPredY)^2 #88.061%
