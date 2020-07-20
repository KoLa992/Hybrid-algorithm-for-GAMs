# ---------------------------------------------
# Load the functions of the Hybrid algorithm
# ---------------------------------------------

# In case we need the parent directorcy
parts <- unlist(strsplit(getwd(), .Platform$file.sep))
ParentDirectory <- do.call(file.path, as.list(parts[1:length(parts) - 1]))

source("HybridFunctions_Parallelized.R")

# ---------------------------------------------
# Load and preprocess Data
# ---------------------------------------------

load("train.Rda")
load("test.Rda")

ConcreteTrain_X<-as.matrix(train[,1:8])
ConcreteTrain_Y<-as.matrix(train[,9])[,1]

ConcreteTest_X<-as.matrix(test[,1:8])
ConcreteTest_Y<-as.matrix(test[,9])[,1]

# -------------------------------------------------
# Hybrid algorithm run 30 times
# -------------------------------------------------

test_index <- 38

library(parallel)
magok<-detectCores()

runnumber=30
eredmenyek<-matrix(nrow = runnumber,ncol = 6)

for (i in 1:runnumber) {
  eleje<-Sys.time()
  best<-Hibrid(8,20,20,0.9,0.05,0.1,0.35,5,ConcreteTrain_X,ConcreteTrain_Y,"gaussian",c(""),1,magok-1)
  vege<-Sys.time()
  futasido<-vege-eleje
  eredmenyek[i,1]=best[1]
  eredmenyek[i,2]=best[2]
  eredmenyek[i,3]=best[3]
  eredmenyek[i,4]=best[4]
  eredmenyek[i,5]=best[5]
  eredmenyek[i,6]=futasido
}

write.table(eredmenyek, file = paste("resHibrid_",test_index,".csv", sep = ""), sep = ";", dec = ',',
          quote = FALSE, row.names = FALSE)

#get the best modell --> row index of "eredmenyek" needs to be updated!!!
best.mod<-ModellEpit_B(as.numeric(strsplit(eredmenyek[1,1],",")[[1]]),ConcreteTrain_X,ConcreteTrain_Y,
                       "gaussian",c(""),magok-1)

# Diagnostics and performance of the GAM proposed by the Hybrid algorithm
summary(best.mod)
plot(best.mod,page=1)

concurvity(best.mod)
gam.check(best.mod)

ConcreteTest_HybridPredY<-predict(best.mod,newdata=test)
Rsquared_Hybrid<-cor(ConcreteTest_Y,ConcreteTest_HybridPredY)^2 #84.363%
