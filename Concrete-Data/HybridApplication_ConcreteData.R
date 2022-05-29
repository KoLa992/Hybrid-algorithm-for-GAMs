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

runnumber=10
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
best.mod<-ModellEpit_B(as.numeric(strsplit(eredmenyek[3,1],",")[[1]]),ConcreteTrain_X,ConcreteTrain_Y,
                       "gaussian",c(""),magok-1)

# Diagnostics and performance of the GAM proposed by the Hybrid algorithm
summary(best.mod)
plot(best.mod,page=1,scheme=1,unconditional=TRUE)

concurvity(best.mod)
gam.check(best.mod)

# Outlier effect - Cook distance

cook <- cooks.distance(best.mod)
length(cook[cook > 1]) # No influential observations

# Performance - R^2

ConcreteTest_HybridPredY<-predict(best.mod,newdata=test)
Rsquared_Hybrid<-cor(ConcreteTest_Y,ConcreteTest_HybridPredY)^2 #84.363%

# Other performance metrics

lsa::cosine(as.vector(ConcreteTest_HybridPredY), ConcreteTest_Y) # 0.9841959

MLmetrics::RMSE(ConcreteTest_HybridPredY, ConcreteTest_Y) # 6.799805

MLmetrics::RMSPE(ConcreteTest_HybridPredY, ConcreteTest_Y) # 0.2672085

# Handling the possible negative values for logarithm
AbsoluteMin <- min(min(ConcreteTest_HybridPredY), min(ConcreteTest_Y))

if (AbsoluteMin < 0) {
  MLmetrics::RMSLE((ConcreteTest_HybridPredY - AbsoluteMin + 1), (ConcreteTest_Y - AbsoluteMin + 1))
} else {
  MLmetrics::RMSLE(ConcreteTest_HybridPredY, ConcreteTest_Y)
} # 0.2149622

MLmetrics::MAE(ConcreteTest_HybridPredY, ConcreteTest_Y) # 5.412294

MLmetrics::MAPE(ConcreteTest_HybridPredY, ConcreteTest_Y) # 0.1904206

sum(log(cosh(ConcreteTest_HybridPredY - ConcreteTest_Y)))/length(ConcreteTest_Y) # 4.770746
