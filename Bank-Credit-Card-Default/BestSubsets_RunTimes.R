# ---------------------------------------------
# Load the functions of the Hybrid algorithm
# ---------------------------------------------
# We use the "ModellEpit" function that computes the GAM corresponding to a solution given by its binary representation.

# In case we need the parent directorcy
parts <- unlist(strsplit(getwd(), .Platform$file.sep))
ParentDirectory <- do.call(file.path, as.list(parts[1:length(parts) - 1]))

source("HybridFunctions.R")

# ---------------------------------------------
# Load and preprocess Data and set parameters
# ---------------------------------------------

load("train.Rda")
load("test.Rda")

DefaultTrain_X<-as.matrix(train[,1:23])
DefaultTrain_Y<-as.matrix(train[,24])[,1]

DefaultTest_X<-as.matrix(test[,1:23])
DefaultTest_Y<-as.matrix(test[,24])[,1]

# Recode factors to dummies
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

# Set dummy list
faktorok<-c("SEX.male","EDUCATION.graduate.school","EDUCATION.high.school",
            "EDUCATION.university","MARRIAGE.married","MARRIAGE.single")

# Detect cores of CPU
library(parallel)
cores<-detectCores()

# ----------------------------------------------------------------------------
# Parallel computation of models on multiple cores for some random solutions
# ----------------------------------------------------------------------------

replicNumber <- 100

futasidok <- rep(NA, replicNumber)
egyedlista <- list()

for (i in 1:replicNumber) {
  egyed.akt=rbinom(26,1,1/2)
  egyedlista[[i]] <- egyed.akt
  eleje<-Sys.time()
  mod.out<-ModellEpit(egyed.akt,DefaultTrain_X,DefaultTrain_Y,"binomial",faktorok,1,cores-1)
  vege<-Sys.time()
  futasidok[i]<-difftime(vege, eleje, units = "mins")
  print(i)
}

mean(futasidok)
sd(futasidok)
hist(futasidok)

Exportalni <- do.call(rbind,egyedlista)
colnames(Exportalni) <- colnames(DefaultTrain_X)
Exportalni <- as.data.frame(Exportalni)
Exportalni$RunTime <- futasidok

# -------------------------------------------------------------------------------------------------
# Computation of models on multiple cores for some random solutions on subsamples of a given size
# -------------------------------------------------------------------------------------------------

smp_size <- 5000
replicNumber <- 100

futasidok_sample <- rep(NA, replicNumber)

ind <- sample(seq_len(nrow(DefaultTrain_X)), size = smp_size, replace = FALSE)
DT_Xrnd_AKT<-DefaultTrain_X[ind,]
DT_Yrnd_AKT<-DefaultTrain_Y[ind]

for (i in 1:replicNumber) {
  eleje<-Sys.time()
  mod.out<-ModellEpit(egyedlista[[i]],DT_Xrnd_AKT,DT_Yrnd_AKT,"binomial",faktorok,1,cores-1)
  vege<-Sys.time()
  futasidok_sample[i]<-difftime(vege, eleje, units = "mins")
  print(i)
}

mean(futasidok_sample)
sd(futasidok_sample)
hist(futasidok_sample)

Exportalni$RunTimeSubSample <- futasidok_sample

write.table(Exportalni, file = "FullData_RunTimes.csv", sep = ";", dec = ',', quote = FALSE,
            row.names = FALSE)

# -------------------------------------------------------------------------------------------
# Simultaneous computation of models for some random solutions on subsamples of a given size
# -------------------------------------------------------------------------------------------

library(foreach)
library(parallel)
library(doParallel)

cl <- makeCluster(11)
registerDoParallel(cl)

eredmenyek<-matrix(nrow = replicNumber,ncol = 1)

eleje<-Sys.time()
eredmenyek <- foreach (i = 1:replicNumber, .combine='rbind', .export="ModellEpit") %dopar% {
    mod.out<-ModellEpit(egyedlista[[i]],DT_Xrnd_AKT,DT_Yrnd_AKT,"binomial",faktorok,1,cores-1)
    c(mod.out[1])
}
stopCluster(cl)
vege<-Sys.time()
futasido <- difftime(vege, eleje, units = "mins")
futasido # Time difference of 3.374438 mins


# -------------------------------------------------------------------------------------------
# Get the pseudo R-squared of the full model on different subsamples of a given size
# -------------------------------------------------------------------------------------------
smp_size <- 5000
replicNumber <- 100
Rnegyzetek<-array(rep(NaN, 5),c(replicNumber))

mod.out<-ModellEpit(rep(1,26),DefaultTrain_X,DefaultTrain_Y,"binomial",faktorok,1,cores-1)
Rnegyzet<-mod.out[1] # 0.2173916

eleje<-Sys.time()
for (i in 1:replicNumber) {
  ind <- sample(seq_len(nrow(DefaultTrain_X)), size = smp_size, replace = FALSE)
  DT_Xrnd_AKT<-DefaultTrain_X[ind,]
  DT_Yrnd_AKT<-DefaultTrain_Y[ind]
  mod.out<-ModellEpit(rep(1,26),DT_Xrnd_AKT,DT_Yrnd_AKT,"binomial",faktorok,1,cores-1)
  Rnegyzetek[i]<-mod.out[1]
  print(i)
}
vege<-Sys.time()
futasido<-difftime(vege, eleje, units = "mins")

hist(Rnegyzetek)

mean(abs(Rnegyzetek-Rnegyzet))
max(abs(Rnegyzetek-Rnegyzet))

mean(abs(Rnegyzetek-Rnegyzet))/Rnegyzet*100
max(abs(Rnegyzetek-Rnegyzet))/Rnegyzet*100

shapiro.test(Rnegyzetek)
summary(Rnegyzetek)
round(sd(Rnegyzetek)*2,3)
sd(Rnegyzetek)*2/Rnegyzet*100

write.table(Rnegyzetek, file = "SubSample_Rsq.csv", sep = ";", dec = ',', quote = FALSE,
            row.names = FALSE)