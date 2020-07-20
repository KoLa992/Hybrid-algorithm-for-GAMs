# ---------------------------------------------
# Load and preprocess Data and set parameters
# ---------------------------------------------

load("train.Rda")
load("test.Rda")

ConcreteTrain_X<-as.matrix(train[,1:8])
ConcreteTrain_Y<-as.matrix(train[,9])[,1]

ConcreteTest_X<-as.matrix(test[,1:8])
ConcreteTest_Y<-as.matrix(test[,9])[,1]

# mRMR with known feature count for global optima

library(mRMRe)
ensemble<-mRMR.ensemble(data = mRMR.data(data = train), target_indices = c(9), solution_count = 1, feature_count = 4)
ensemble@filters
ensemble@scores
ensemble@causality_list

# build mRMR model
gam.mRMR<-gam(CompressiveStrength ~ s(Cement) + 	s(BlastFurnaceSlag, k=20) + 	s(Superplasticizer, k=20) + 
                s(Age, k=8),family="gaussian",data=train, method="REML")

# Diagnostics and performance of the GAM proposed by mRMR
summary(gam.mRMR)
gam.check(gam.mRMR)

plot(gam.mRMR,pages=1,scheme=1,unconditional=TRUE)

concurvity(gam.mRMR)

ConcreteTest_FullmRMR_PredY<-predict(gam.mRMR,newdata=test)
Rsquared_mRMR<-cor(ConcreteTest_Y,ConcreteTest_FullmRMR_PredY)^2 #81.580%

# Export the training data in the format accepted by the HSIC-Lasso Python implementation.

write.csv(train, file = "trainConcr.csv", row.names=FALSE)

# Python script run on Repl.it

#from pyHSICLasso import HSICLasso
#hsic_lasso = HSICLasso()
#hsic_lasso.input("trainConcr.csv")
#hsic_lasso.regression()
#hsic_lasso.dump()
#print(hsic_lasso.get_features())

# Build HSIC Lasso model from Python code output

#['Age', 'Cement', 'Water', 'Superplasticizer', 'BlastFurnaceSlag']

gam.HSIC<-gam(CompressiveStrength ~ s(Cement) + 	s(Water, k=20) + 	s(Superplasticizer, k=20) + 
                s(Age, k=8) + 	s(BlastFurnaceSlag, k=30),family="gaussian",data=train, method="REML")

# Diagnostics and performance of the GAM proposed by HSIC-Lasso
summary(gam.HSIC)
gam.check(gam.HSIC)

plot(gam.HSIC,pages=1,scheme=1,unconditional=TRUE)

concurvity(gam.HSIC)

ConcreteTest_FullHSIC_PredY<-predict(gam.HSIC,newdata=test)
Rsquared_HSIC<-cor(ConcreteTest_Y,ConcreteTest_FullHSIC_PredY)^2 #86.382%