RuntimeData <- readxl::read_excel("Compare Choosing Number of Basis Functions.xlsx", sheet = "Compare")

hist(RuntimeData$RunTime_Increasing_k)
hist(RuntimeData$Runtime_fix_k)

shapiro.test(RuntimeData$RunTime_Increasing_k)
shapiro.test(RuntimeData$Runtime_fix_k)

# Normality strongly accepted for Increasing k
# Normality mildly accepted for fix k (alpha= 1% and 5%)

# Differences on boxplot
boxplot(RuntimeData)
summary(RuntimeData)

# t-test not assuming equal varances
t.test(RuntimeData$RunTime_Increasing_k, RuntimeData$Runtime_fix_k,
       alternative = "less",
       var.equal = FALSE) # t = -8.8846, df = 40.426, p-value = 2.373e-11
