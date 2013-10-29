

source("BLR_functions.R")

Data <- read.table("~/STA250/Stuff/HW1/BayesLogit/breast_cancer.txt", header=TRUE)
Data[, ncol(Data)] <- Data[, ncol(Data)]=="M"

y <- Data[, ncol(Data)]
X <- as.matrix(cbind(rep(1,nrow(Data)), Data[, -ncol(Data)]))
m <- rep(1, nrow(Data))
beta.0 <- rep(0, ncol(X))
Sigma.0.inv <- diag(rep(1.0,ncol(X)))


Result1 <- bayes.logreg(m=m, y=y, X=X, beta.0=beta.0, Sigma.0.inv=Sigma.0.inv, niter=10000, burnin=1000, retune=100)
print(Result1$rate)

write.csv(Result1$sample,file="~/STA250/Stuff/HW1/BayesLogit/res_breast_cancer_MwG.csv")



