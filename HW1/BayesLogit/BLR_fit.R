

#library(mvtnorm)
#library(coda)


args <- commandArgs(TRUE)
cat(paste("Command-line arguments:\n"))
print(args)

sim_start <- 1000
length.datasets <- 200


if (length(args)==0){
  sinkit <- FALSE
  sim_num <- sim_start + 1
  set.seed(1330931)
} else {
  # Sink output to file?
  sinkit <- TRUE
  # Decide on the job number, usually start at 1000:
  sim_num <- sim_start + as.numeric(args[1])
  # Set a different random seed for every job number!!!
  set.seed(762*sim_num + 1330931)
}

#file_read <- paste("~/Documents/STA250/blr_data_",sim_num,".csv", sep="")
#file_write <- paste("~/Documents/STA250/blr_res_",sim_num,".csv", sep="")


file_read <- paste("~/STA250/Stuff/HW1/BayesLogit/data/blr_data_", sim_num, ".csv", sep="")
file_write <- paste("~/STA250/Stuff/HW1/BayesLogit/results/blr_res_", sim_num, ".csv", sep="")

beta.0 <- matrix(c(0,0))
Sigma.0.inv <- diag(rep(1.0,ncol(X)))
niter <- 15000
burnin <- 5000
retune <- 100


Data <- read.csv(file=file_read, header=TRUE)
y <- Data[,1]
X <- as.matrix(Data[,3:4], ncol=2)
m <- Data[,2] 


source("BLR_functions.R")  
#source("~/Documents/STA250/HW1/BLR_functions.R")

result <- bayes.logreg(m=m, y=y, X=X, beta.0=beta.0, Sigma.0.inv=Sigma.0.inv, niter=niter, burnin=burnin, retune=retune)

BLR_result(result$sample, file_write=file_write)



