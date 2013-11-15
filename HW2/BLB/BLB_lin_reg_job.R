
mini <- FALSE
#mini <- TRUE
verbose <- TRUE

#============================== Setup for running on Gauss... ==============================#

args <- commandArgs(TRUE)

cat("Command-line arguments:\n")
print(args)

####
# sim_start ==> Lowest possible dataset number
###

###################
sim_start <- 1000
###################

if (length(args)==0){
  sim_num <- sim_start + 1
  set.seed(121231)
} else {
  # SLURM can use either 0- or 1-indexing...
  # Lets use 1-indexing here...
  sim_num <- sim_start + as.numeric(args[1])
  sim_seed <- (762*(sim_num-1) + 121231)
}

cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))

# Find r and s indices:

r_index <- as.integer(args) %% 50 
if(r_index == 0) r_index <- 50

s_index <- ceiling(as.integer(args)/50)

#============================== Run the simulation study ==============================#

# Load packages:
library(BH)
library(bigmemory.sri)
library(bigmemory)
library(biganalytics)

# I/O specifications:
datapath <- "/home/pdbaines/data"
outpath <- "output/"

# mini or full?
if (mini){
	rootfilename <- "blb_lin_reg_mini"
} else {
	rootfilename <- "blb_lin_reg_data"
}


# Filenames:

data_file <- paste(datapath, "/", rootfilename, ".txt", sep="")
bin_file <- paste(datapath, "/", rootfilename, ".bin", sep="")
desc_file <- paste(datapath, "/", rootfilename, ".desc", sep="")


# Set up I/O stuff:
# Attach big.matrix :

if (verbose){
  cat("Attaching big.matrix...\n")
}
data <- attach.big.matrix(dget(desc_file),backingpath=datapath)

# Remaining BLB specs:

s <- 5
r <- 50
gamma <- .7
n <- nrow(data)
p <- ncol(data) -1
b <- floor(n ^ gamma)

# Extract the subset:

set.seed(121231 + s_index)
subset_index <- sample(1:n, b)

# Reset simulation seed:
set.seed(121231 + 365 * as.numeric(args))
# Bootstrap dataset:

data_bst <- data[subset_index,]
weight <- rmultinom(1, n, rep(1/b,b))

# Fit lm:

model <- lm(data_bst[,p+1] ~ . -1 , weights=weight, data=as.data.frame(data_bst[,-(p+1)]))

# Output file:

outfile = paste("output/","coef_",sprintf("%02d",s_index),"_",sprintf("%02d",r_index),".txt", sep="")
file_write <- paste("~/STA250/Stuff/HW2/BLB/", outfile, sep="")

# Save estimates to file:

write.table(model$coefficients, file=file_write)
