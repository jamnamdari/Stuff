
library("MASS")

MCMC.0 <- 
  function(TargetDist, PropDist, niter=10000, burnin=1000, start, sigma.0, ...){
    p <- length(start)
    Old <- start
    Sample <- matrix(0, nrow=niter+burnin, ncol=p)
    accept <- 0
    for (i in 1: (niter+burnin) ) { #browser()
      if(missing(PropDist)) {
        Proposal <- mvrnorm(n=1, mu=Old, Sigma=sigma.0) 
      }else Proposal <- PropDist(theta=OLD)
      u <- runif(1)
      Crit <- TargetDist(Proposal) - TargetDist(Old)
      if (log(u) < Crit) {
        Sample[i,] <- Proposal 
        accept <- accept + 1
      } else Sample[i,] <- Old
      #temp[i,] <- Old    
      Old <- Sample[i,]
    }
    return(list(sample = Sample[-(1:burnin),], rate = accept/(niter+burnin)))
  }


MCMC <- 
  function(TargetDist, PropDist, niter=10000, burnin=1000, start=0, sigma.0=1, retune=100){
    p <- length(start)
    sigma <- sigma.0
    for(i in 1:floor(niter/burnin)){#browser()
      test <- MCMC.0(TargetDist, niter=retune, burnin=0, start=start, sigma.0=sigma)
      if(test$rate<.3 | test$rate>.6) {sigma <- var(test$sample); start <- colMeans(test$sample) }else break
    }
    return(MCMC.0(TargetDist, PropDist, niter=niter, burnin=burnin, start, sigma.0=sigma))
  }

LogPiFun <- function(beta, y, X, m, beta.0, Sigma.0.inv){
  y%*%X%*%beta - m%*%log(1+exp(X%*%beta)) - 1/2*t(beta-beta.0)%*%Sigma.0.inv%*%(beta-beta.0)
}



bayes.logreg <- 
  function( m, y, X, beta.0, Sigma.0.inv, niter=10000, burnin=1000, print.every=1000,retune=100, verbose=TRUE)
  {browser()
    
    Spar <- summary(glm(cbind(y, m)~ X, family=binomial))$coefficients
    Post<- MCMC(TargetDist=function(beta) LogPiFun(beta, m=m, y=y, X=X, beta.0=beta.0, Sigma.0.inv=Sigma.0.inv),start=Spar[,1], sigma.0=diag(Spar[,2],))
    return(Post)
  }


BLR_result <- function(post, file_write){browser()
  write.csv(cbind(quantile(post[,1], probs=seq(.01, .99, by=.01)), quantile(post[,2], probs=seq(.01, .99, by=.01))), file=file_write, row.names=FALSE, col.names=FALSE)
}

