# random sample drawing -------------------------------------------------------
Ksize <- 200 # how many RS you want to simulate
KOPar <- c(-1,0.5)
SigmoidFunc <- function(x){
  return(1/(1 + exp (-x)))
}

x <- runif(Ksize, min = 0.5, max = 1.5)

X <- cbind(1,x)
mu <- X%*%KOPar
p <- SigmoidFunc(mu)
y <- rbinom(n = Ksize,size = 1,prob = p)
(Sim.Data <- as.data.frame(cbind(y,x))) # will print simu data


# estimation of parameter using simulated data --------------------------------

ScoreFunc <- function(theta,y,X){#returns value of score func
  p <- as.vector(SigmoidFunc(X%*%theta))
  return(t(X) %*% (y-p))
}

HessianFunc <- function(theta,y,X){#returns value of Hessian matrix
  p <- as.vector(SigmoidFunc(X%*%theta))
  W <- diag(p*(1-p))
  return(-t(X)%*%W%*%X)
}

NROptimFunc <- function(y,X,ini_g,err_p){
    theta <- ini_g
    n_iter <- 1#iteration num
    diff_vec <- rep(1,length(ini_g))
    while(max(diff_vec)>err_p){
      n_iter <- n_iter+1
      theta_old <- theta
      ScoreVec <- ScoreFunc(theta,y,X)
      HessianMat <- HessianFunc(theta,y,X)
      # print(det(HessianMat))
      # if(abs(det(HessianMat)) < 0.0001) break # tackle singularity of Hessian 
      theta <- as.vector(theta + solve(-HessianMat)%*%ScoreVec) # Update theta
      diff_vec <- abs(theta_old - theta)
    }
    StanErrVec <- sqrt(diag(solve(-HessianMat)))
    return(data.frame(theta = theta,SE = StanErrVec))
}

NROptimFunc(y,X,ini_g = c(0,0),err_p = 0.0001)

