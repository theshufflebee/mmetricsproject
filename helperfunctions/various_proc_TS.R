# ==============================================================================
# MacroEconometrics course
# Various procedures
# Jean-Paul Renne, 2024
# ==============================================================================

autocov <- function(X,n){
  T <- length(X)
  X.1 <- X[1:(T-n)] - mean(X)
  X.2 <- X[(n+1):T] - mean(X)
  return(1/T * sum(X.1 * X.2))
}

make.F <- function(phi){
  # Make F matrix for an AR process
  p <- length(phi)
  F <- matrix(0,p,p)
  F[1,] <- phi
  if(p>1){
    F[2:p,1:(p-1)] <- diag(p-1)
  }
  return(F)
}

make.dyn.mult <- function(phi,max.dyn.mult){
  # Compute dynamic mutlipliers for an AR process
  vec.dyn.mult <- NULL
  F <- make.F(phi)
  p <- length(phi)
  F.j <- diag(p)
  vec.j <- 0:max.dyn.mult
  for(j in vec.j){
    vec.dyn.mult <- c(vec.dyn.mult,F.j[1,1])
    F.j <- F.j %*% F
  }
  return(vec.dyn.mult)
}


sim.arma <- function(c,phi,theta,sigma,T,y.0,nb.sim=1,make.IRF=0,X=NaN,beta=NaN){
  # nb.sim samples of length T are simulated, all
  # of them begin with y_0 = y.0, y.0 is of dimension p x 1
  # sigma is a standard deviation
  # Warning:
  #     ->>>> for an AR process, set theta=1.
  #     ->>>> that is, to simulate an ARMA(p,q), theta has to be of length (q+1).
  # By default, only one simulation is done (nb.sim=1).
  #
  # If make.IRF=0,
  #     then it means that the user wants to compute IRFs with a maximum horizon T.
  # X is a matrix of exogenous variables; beta is a vector with appropriate dimensions.
  p <- length(phi)
  q <- length(theta)
  if(length(y.0)!=p){
    print("y.0 should have the same length as phi.")
    return(NULL)
  }
  
  if(!is.na(X[1])){
    X.beta <- X %*% matrix(beta,ncol=1)
  }else{
    X.beta <- rep(0,T)
  }
  
  if(make.IRF==0){# In that case, the user wants to perform standard simulations
    eps <- sigma*matrix(rnorm(nb.sim*T),T,nb.sim) # These are all the shocks that will be used
    #eps[1:q,] <- 0
    eps.t <- matrix(0,q,nb.sim) # eps.t is of dimension (q x nb.sim)
    # eps.t will change at each iteration, the i^th column corresponds to simulation i.
    # At date t, the i^th column of eps.t contains, for simulation i: (epsilon[t],epsilon[t-1],...,epsilon[t-q+1])
  }else{# In that case, the user wants to compute IRFs
    eps <- matrix(0,T,nb.sim)
    eps[1,] <- sigma # This is the initial impulsion
    eps.t <- matrix(0,q,nb.sim) # eps.t is of dimension (q x nb.sim)
  }
  F <- make.F(phi) # This is the usual F matrix
  Y <- NULL
  y.00 <- matrix(y.0,p,nb.sim)
  y.t <- y.00
  for(t in 1:T){
    if(q>1){
      eps.t <- rbind(eps[t,],matrix(eps.t[1:(q-1),],q-1,nb.sim))
    }else{
      eps.t <- matrix(eps[t,],nrow=1)
    }
    theta.eps.t <- matrix(theta,nrow=1) %*% eps.t
    theta.eps.t <- rbind(theta.eps.t,matrix(0,p-1,nb.sim))
    y.t <- c(c,rep(0,p-1)) + F %*% y.t + theta.eps.t + X.beta[t]
    Y <- rbind(Y,y.t[1,])
  }
  return(Y)
}


make.PHI <- function(Phi){
  p <- length(Phi)
  n <- dim(Phi[[1]])[1]
  PHI <- matrix(0,n*p,n*p)
  if(p>1){
    PHI[(n+1):(n*p),1:((p-1)*n)] <- diag((p-1)*n)
  }
  for(i in 1:p){
    PHI[1:n,((i-1)*n+1):(i*n)] <- Phi[[i]]
  }
  return(PHI)
}

simul.VAR <- function(c,Phi,B,nb.sim,y0.star,indic.IRF=0,u.shock=0){
  # This function simulates a VAR model, initial condition = y0.star
  # Phi is a list, each element of which is a Phi_i matrix. Hence it has p elements if we consider a VAR(p)
  p <- length(Phi)
  n <- dim(Phi[[1]])[1]
  # Check that right dimension for y0.star:
  if((indic.IRF==0)&(length(y0.star)!=(n*p))){
    print("The dimension of y0.star should be (np x 1) where p is the number of lags in the VAR and n is the dimension of y")
    return(0)
  }
  if((indic.IRF!=0)&(length(u.shock)!=n)){
    print("If you want to compute IRFs, u.shock has to be of length n, where n is the number of dependent variables")
  }
  PHI <- make.PHI(Phi)
  c.star <- c(c,rep(0*c,p-1))
  B.star <- matrix(0,n*p,n*p)
  B.star[1:n,1:n] <- B
  y <- y0.star
  Y <- NULL
  for(t in 1:nb.sim){
    if(indic.IRF==0){
      y <- c.star + PHI %*% y + B.star %*% rnorm(n*p)
    }else{
      if(t==1){
        y <- B.star %*% c(u.shock,rep(0*u.shock,p-1))
      }else{
        y <- PHI %*% y
      }
    }
    Y <- rbind(Y,c(y))
  }
  return(Y[,1:n])
}


# Function that computes the density of vector eps, where the elements
# of vector eps are Gaussian i.i.d. variables N(mu,sigma^2):
log.l.gaussian <- function(eps,mu,sigma2){
  vec.log.f <- - 1/2*log(2*pi*sigma2) - (eps - mu)^2/(2*sigma2)
  return(vec.log.f)
}


log.lik.armax <- function(THETA,Y,p,q,X=NaN){
  c <- THETA[1]
  phi <- THETA[2:(p+1)]
  if(q>0){
    theta <- c(1,THETA[(1+p+1):(1+p+q)])
  }else{
    theta <- 1
  }
  sigma <- THETA[1+p+q+1]
  if(!is.na(X[1])){
    if(is.null(dim(X))){
      r <- 0
    }else{
      r <- dim(X)[2] - 1
    }
    beta <- THETA[(1+p+q+1+1):(1+p+q+1+(r+1))]
  }else{
    beta <- NULL
  }
  res <- armax.log.L(Y,c,phi,theta,sigma,X,beta)
  return(-sum(res$vec.log.l))
}

armax.log.L <- function(Y,c,phi,theta,sigma,X=NaN,beta=NaN){
  T <- length(Y)
  p <- length(phi)
  q <- length(theta)-1
  if(is.nan(X[1])){
    X.beta <- rep(0,T)
  }else{
    X.beta <- X %*% matrix(beta,ncol=1)
    if(length(X.beta)!=T){
      print("X is not of the same size as Y")
      return(0)
    }
  }
  if(q>=1){
    vec.eps <- rep(0,q+1)
    all.eps <- rep(0,p)
    for(t in (p+1):T){
      vec.eps <- c(Y[t] - c - sum(phi*Y[(t-1):(t-p)]) - sum(theta[2:(q+1)]*vec.eps[1:q])  - X.beta[t],vec.eps[1:q])
      all.eps <- c(all.eps,vec.eps[1])
    }
  }else{# q == 0
    if(p==0){
      all.eps <- Y - c - X.beta
    }else{
      Y_1 <- Y[p:(T-1)]
      if(p>1){
        for(i in 2:p){
          Y_1 <- cbind(Y_1,Y[(p+1-i):(T-i)])
        }
      }
      all.eps <- c(
        rep(0,p),
        Y[(p+1):T] - c - Y_1 %*% matrix(phi,ncol=1) - X.beta[(p+1):T]
      )
    }
  }
  #   sigma.eps <- Y[2:T] - c - phi*Y[1:(T-1)]
  #   log.Lik <- log.l.gaussian(sigma.eps,0,sigma2)
  #   log.L <- sum(log.Lik) - 1/2*log(2*pi*sigma2) + 1/2*log(1-phi^2) -
  #     (y[1] - c/(1-phi))^2 / (2* (sigma2/(1 - phi^2)))
  vec.log.l <- log.l.gaussian(all.eps,0,sigma^2)
  return(list(
    all.eps = all.eps,
    vec.log.l = vec.log.l
  )) # we return -log-lik because the optim procedure of R minimises functions
}

estim.armax <- function(Y,p,q,X=NaN){
  # first step: estimate an AR(p)
  T <- length(Y)
  if(is.na(X[1])){
    beta.0 <- NULL
    
    if(p>=1){
      y <- Y[(p+1):T]
      Y_1 <- Y[p:(T-1)]
      if(p>1){
        for(i in 2:p){
          Y_1 <- cbind(Y_1,Y[(p+1-i):(T-i)])
        }
      }
      eq <- lm(y~Y_1)
      c.0 <- eq$coefficients[1]
      phi.0 <- eq$coefficients[2:(p+1)]
      theta.0 <- rep(0,q)
      sigma.0 <- sd(eq$residuals)
    }else{
      y <- Y
      c.0 <- mean(y)
      phi.0 <- 0
      theta.0 <- rep(0,q)
      sigma.0 <- sd(y)
    }
  }else{# exogenous variables
    if(is.null(dim(X))){
      r <- 0
    }else{
      r <- dim(X)[2] - 1
    }
    if(p>=1){
      y <- Y[(p+1):T]
      Y_1 <- Y[p:(T-1)]
      if(p>1){
        for(i in 2:p){
          Y_1 <- cbind(Y_1,Y[(p+1-i):(T-i)])
        }
      }
      M <- matrix(X,T,r+1)
      eq <- lm(y~Y_1+M[(p+1):T,])
      c.0 <- eq$coefficients[1]
      phi.0 <- eq$coefficients[2:(p+1)]
      theta.0 <- rep(0,q)
      sigma.0 <- sd(eq$residuals)
      beta.0 <- eq$coefficients[(1+p+1):(1+p+r+1)]
    }else{
      y <- Y
      eq <- lm(y~X)
      c.0 <- eq$coefficients[1]
      phi.0 <- NULL
      theta.0 <- rep(0,q)
      sigma.0 <- sd(eq$residuals)
      beta.0 <- eq$coefficients[(1+1):(1+r+1)]
    }
  }
  
  THETA.0 <- c(c.0,phi.0,theta.0,sigma.0,beta.0)
  
  MAXIT.NlMd <- 100*length(THETA.0)
  MAXIT.BFGS <- 10
  
  print("==================================================")
  print("  ESTIMATING")
  print("==================================================")
  
  nb.iter <- 4
  for(i in 1:nb.iter){
    res.optim <- optim(par=THETA.0,
                       fn=log.lik.armax,
                       Y=Y,
                       p=p,
                       q=q,
                       X=X,
                       gr = NULL,
                       method="Nelder-Mead",
                       #method="CG",
                       #method="BFGS",
                       control=list(trace=FALSE,maxit=MAXIT.NlMd),hessian=FALSE)
    THETA.0 <- res.optim$par
    
    if(nb.iter==i){
      hessian.TRUE <- TRUE
    }else{
      hessian.TRUE <- FALSE
    }
    res.optim <- optim(par=THETA.0,
                       fn=log.lik.armax,
                       Y=Y,
                       p=p,
                       q=q,
                       X=X,
                       gr = NULL,
                       #method="Nelder-Mead",
                       #method="CG",
                       method="BFGS",
                       control=list(trace=FALSE,maxit=MAXIT.BFGS),hessian=hessian.TRUE)
    THETA.0 <- res.optim$par
  }
  
  print("  END OF ESTIMATION")
  print("==================================================")
  print("")
  print("  RESULTS:")
  print("  -----------------------")
  
  THETA <- THETA.0
  I <- solve(res.optim$hessian)
  
  st.dev <- sqrt(diag(I))
  t.ratio <- THETA/st.dev
  
  res.matrix <- as.matrix(cbind(THETA,st.dev,t.ratio)) # to be printed
  
  vec.names <- c("c")
  
  if(p>0){
    for(i in 1:p){
      vec.names <- c(vec.names,paste("phi   t-",toString(i),sep=""))
    }
  }
  if(q>0){
    for(i in 1:q){
      vec.names <- c(vec.names,paste("theta t-",toString(i),sep=""))
    }
  }
  vec.names <- c(vec.names,"sigma")
  if(!is.na(X[1])){
    for(i in 0:r){
      vec.names <- c(vec.names,paste("beta  t-",toString(i),sep=""))
    }
  }
  
  rownames(res.matrix) <- vec.names
  
  print(res.matrix)
  
  print("==================================================")
  
  c <- THETA[1]
  phi <- THETA[2:(p+1)]
  if(q>0){
    theta <- c(1,THETA[(1+p+1):(1+p+q)])
  }else{
    theta <- 1
  }
  sigma <- THETA[1+p+q+1]
  if(!is.na(X[1])){
    if(is.null(dim(X))){
      r <- 0
    }else{
      r <- dim(X)[2] - 1
    }
    beta <- THETA[(1+p+q+1+1):(1+p+q+1+(r+1))]
  }else{
    beta <- NULL
  }
  
  return(list(
    c=c,phi=phi,theta=theta,sigma=sigma,beta=beta,
    max.logl = -res.optim$value,I=I,st.dev=st.dev,THETA=THETA
  ))
}




