#
# f(x) = Sum[i=1..r]wifi(x)
# X1 ~ U[-2,2]
# X2 ~ Exp(2)
#
pdf <- function(x, w1, a, b, lambda) {
  w1 * dunif(x, min=a, max=b) + (1-w1) * dexp(x, rate=lambda)
}

#
# F(x) = Sum[i=1..r]wiPi(x)
# X1 ~ U[-2,2]
# X2 ~ Exp(2)
#
cdf <- function(x, w1, a, b, lambda) {
  w1 * punif(x, min=a, max=b) + (1-w1) * pexp(x, rate=lambda)
}


#
# U ~ F(X) => X ~ F-1(U)
# U ~ U[0,1]
# X ~ p1*f1(u) + p2*f2(u)
#
gen.mix.distr <- function(nvals, p1, a, b, lambda) {
  X <- NULL
  U <- runif(nvals)

  p2 <- 1-p1
  n  <- 0
  while(n < nvals) {
    pn <- runif(1)
    n <- n + 1
    if(pn < p2) {
      X[n] <- -log(U[n])/lambda
    } else {
      X[n] <- U[n]*(b-a)+a
    }
  }
  X
}

#
# Plot the distributions
#
plot.mix.distr <- function(XX, fx, gx, a, b, lambda, breaks=50) {
  
  x.min <- min(XX); y.min <- 0
  x.max <- max(XX); y.max <- 1 
  
  title <- sprintf("Composition method to add U[%d,%d] and Exp(%d)",
                   a, b, lambda)
  
  par(cex.axis=0.8, pty="s")
  hist(gx, probability=TRUE, breaks=breaks,
       xlim=c(x.min,x.max), ylim=c(y.min,y.max),
       xlab="x", ylab="f(x)", main=title)
  lines(XX, fx, col='red')
}

#
#
#
qqplot <- function(nvals, X, Y) {
  q <- seq(0.5/nvals, 1 , 1/nvals)
  
  qq.popX <- quantile(sort(X), prob=q)
  qq.popY <- quantile(sort(Y), prob=q)
  qq <- cbind(qq.popX, qq.popY)
  
  plot(qq,pch=".", xlim=c(-3,3), ylim=c(-3,3), 
       xlab="X", ylab="Y", main="QQ plot")
  abline(0,1,col="red")
}

#
# Kolmogorov-Smirnoff test
#
ks.mix.distr <- function(A,B,alpha=0.01) {
  q  <- seq(0, 1, 0.01)
  sA <- quantile(A, prob=q)
  sB <- quantile(B, prob=q)
  T <- rbind(cbind(X=sA,set=1,FnA=0,FnB=0,DFn=0),
             cbind(X=sB,set=2,FnA=0,FnB=0,DFn=0))
  
  T <- T[order(T[,'X']),]
  N <- length(q)
  for(n in seq(2*N)) {
    T[n,'FnA'] <- sum(T[1:n,'set']==1)/N
    T[n,'FnB'] <- sum(T[1:n,'set']==2)/N
    T[n,'DFn'] <- abs(T[n,'FnA']-T[n,'FnB'])
  }
  
  c<-switch(toString(alpha),
        "0.20" = 1.07,
        "0.10" = 1.22,
        "0.05" = 1.36,
        "0.02" = 1.52,
        1.63 # 0.01
      ) * sqrt((2*N)/(N^2))
  c(c=c,D=max(T[,'DFn']))
}

#
# Run simulation
#
sim.mix.distr <- function(nvals) {
  # X1 ~ U[-2,2]
  a <- -2; b <- 2; p1 <- 0.6
  
  # X2 ~ Exp(2)
  lambda <- 2; p2 <- 0.4
  
  xx <- seq(a-1, b+1, (b-a)/nvals)
  fx <- pdf(xx, p1, a, b, lambda)
  gx <- gen.mix.distr(nvals, p1, a, b, lambda)
  
  brk = 25 * floor((log(nvals,10) - 1))
  plot.mix.distr(xx, fx, gx, a, b, lambda, breaks=brk)
  
  fset <- ceiling((length(fx)-length(gx))/2)
  fx   <- fx[fset:(length(fx)-fset)]
  
  ks <- ks.mix.distr(fx,gx,alpha=0.05)
  cat(sprintf("RR=]%0.5f,+Inf[, D'=%0.5f\n", ks['c'], ks['sup']))
  
  qqplot(nvals, gx,fx)
}


