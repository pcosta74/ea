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
# Random generated sample
#
rgs <- function(n, w1, a, b, lambda) {
  w1 * runif(n, min=a, max=b) + (1-w1) * rexp(n, rate=lambda)
}

#
# U ~ F(X) => X ~ F-1(U)
# U ~ U[0,1]
# X ~ p1*f1(u) + p2*f2(u)
#
gen.mix.sample <- function(nvals, p1, a, b, lambda) {
  X <- NULL
  U <- runif(nvals)

  p2 <- 1-p1
  n  <- 0
  while(n < nvals) {
    pn <- runif(1)
    n <- n + 1    
    if(pn < p2) {
      X[n] <- -log(1-U[n])/lambda
    } else {
      X[n] <- U[n]*(b-a)+a
    }
  }
  X
}

#
# Plot the distributions
#
plot.mix.sample <- function(XX, fx, gx, a, b, lambda, breaks=50) {
  
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
# QQ plot
#
qq.plot <- function(X, Y) {
  nvals <- length(X)
  q <- seq(0.5/nvals, 1 , 1/nvals)
  
  qq.popX <- quantile(sort(X), prob=q)
  qq.popY <- quantile(sort(Y), prob=q)
  qq <- cbind(qq.popX, qq.popY)
  
  x.min <- floor(min(qq.popX)); x.max <- ceiling(max(qq.popX))
  y.min <- floor(min(qq.popY)); y.max <- ceiling(max(qq.popY))
  
  plot(qq,pch=".", xlim=c(x.min,x.max), ylim=c(y.min,y.max), 
       xlab="X", ylab="Y", main="QQ plot")
  abline(0,1,col="red")
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
  gx <- gen.mix.sample(nvals, p1, a, b, lambda)
  rx <- 
  
  brk = 25 * floor((log(nvals,10) - 1))
  plot.mix.sample(xx, fx, gx, a, b, lambda, breaks=brk)
  
  #ks.test(Fx,Gx)
  qqplot(nvals, rx, fx)
  
  gx
}


