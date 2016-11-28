gen.mix.distr <- function(p,lambda,a,b,nvals) {
  X <- NULL
  U <- runif(nvals)

  n <- 0
  while(n < nvals) {
    pn <- runif(1)
    n <- n + 1
    if(pn < p) {
      X[n] <- -log(U[n])/lambda
    } else {
      X[n] <- U[n]*(b-a)+a
    }
  }
  X
}

test.mix.distr <- function(nvals, breaks=50) {
  # X1 ~ U[-2,2]
  a <- -2; b <- 2
  p1 <- 0.6
  
  # X2 ~ Exp(2)
  lmb <- 2
  p2 <- 0.4
  
  XX <- gen.mix.distr(p2,lmb,a,b,nvals)
  
  par(cex.axis=0.8, pty="s")
  
  x.min <- -3; x.max <- 3
  y.min <- 0;  y.max <- 1
  
  title <- sprintf("Composition method to add U[%d,%d] and Exp(%d)",a,b,lmb)
  hist(XX, probability=TRUE, breaks=breaks,
       xlim=c(x.min,x.max), ylim=c(y.min,y.max),
       xlab="x", ylab="f(x)",
       main=title)
  
  x <- seq(x.min,x.max,0.005)
  lines(x,p1*dunif(x,min=a,max=b) + p2*dexp(x,rate=lmb), col='red')
  
  #q.pop <- quantile(d,prob=seq(0.5/nvals,1,1/nvals))
  q.pop <-qunif(p1,min=a,max=b)+qexp(p2,rate=lmb)
  qq <- cbind(sort(XX), q.pop)
  plot(qq,pch=".", xlim=c(-3,3), ylim=c(-3,3), xlab="X", ylab="Y", main="QQ plot")
  abline(0,1,col="red")
}


