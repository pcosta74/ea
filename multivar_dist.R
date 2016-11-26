gen.conj.distr <- function(n) {
  # Generate variables U1 ~ U[0,1], U2 ~ U[0,1]
  U1 <- runif(n)
  U2 <- runif(n)

  # Apply Inverse Transform Method
  X <- sqrt(U1)
  Y <- sqrt(X*2*U2)
  
  # Return Random Variables
  XY <- cbind(X,Y)
  XY
}


N <- 10000
XY <- gen.conj.distr(N)

title=sprintf('Distribution f(x,y)=2, 0≤y<x<1 (n=%d)',N)
plot(XY, pch=".", xlim=c(0,1), ylim=c(0,2),main=title)
abline(0, 2, col="blue")