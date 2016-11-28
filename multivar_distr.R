gen.bivar.distr <- function(nvals) {
  # Generate variables U1 ~ U[0,1], U2 ~ U[0,1]
  U1 <- runif(nvals)
  U2 <- runif(nvals)

  # Apply Inverse Transform Method
  X <- sqrt(U1)
  Y <- sqrt(2*X*U2)
  
  # Return Random Variables
  XY <- cbind(X,Y)
  XY
}

test.bivar.distr <- function(nvals) {
  XY <- gen.bivar.distr(nvals)

  par(cex.axis=0.8, pty="s")
  title <- sprintf('Distribution f(x,y)=2, 0≤y<x<1 (n=%d)', nvals)
  plot(XY, pch='.', xlim=c(0,1), ylim=c(0,1.5), col='gray45', main=title)
  abline(0, 1, col='red')
  
  #dev.off()
}  
