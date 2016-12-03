#
# f(x) = 2 <= 0<=y<x<1, 0 otherwise
#
pdf <- function(x,y) {
  2*(0<=y & y<x & x<1)
}

#
# U ~ F(X) => X ~ F-1(U)
# U ~ U[0,1]
# XY ~ Fx(X)Fx|y(y|X=x)
#
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

#
# Plot bivariate distribution
#
plot.bivar.distr <- function(XY, nvals) {
  par(cex.axis=0.8, pty="s")
  title <- sprintf('Distribution f(x,y)=2, 0≤y<x<1 (n=%d)', nvals)
  plot(XY, pch='.', xlim=c(0,1), ylim=c(0,1.5), col='gray45', main=title)
  abline(0, 1, col='red')
}

#
# Run simulation
#
sim.bivar.distr <- function(nvals) {
  XY <- gen.bivar.distr(nvals)
}
