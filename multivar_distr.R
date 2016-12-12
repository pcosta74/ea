#
# Probability Density Function (p.d.f)
# f(x) = 2 <= 0<=y<x<1, 0 otherwise
#
pdf <- function(x,y) {
  2*(0<=y & y<x & x<1)
}

#
# U ~ F(X) => X ~ F-1(U)
# U ~ U[0,1]
# XY ~ Fx-1(U1)Fu1|u2(U2|U1=u1)
#
gen.bivar.sample <- function(nvals) {
  # Generate variables U1 ~ U[0,1], U2 ~ U[0,1]
  U1 <- runif(nvals)
  U2 <- runif(nvals)

  # Apply Inverse Transform Method
  X <- sqrt(U1)
  Y <- X*U2
  
  # Return Random Variables
  cbind(X=X,Y=Y)
}

#
# Plot bivariate distribution
#
plot.bivar.sample <- function(XY) {
  par(cex.axis=0.8, pty="s")
  title <- sprintf('Distribution f(x,y)=2, 0≤y<x<1 (n=%d)', nrow(XY))
  plot(XY, pch='.', xlim=c(0,1), ylim=c(0,1), col='gray45', main=title)
  abline(0, 1, col='red')
}

#
# Run simulation
#
sim.bivar.distr <- function(nvals) {
  XY <- gen.bivar.sample(nvals)
  plot.bivar.sample(XY)
}
