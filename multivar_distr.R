source('util3D.R')

#
# Probability Density Function (p.d.f)
# f(x) = 2 if 0≤y<x<1, 0 otherwise
#
pdf <- function(x,y) {
  2*(0<=y & y<x & x<1)
}

#
# Cumulative Distribution Function (cdf)
# f(x) = 2 if 0≤y<x<1, 0 otherwise
#
cdf <- function(x,y) {
  2*y*(x-y)*(0<=y & y<x & x<1)
}


#
# U ~ F(X) => X ~ F-1(U)
# U ~ U[0,1]
# XY ~ Fx(X)Fx|y(y|X=x)
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
  plot(XY, pch='.', xlim=c(0,1), ylim=c(0,1), col='gray45', main=title, xlab='X',ylab='Y')
  abline(0, 1, col='red')
}

#
# Plot bivariate distribution 3D
#
plot3D.bivar.sample <- function(XY) {
  XY <- xy.coords(XY)
  um <- matrix(c(-0.8076876, 0.5884517,-0.03695673, 0,
                 -0.4632577,-0.5945783, 0.65716797, 0,
                 0.3647376, 0.5479065, 0.75283819, 0,
                 0.0000000, 0.0000000, 0.00000000, 1),
               nrow=4,ncol=4,byrow=TRUE)
  wr <- c(x=0, y=0, width=512, height=512)
  
  hist3D(XY$x, XY$y, nclass=22, probability = TRUE,
         xlim=c(0,1), ylim=c(0,1), zlim=c(0,3), col='#ffffff')
  triangles3d(x=c(0,1,1),y=c(0,0,1),z=c(2,2,2), col='#ff0000',
              alpha=0.5)
  par3d(userMatrix=um, windowRect=wr)
}


#
# Run simulation
#
sim.bivar.distr <- function(nvals) {
  XY <- gen.bivar.sample(nvals)
  
  # filename <- sprintf('multivar_distr_%d.png',nvals)
  # png(file=filename, width=dim.W, height=dim.H)
  plot.bivar.sample(XY)
  # dev.off()
  
  # filename <- sprintf('multivar_distr_3d_%d.png',nvals)
  plot3D.bivar.sample(XY)
  # snapshot3d(filename)
}
