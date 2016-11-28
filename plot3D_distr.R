library(rgl)

pdf <- function(x,y,z0=NA) {
  z <- rep(z0,length(x))
  z[0<=y & y<=x & x<1] <- 2
  z
}

cdf <- function(x,y,z0=NA) {
  i <- c(0<=y & y<=x & x<1) 
  z <- rep(z0,length(x))
  z[i] <- 2*x[i]*y[i]
  z
}

plot3D_fun <- function(x,y,FUN=pdf,z0=NA) {
  z <- FUN(x,y,z0)
  
  x.min <- floor(min(x, na.rm = TRUE))
  x.max <- ceiling(max(x, na.rm = TRUE))
  y.min <- floor(min(y, na.rm = TRUE))
  y.max <- ceiling(max(y, na.rm = TRUE))
  z.min <- floor(min(z, 0, na.rm = TRUE))
  z.max <- ceiling(max(z, 2, na.rm = TRUE)) + 1
  
  open3d(cex=0.7)
  plot3d(x, y, z, pch='.', 
         xlim=c(x.min,x.max), ylim=c(y.min,y.max), zlim=c(z.min,z.max),
         xlab='',ylab='',zlab='',
         col='red', box=FALSE, axes=FALSE)
  mtext3d('X',edge='x',at=x.max+0.25)
  mtext3d('Y',edge='y',at=y.max+0.25)
  mtext3d('Z',edge='z',at=z.max+0.25)
  axes3d(edges=c('x','y','z'), labels=T, pos=c(0,0,0))
  grid3d(side=c('x', 'y', 'z-'), col = "#F3F3F3")  
}

N <- 25000
X <- runif(N)
Y <- runif(N)
plot3D_fun(X,Y,FUN=pdf)
plot3D_fun(X,Y,FUN=cdf)

#X <- runif(N,-1,2)
#Y <- runif(N,-1,2)
#plot_pdf(X,Y)