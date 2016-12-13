library(rgl)

plot3D.fun <- function(x, y, FUN, zero.exclude=FALSE) {
  z <- FUN(x,y)
  
  x.min <- floor(min(x, na.rm = TRUE))
  x.max <- ceiling(max(x, na.rm = TRUE))
  y.min <- floor(min(y, na.rm = TRUE))
  y.max <- ceiling(max(y, na.rm = TRUE))
  z.min <- floor(min(z, na.rm = TRUE))
  z.max <- ceiling(max(z, na.rm = TRUE)) + 1
  
  if(zero.exclude) {
    z[z==0] <- NA
  }
  
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

N <- 50000
X <- runif(N, min = -1,max = 2)
Y <- runif(N, min = -1,max = 2)
plot3D.fun(X,Y,FUN=pdf)
plot3D_fun(X,Y,FUN=cdf)
