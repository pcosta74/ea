library(rgl)

pdf <- function(x,y,set.null=FALSE) {
  z0 <- if(set.null) NA else 0
  z <- rep(z0,length(x))
  z[0<=y & y<=x & x<1] <- 2
  z
}

plot_pdf <- function(x,y,set.null=FALSE) {
  z <- pdf(x,y,set.null)
  
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
plot_pdf(X,Y,set.null=TRUE)

#X <- runif(N,-1,2)
#Y <- runif(N,-1,2)
#plot_pdf(X,Y)