library(rgl)

plot_pdf <- function(n) {
  c <- 1
  
  x <- runif(n)
  y <- NULL
  z <- rep(2, n)
  while(c<=n) {
    y[c]<-runif(1)
    if(y[c]<x[c]) { 
      c<-c+1
    }
  }
  
  plot3d(x, y, z, pch='.', 
         xlim=c(0,1), ylim=c(0,1), zlim=c(0,3),
         xlab='',ylab='',zlab='',
         col='red', box=FALSE, axes=FALSE)
  mtext3d('X',edge='x',at=1.15)
  mtext3d('Y',edge='y',at=1.15)
  mtext3d('Z',edge='z',at=3.25)
  axes3d(edges=c('x','y','z'), labels=T)
  grid3d(side=c('x', 'y', 'z'), col = "#F6F6F6")  
}

plot_df <- function(n) {
  c <- 1
  
  x <- runif(n)
  y <- NULL
  while(c<=n) {
    y[c]<-runif(1)
    if(y[c]<x[c]) { 
      c<-c+1 
    }
  }
  z <- 2*x*y
  
  plot3d(x, y, z, pch='.', 
         xlim=c(0,1), ylim=c(0,1), zlim=c(0,3),
         xlab='',ylab='',zlab='',
         col='red', box=FALSE, axes=FALSE)
  mtext3d('X',edge='x',at=1.15)
  mtext3d('Y',edge='y',at=1.15)
  mtext3d('Z',edge='z',at=3.25)
  axes3d(edges=c('x','y','z'), labels=T)
  grid3d(side=c('x', 'y', 'z'), col = "#F6F6F6")  
}

open3d(cex=0.7)
plot_df(25000)