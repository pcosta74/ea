library(rgl)

#
# Plot functions in 3D
#
plot3D <- function(x, y, FUN, zero.exclude=FALSE) {
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


#
# Plot functions in 3D
#
hist3D <- function(x, y, nclass=10, plot=TRUE, probability=FALSE,
                   xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),
                   xlab='X', ylab='Y', zlab='Z',
                   alpha=1, col='#cccccc') {


  if(plot) { open3d(cex=0.7) }
  
  z <- .hist3d(x, y, nclass=nclass, plot=plot, probability=probability,
               alpha=alpha, topcol=col)
  
  if(plot) {
    mtext3d(xlab, edge='x', at=max(xlim)+.1)
    mtext3d(ylab, edge='y', at=max(ylim)+.1)
    mtext3d(zlab, edge='z', at=max(zlim)+.1)
    
    decorate3d(box=FALSE, axes=FALSE,
               xlim=xlim, ylim=ylim, zlim=zlim,
               xlab='', ylab='', zlab='')
    axes3d(edges=c('x','y','z'), labels=T)
    grid3d(side=c('x','y','z'), col = "#F3F3F3")
    aspect3d(1,1,0.5)
  }
  
  return(z)
}


################################################################################
# Addapted from rgl's hist3D demo
#
# Package ‘rgl’
# August 25, 2016
# Version 0.96.0
# Title 3D Visualization Using OpenGL
# Author Daniel Adler <dadler@unigoettingen.de>, Duncan Murdoch <murdoch@stats.uwo.ca>, and others (see README)
# Maintainer Duncan Murdoch <murdoch@stats.uwo.ca>
#   Depends R (>= 3.2.0)
# License GPL
# doc: https://cran.r-project.org/web/packages/rgl/rgl.pdf
# src: https://github.com/trestletech/rgl/blob/master/demo/hist3d.r
################################################################################

#
# Plot bin
#
.binplot3d<-function(x,y,z,alpha=1,topcol="#ff0000",sidecol="#cccccc")
{
  save <- par3d(skipRedraw=TRUE)
  on.exit(par3d(save))
  
  x1<-c(rep(c(x[1],x[2],x[2],x[1]),3),rep(x[1],4),rep(x[2],4))
  z1<-c(rep(0,4),rep(c(0,0,z,z),4))
  y1<-c(y[1],y[1],y[2],y[2],rep(y[1],4),rep(y[2],4),rep(c(y[1],y[2],y[2],y[1]),2))
  x2<-c(rep(c(x[1],x[1],x[2],x[2]),2),rep(c(x[1],x[2],rep(x[1],3),rep(x[2],3)),2))
  z2<-c(rep(c(0,z),4),rep(0,8),rep(z,8) )
  y2<-c(rep(y[1],4),rep(y[2],4),rep(c(rep(y[1],3),rep(y[2],3),y[1],y[2]),2) )
  rgl.quads(x=x1,y=y1,z=z1,col=rep(sidecol,each=4),alpha=alpha)
  rgl.quads(x=c(x[1],x[2],x[2],x[1]),y=c(y[1],y[1],y[2],y[2]),z=rep(z,4),
            col=rep(topcol,each=4),alpha=1) 
  rgl.lines(x2,y2,z2,col="#000000")
}

#
# Histogram 3D
#
.hist3d<-function(x,y=NULL,nclass="auto", plot=TRUE, probability=FALSE,
                  alpha=1, topcol="#ff0000",sidecol="#cccccc")
{
  if (nclass == "auto") { nclass<-ceiling(sqrt(nclass.Sturges(x))) }
  
  xy <- xy.coords(x,y)
  x <- xy$x
  y <- xy$y
  n <- ifelse(probability,(nclass^2)/length(x),1)

  breaks.x <- seq(min(x),max(x),length=(nclass+1))
  breaks.y <- seq(min(y),max(y),length=(nclass+1))
  
  z <- matrix(0,(nclass),(nclass))
  for (i in 1:nclass) {
    for (j in 1:nclass) {
      z[i,j] <- n*sum(breaks.x[i] <= x & x < breaks.x[i+1] &
                      breaks.y[j] <= y & y < breaks.y[j+1])
    }
  }  
  
  if(plot) {
    save <- par3d(skipRedraw=TRUE)
    on.exit(par3d(save))

    for (i in 1:nclass) {
      for (j in 1:nclass) {
        .binplot3d(c(breaks.x[i],breaks.x[i+1]),c(breaks.y[j],breaks.y[j+1]),
                   z[i,j],alpha=alpha,topcol=topcol,sidecol=sidecol)
      }
    }
    
    z <- NULL
  }
  
  return(z)
}

################################################################################

