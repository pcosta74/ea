#
# f(x) = Sum[i=1..r]wifi(x)
# X1 ~ U[-2,2]
# X2 ~ Exp(2)
#
pdf <- function(x, w1, a, b, lambda) {
  w1 * dunif(x, min=a, max=b) + (1-w1) * dexp(x, rate=lambda)
}

#
# F(x) = Sum[i=1..r]wiPi(x)
# X1 ~ U[-2,2]
# X2 ~ Exp(2)
#
cdf <- function(x, w1, a, b, lambda) {
  w1 * punif(x, min=a, max=b) + (1-w1) * pexp(x, rate=lambda)
}

#
# U ~ F(X) => X ~ F-1(U)
# U ~ U[0,1]
# X ~ p1*F1^-1(U) + p2*F2^-1(U)
#
gen.mix.sample <- function(nvals, p1, a, b, lambda) {
  # Generate variable U ~ U[0,1]
  U <- runif(nvals)
  X <- NULL
  
  n  <- 0
  p2 <- 1-p1
  while(n < nvals) {
    pn <- runif(1)
    n <- n + 1
    # Choose j in {1, ..., r} according to p1, ..., pr
    if(pn < p2) {
      # Generate Xi with pdf fi(x)
      X[n] <- -log(U[n])/lambda 
    } else {
      # Generate Xi with pdf fi(x)
      X[n] <- U[n]*(b-a)+a
    }
  }
  X
}

#
# Fu^-1(U)
#
# gen.mix.distr <- function(X,p1,a,b,lambda) {
#   FX <- rep(0,length(X))
#   
#   for(i in seq_along(X)) {
#     x <- X[i]
#     if(x>b) { # x > b 
#       FX[i] <- 1 - (1-p1)*exp(-2*x)
#     } 
#     else if(x>0) { # 0≤x<b
#       FX[i] <- 0.7+ (p1*x)/(b-a) - (1-p1)*exp(-lambda*x) 
#     } 
#     else if(x>a) { # a<x≤0
#       FX[i] <- 0.3 + (p1*x)/(b-a)
#     } 
#     else { # x<a
#       #  FX[i] <- 0
#     }
#   }
#   FX
# }

#
# Plot the distributions
#
plot.mix.sample <- function(X, x.axis, fx, a, b, lambda, breaks=50) {
  
  x.min <- min(x.axis); y.min <- 0
  x.max <- max(x.axis); y.max <- 1 
  
  title <- sprintf("Composition method to add U[%d,%d] and Exp(%d)",
                   a, b, lambda)
  
  par(cex.axis=0.8, pty="s")
  hist(X, probability=TRUE, breaks=breaks,
       xlim=c(x.min,x.max), ylim=c(y.min,y.max),
       xlab="x", ylab="f(x)", main=title)
  lines(x.axis, fx, col='red')
}

#
# Quantile-quantile plot
#
qqplot <- function(nvals, X, Y) {
  q <- seq(0.5/nvals, 1 , 1/nvals)
  
  qq.popX <- quantile(sort(X), prob=q)
  qq.popY <- quantile(sort(Y), prob=q)
  qq <- cbind(qq.popX, qq.popY)
  
  plot(qq,pch=".", xlim=c(0,1), ylim=c(0,1), 
       xlab="X", ylab="Y", main="QQ plot")
  abline(0,1,col="red")
}



#
# Run simulation
#
sim.mix.distr <- function(nvals) {
  # X1 ~ U[-2,2]
  a <- -2; b <- 2; p1 <- 0.6
  
  # X2 ~ Exp(2)
  lambda <- 2; p2 <- 0.4
  
  xx <- seq(a-1, b+1, (b-a)/nvals)
  fx <- pdf(xx, p1, a, b, lambda)
  X  <- gen.mix.sample(nvals, p1, a, b, lambda)
  
  brk = 25 * floor((log(nvals,10) - 1))
  plot.mix.sample(X, xx, fx, a, b, lambda, breaks=brk)
  
  # FX <- cdf(xx,p1,a,b,lambda)
  # Y  <- gen.mix.distr(xx,p1,a,b,lambda)
  # ks.test(FX,Y,alternative='two.sided')
  # qqplot(nvals, FX, Y)
}


