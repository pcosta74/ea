source('multivar_distr.R')
source('mixture_distr.R')

nvals  <- c(5000,25000,50000,250000)
breaks <- c(50,100,100,100)

dim.W  <-700
dim.H  <-700

# for(i in seq_along(nvals)) {
#   filename <- sprintf('mixture_distr_%d.png',nvals[i])
#   png(file=filename, width=dim.W, height=dim.H)
#   
#   test.mix.distr(nvals[i],breaks[i])
#   
#   dev.off()
# }

for(n in nvals) {
  filename <- sprintf('multivar_distr_%d.png',n)
  png(file=filename, width=dim.W, height=dim.H)
  
  sim.bivar.distr(n)
  
  dev.off()
}
