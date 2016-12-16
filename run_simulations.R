source('multivar_distr.R')
source('mixture_distr.R')

dim.W  <-700
dim.H  <-700

nvals  <- c(5000,25000,50000,250000)

for(i in seq_along(nvals)) {
  sim.mix.distr(nvals[i])
  #sim.bivar.distr(n)
}
