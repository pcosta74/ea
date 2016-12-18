source('multivar_distr.R')
source('mixture_distr.R')


nvals  <- c(5000,25000,50000,250000)
for(i in seq_along(nvals)) {
  sim.bivar.distr(nvals[i])
}

for(i in seq_along(nvals)) {
   sim.mix.distr(nvals[i])
}

