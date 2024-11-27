## .................................................................................
## Purpose: Setup a NeVary Multiplier for Respective Surfaces
##
## Author: Nick Brazeau
##
## Date: 27 October, 2023
##
## Notes: Type 1 and Type 2 will have Ne Varying Sims
## .......................,..........................................................
library(tidyverse)

#............................................................
# read in and manip
#...........................................................
migmatdf <- readRDS("data/sim_data/sim_params/migmat_framework.RDS")
# replicate migmat type 1 and type 2
migmatdf <- dplyr::bind_rows(migmatdf, migmatdf[1:2,])
migmatdf$modnameNe <- NA
migmatdf$modnameNe[1:3] <- migmatdf$modname[1:3]
migmatdf$modnameNe[4] <- paste0(migmatdf$modname[4], "-NeVary")
migmatdf$modnameNe[5] <- paste0(migmatdf$modname[5], "-NeVary")
#......................
# set up Ne Vary column
#......................
migmatdf$NeVaryMult <- NA
# get number of inds
nInds <- nrow(migmatdf$migmat[[1]])
# vary by e^x
expmult <- exp(seq(0, 2, by = 0.5))
expmult <- rep(expmult, sqrt(nInds))
migmatdf$NeVaryMult[1:3] <- list(rep(1, nInds))
migmatdf$NeVaryMult[4:5] <- list(expmult)

#............................................................
# out
#...........................................................
saveRDS(migmatdf,
        "data/sim_data/sim_params/NeVary_migmat_framework.RDS")
