## .................................................................................
## Purpose: Using future to find optimal start params
##
## Author: Nick Brazeau
##
## Date: 13 November, 2022
## Notes: unfortunately had to move to cluster for time
##
## .................................................................................
library(tidyverse)
library(furrr)
library(future)
library(future.batchtools)
library(discent)
source("R/discent_wrappers.R")
source("R/utils.R")
set.seed(48)

#............................................................
# read in prior polySimIBD results
#...........................................................
sims <- readRDS("results/discdat_from_polySimIBD_maestro.RDS")
# assume one use case per sim for optimal start
IBDsmpl <- sample(which(sims$modname == "IsoByDist"), size = 1)
latsmpl <- sample(which(sims$modname == "lattice"), size = 1)
torsmpl <- sample(which(sims$modname == "torus"), size = 1)
Nesmpl <- sample(which(sims$modname == "NeVary"), size = 1)
badIBDsmpl <- sample(which(sims$modname == "badIsoByDist"), size = 1)
# downsample
sims <- sims[c(IBDsmpl, latsmpl, torsmpl, Nesmpl, badIBDsmpl), ]


#............................................................
# make search grid
#...........................................................
# look up tables
fstartsvec <- seq(from = 0.05, to = 0.5, by = 0.05)
mstartsvec <- c(0.1, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5) # standardizing geodistance
flearnsvec <- 10^-seq(2,7)
mlearnsvec <- 10^-seq(1,10)
search_grid <- expand.grid(fstartsvec, mstartsvec, flearnsvec, mlearnsvec)
colnames(search_grid) <- c("fstart", "mstart", "f_learn", "m_learn")

# template start
tempstart_params <- rep(0.1, 25)
names(tempstart_params) <- as.character(1:25)
tempstart_params <- c(tempstart_params, "m" = 1)

# liftover to start param format
liftover_start_params <- function(fstart, mstart, start_param_template) {
  out <- start_param_template
  out[names(out) != "m"] <- fstart
  out[names(out) == "m"] <- mstart
  return(out)
}
search_grid <- search_grid %>%
  dplyr::mutate(start_params = purrr::map2(fstart, mstart, liftover_start_params,
                                           start_param_template = tempstart_params)) %>%
  dplyr::select(c("start_params", "f_learn", "m_learn"))

#............................................................
# Perform search
#...........................................................

# expand out grid
search_grid_full <- lapply(1:nrow(sims), function(x){
  return(dplyr::bind_cols(sims[x,], search_grid))}
) %>%
  dplyr::bind_rows()

#......................
# run discent wrapper
#......................
plan(future.batchtools::batchtools_slurm, workers = availableCores(),
     template = "analyses/slurm_discent.tmpl")
search_grid_full$discret <- furrr::future_pmap(search_grid_full[,c("start_params", "f_learn", "m_learn", "discdat")],
                                               get_GS_cost,
                                               .options = furrr_options(seed = TRUE))

# pull out final cost
search_grid_full <- search_grid_full %>%
  dplyr::mutate(finalcost = purrr::map_dbl(discret, extract_final_cost),
                final_fs = purrr::map(discret, get_fs),
                final_ms = purrr::map(discret, "Final_ms"))



search_grid_full <- search_grid_full %>%
  dplyr::select(c("modname", "rep", "start_params", "f_learn", "m_learn", "discret", "finalcost"))


dir.create("results", recursive = T)
saveRDS(search_grid_full, "results/search_grid_full_for_discdat.RDS")

