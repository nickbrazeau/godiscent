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
source("R/polysim_wrappers.R")
source("R/discent_wrappers.R")
source("R/utils.R")

#............................................................
# make search grid
#...........................................................
# look up tables
fstartsvec <- seq(from = 0.1, to = 0.9, by = 0.05)
mstartsvec <- 10^-seq(1,6)
flearnsvec <- 10^-seq(1,10)
mlearnsvec <- 10^-seq(5,15)
search_grid <- expand.grid(fstartsvec, mstartsvec, flearnsvec, mlearnsvec)
colnames(search_grid) <- c("fstart", "mstart", "f_learn", "m_learn")

# template start
tempstart_params <- rep(0.1, 25)
names(tempstart_params) <- as.character(1:25)
tempstart_params <- c(tempstart_params, "m" = 1e-3)

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
#......................
# read in results
#......................
sims <- readRDS("results/discdat_from_polySimIBD_maestro.RDS")
sims <- sims[1:4,] # assume one use case per sim for optimal start

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
search_grid_full$cost <- furrr::future_pmap(search_grid_full[,c("start_params", "f_learn", "m_learn", "discdat")],
                                            get_GS_cost,
                                  .options = furrr_options(seed = TRUE))

search_grid_full <- search_grid_full %>%
  dplyr::select(c("modname", "rep", "start_params", "f_learn", "m_learn", "cost"))


dir.create("results", recursive = T)
saveRDS(search_grid_full, "results/search_grid_full_for_discdat.RDS")

