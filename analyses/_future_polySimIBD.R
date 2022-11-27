## .................................................................................
## Purpose: Using future to frame out my poly sim simulations
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
library(progressr)
library(polySimIBD)
source("R/polysim_wrappers.R")
source("R/utils.R")

#............................................................
# read in and make polysim IBD dataframe
#...........................................................
reps <- 100
maestro <- readRDS("validation/mkdata/simulation_maestro.RDS")
maestro <- lapply(1:reps, function(x){
  maestro <- maestro %>%
    dplyr::mutate(rep = x) %>%
    dplyr::select(c("modname", "rep", dplyr::everything()))
  return(maestro)}
) %>%
  dplyr::bind_rows()
locatcomb <- readRDS("mkdata/simdata/locatcombo.rds")


#............................................................
# run simulations on slurm
#...........................................................
ret <- maestro %>% dplyr::select(c("modname", "rep"))
plan(future.batchtools::batchtools_slurm, workers = availableCores(),
     template = "analyses/slurm_discent.tmpl")

# add progress bar
with_progress({
  p <- progressor(steps = nrow(maestro))
  ret$discdat <- furrr::future_pmap(maestro[,c("pos", "N", "m", "rho", "mean_coi", "tlim", "migr_mat", "demeNames")],
                                    swfsim_2_discdat_wrapper,
                                    dwnsmplnum = 5,
                                    locatcomb = locatcomb,
                                    p = p,
                                    .options = furrr_options(seed = TRUE))

})

dir.create("results")
saveRDS(ret, "results/discdat_from_polySimIBD_maestro.RDS")
