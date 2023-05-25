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

#............................................................
# read in discdat from polySimIBD to run DISCent on
#...........................................................
discdat <- readRDS("simdata/sim_results/discdat_from_polySimIBD_maestro.RDS")
discdat <- discdat %>%
  dplyr::mutate(simrealization = 1:dplyr::n())

#............................................................
# make search grid of start params
#...........................................................
# look up tables
fstartsvec <- c(0.1, 0.3, 0.5)
mstartsvec <- c(10, 25, 50, 100, 250) # standardizing geodistance
flearnsvec <- mlearnsvec <-  c(1e-2, 1e-3, 1e-4)
search_grid <- expand.grid(fstartsvec, mstartsvec, flearnsvec, mlearnsvec)
colnames(search_grid) <- c("fstart", "mstart", "f_learn", "m_learn")
#......................
# now nest start params
#......................
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
#......................
# expand out for reps
#......................
simrealization <- 1:nrow(discdat)
search_grid_full <- lapply(simrealization, function(x){
  search_grid <- search_grid %>%
    dplyr::mutate(simrealization = x) %>%
    dplyr::relocate(., simrealization)
}) %>%
  dplyr::bind_rows()

#......................
# bring home
#......................
fulldiscdat <- dplyr::left_join(discdat, search_grid_full, by = "simrealization")

#TODO remove after test PRN
check <- sample(1:nrow(fulldiscdat), size = 500)
fulldiscdat <- fulldiscdat[check, ]

#............................................................
# run discent
#...........................................................
ret <- fulldiscdat %>%
  dplyr::select(c("modname", "rep", "simrealization"))
# run out on Longleaf
plan(future.batchtools::batchtools_slurm, workers = nrow(fulldiscdat),
     template = "simdata/slurm_discent.tmpl")
ret$discret <- furrr::future_pmap(fulldiscdat[,c("discdat", "start_params", "f_learn", "m_learn")],
                                  get_discentwrapper,
                                  .options = furrr_options(seed = TRUE))

# out
dir.create("simdata/disc_results/", recursive = T)
saveRDS(ret, "simdata/disc_results/final_discresults_for_discdat.RDS")


