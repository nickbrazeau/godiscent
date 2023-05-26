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
  dplyr::select(c("start_params", "f_learn", "m_learn")) %>%
  dplyr::mutate(start_param_realization = 1:nrow(.))
#......................
# expand discdat out for reps
#......................
fulldiscdat <- lapply(1:nrow(discdat), function(x){
  dplyr::bind_cols(discdat[x,], search_grid)}) %>%
  dplyr::bind_rows(.)


#......................
# now batch by modname and rep
#......................
fulldiscdat <- fulldiscdat %>%
  tidyr::nest(data = c(discdat, start_param_realization, start_params, f_learn, m_learn))


#............................................................
# run discent
#...........................................................
# run out on Longleaf
plan(future.batchtools::batchtools_slurm, workers = nrow(fulldiscdat),
     template = "simdata/slurm_discent.tmpl")

fulldiscdat <- fulldiscdat %>%
  dplyr::mutate(discret = furrr::future_map(data,
                                            get_discentwrapper,
                                            .options = furrr_options(seed = TRUE)))

fulldiscdat <- fulldiscdat %>%
  tidyr::unnest(cols = c("data", "discret")) %>%
  dplyr::select(-c("discdat")) # replicated across modname - a lot of mem/data not needed in final product


# out
dir.create("simdata/disc_results/", recursive = T)
saveRDS(fulldiscdat, "simdata/disc_results/final_discresults_for_discdat.RDS")


