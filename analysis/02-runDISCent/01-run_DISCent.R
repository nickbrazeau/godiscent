## .................................................................................
## Purpose: Tidy model architecture for running DISCent
##
## Author: Nick Brazeau
##
## Date: 10 December, 2024
##
## Notes: tidy models (meaning tibble architecture and tidyverse) not ML Tidy Models
## .................................................................................

# import libraries
library(tidyverse)
remotes::install_github("nickbrazeau/discent")
library(discent)
set.seed(48)

#++++++++++++++++++++++++++++++++++++++++++
### Part 0: Import Data and Setup     ####
#++++++++++++++++++++++++++++++++++++++++++
# sim data from 01-polySimIBD_data/05-run_sims.R
simdat <- readRDS("data/sim_data/goDISC_simulated_gendata.RDS") %>%
  dplyr::mutate(modname = paste(modnameNe, "rep", rep, sep = "-")) %>%
  dplyr::select(c("modname", "IBDcalc"))
# location data from 01-polySimIBD_data/02-cartesian_empiric_liftover.R
locatdat <- readRDS("data/sim_data/sim_params/locatcombo.rds")

DISChelperfunction <- function(simdata, locationdata) {
  # read in simdata, unnest for join, then join location data and re-nest
 simdata %>%
    tidyr::unnest(., IBDcalc) %>%
    dplyr::filter(deme1 != deme2) %>%
    dplyr::left_join(x = ., y = locationdata, by = c("deme1", "deme2")) %>%
    dplyr::select(c("modname", "smpl1", "smpl2", "deme1", "deme2", "gendist", "geodist")) %>%
    dplyr::group_by(modname) %>%
    tidyr::nest() %>%
    dplyr::ungroup()
}
#......................
# run helper function for DISCent input
#......................
DISCdat <- DISChelperfunction(simdata = simdat, locationdata = locatdat)

#++++++++++++++++++++++++++++++++++++++++++
### Part 1: Setup DISCent start params        ####
#++++++++++++++++++++++++++++++++++++++++++
# full spectrum of start parameters for DISCent include below. However, we will
# focus on F, M, lambda, and learning rate as the most critical parameters that
# would affect convergence
# f - FOCUS
# m - FOCUS
# lambda - FOCUS
# learningrate - FOCUS
# m_lowerbound: 1e-3
# m_upperbound: 1e3
# b1: 0.9
# b2: 0.999
# e: 1e-8
# steps: 5e4
#......................
# magic numbers
#......................
fstart <- c(1e-4, 1e-3, 0.01, 0.025, 0.05, 0.1)
mstart <- c(0.5, 5, 10, 50, 100)
learningrate <- 10^seq(-4, -1, by = 1)
lambda <- 10^seq(-6, -3, by = 1)
dynamic_start_params <- tidyr::expand_grid(fstart, mstart, learningrate, lambda)

#......................
# combine data and dynamic start params
#......................
DISCdat <- tidyr::expand_grid(DISCdat, dynamic_start_params)

#......................
# wrapper function for modname and static start params
#......................
# m_lowerbound: 1e-3
# m_upperbound: 1e3
# b1: 0.9
# b2: 0.999
# e: 1e-8
# steps: steps 1e4 for search; steps 1e5 for full

discwrapper <- function(modname, data, fstart, mstart, learningrate, lambda, cost, steps){
  # name start params
  setstartparam <- rep(fstart, 25)
  names(setstartparam) <- as.character(1:25)
  setstartparam <- c(setstartparam, "m" = mstart)

  # out
   discent::disc(
    discdat = data,
    start_params = setstartparam,
    lambda = lambda,
    learningrate = learningrate,
    b1 = 0.9,
    b2 = 0.999,
    e = 1e-8,
    steps = steps,
    thin = 1e2,
    normalize_geodist = TRUE,
    report_progress = F,
    return_verbose = F
  )
}


#++++++++++++++++++++++++++++++++++++++++++
### Part 2: Run DISCent from start params        ####
#++++++++++++++++++++++++++++++++++++++++++
DISCdat <- DISCdat %>%
  dplyr::mutate(mod = purrr::pmap(., .f  = discwrapper, .progress = T, steps = 1e4))


#++++++++++++++++++++++++++++++++++++++++++
### Part 3: Identify Minimum DISCent start params ####
#++++++++++++++++++++++++++++++++++++++++++
DISCdatmin <- DISCdat %>%
  dplyr::group_by(modname) %>%
  dplyr::summarise( cost = min(cost)  )
# bring in start parameters
DISCdat_full <- dplyr::left_join(DISCdatmin, DISCdat, by = c("modname", "cost"))
DISCdat_full <- DISCdat_full %>%
  dplyr::mutate(mod = purrr::pmap(., .f  = discwrapper, .progress = T, steps = 1e5))

#......................
# save out
#......................
saveRDS(DISCdat, file = "data/sim_data/goDISC_search-DISC_simulated_gendata.RDS")
saveRDS(DISCdat_full, file = "data/sim_data/goDISC_full-DISC_simulated_gendata.RDS")
