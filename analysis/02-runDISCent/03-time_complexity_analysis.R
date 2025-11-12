## .................................................................................
## Purpose: Time Complexity Analysis
##
## Author: Nick Brazeau
##
##
## Notes:
## .................................................................................

library(discent)
library(tidyverse)
library(polySimIBD)
library(viridisLite); library(viridis)


#++++++++++++++++++++++++++++++++++++++++++
### Part 0: Helper Functions & Data ####
#++++++++++++++++++++++++++++++++++++++++++
sim_swf_2_discdata_wrapper <- function(pos, ind_n, demesize) {

  # simulation
  swfsim <-
    polySimIBD::sim_swf(
    pos = pos,
    m = rep(0.5, demesize),
    rho = 7.4e-7,
    mean_coi = rep(1.5, demesize),
    tlim = 10,
    N = rep(ind_n, demesize),
    migr_mat = matrix(1, nrow = demesize, ncol = demesize) # doesn't need to be realistic migration
  )
  # get combinations
  hosts <- 1:(ind_n*demesize)
  comb_hosts_df <- t(combn(hosts, 2)) %>%
    data.frame(.) %>%
    magrittr::set_colnames(c("smpl1", "smpl2"))
  # get pairwise IBD
  comb_hosts_df <- comb_hosts_df %>%
    dplyr::mutate(gendist = purrr::map2_dbl(smpl1, smpl2, function(x, y, swf) {
      out <- polySimIBD::get_bvibd(swf = swf, host_index = c(x, y))
      return(out)
    }, swf = swfsim))

  # apply demes
  dms <- as.numeric( cut(hosts, breaks = c(0,cumsum(rep(ind_n, demesize)))) ) #intelligent coercion of factor to numeric to represent demes
  demeliftoverx <- tibble::tibble(smpl1 = hosts,
                                  deme1 = dms)
  demeliftovery <- tibble::tibble(smpl2 = hosts,
                                  deme2 = dms)
  # output
  comb_hosts_df <- comb_hosts_df %>%
    dplyr::left_join(., demeliftoverx, by = "smpl1") %>%
    dplyr::left_join(., demeliftovery, by = "smpl2")

  # make up location data
  locatdat <- comb_hosts_df %>%
    dplyr::select(c("deme1", "deme2")) %>%
    dplyr::filter(!duplicated(.)) %>%
    dplyr::mutate(geodist = abs(rnorm(nrow(.))))

  #......................
  # bring together for DISCent output
  #......................
  dplyr::left_join(comb_hosts_df, locatdat) %>%
    dplyr::select(c("smpl1", "smpl2",
                    "deme1", "deme2",
                    "gendist", "geodist"))
}

disc_wrapper <- function(inputdisc, demesize, steps) {
  inputdisc <- inputdisc %>%
    dplyr::filter(deme1 != deme2)
  our_start_params <- rep(0.01, demesize)
  names(our_start_params) <- 1:demesize
  our_start_params <- c(our_start_params, "m" = 1)
  # start time
  start <- Sys.time()
  mod <- discent::disc(discdat = inputdisc,
                       start_params = our_start_params,
                       learningrate = 1e-3,
                       lambda = 0.001,
                       b1 = 0.9,
                       b2 = 0.999,
                       e = 1e-8,
                       steps = steps,
                       m_lowerbound = 1e-1,
                       m_upperbound = 1e5,
                       normalize_geodist = T,
                       report_progress = F,
                       return_verbose = F)
  timedur <- Sys.time() - start

  return(list(
    mod = mod,
    timedur = timedur
  ))
}


#++++++++++++++++++++++++++++++++++++++++++
###   Part 2: Run polySimIBD and format for DISC     ####
#++++++++++++++++++++++++++++++++++++++++++
pos <- list(sort(sample(1:1e6, size = 1e3)))
ind_n <- c(4, 16, 64)
demesize <- c(2, 5, 10, 25)
migmatdf <- tidyr::expand_grid(pos, ind_n, demesize)
migmatdf <- migmatdf %>%
  dplyr::mutate(inputdisc = purrr::pmap(., sim_swf_2_discdata_wrapper))


#++++++++++++++++++++++++++++++++++++++++++
###   Part 3: Run DISC     ####
#++++++++++++++++++++++++++++++++++++++++++
steps <- c(1e3, 1e4, 5e4, 1e5, 5e5)
disc_complex <- tidyr::expand_grid(migmatdf, steps)
# run analysis
disc_complex$discret <- purrr::pmap(disc_complex[,c("inputdisc", "demesize", "steps")],
                                    disc_wrapper)



#++++++++++++++++++++++++++++++++++++++++++
###   Part 4: Save Out     ####
#++++++++++++++++++++++++++++++++++++++++++
saveRDS(disc_complex, file = "data/sim_data/goDISC_full-complexity_analysis.RDS")
disc_complex_lite <- disc_complex %>%
  dplyr::mutate(timedur = purrr::map_dbl(discret, function(x)x$timedur)) %>%
  dplyr::select(c("ind_n", "demesize", "steps", "timedur"))
saveRDS(disc_complex_lite, file = "data/sim_data/goDISC_lite-complexity_analysis.RDS")
