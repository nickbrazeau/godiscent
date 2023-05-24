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
set.seed(48)

#............................................................
# read in and make polysim IBD dataframe
#...........................................................
reps <- 100
maestro <- readRDS("simdata/simulation_setup/simulation_maestro.RDS")
maestro <- lapply(1:reps, function(x){
  maestro <- maestro %>%
    dplyr::mutate(rep = x) %>%
    dplyr::select(c("modname", "rep", dplyr::everything()))
  return(maestro)}
) %>%
  dplyr::bind_rows()
locatcomb <- readRDS("simdata/simulation_setup/inputs/locatcombo.rds")


#............................................................
# run simulations on slurm
#...........................................................
ret <- maestro %>% dplyr::select(c("modname", "rep"))
plan(future.batchtools::batchtools_slurm, workers = 1028,
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


#............................................................
# make "wrong IBD" to look at cost
#...........................................................
wrongIBD <- ret %>%
  dplyr::filter(modname == "IsoByDist")


# make wrong dist by randomly adding noise to real distances
wrongdist <- realdist <- sort(unique(wrongIBD$discdat[[1]]$geodist)) # sort for perserving 0
wrongdist <- wrongdist + rnorm(length(wrongdist), mean = mean(wrongdist), sd = sd(wrongdist))
if(!all(wrongdist > 0)){
  warning("Random distances not all positive; converted")
  wrongdist <- abs(wrongdist)
}
wrongdist[1] <- 0
# combine for liftover
distkey <- tibble::tibble(
  geodist = realdist,
  newgeodist = wrongdist)

# function for liftover
replace_wrong_dist <- function(discdatsing, distkeylo){
  out <- discdatsing %>%
    dplyr::left_join(., distkeylo, by = "geodist") %>%
    dplyr::select(-c("geodist")) %>%
    dplyr::rename(geodist = newgeodist)
  return(out)
}

# now loop through and replace
wrongIBD <- wrongIBD %>%
  dplyr::mutate(modname = "badIsoByDist",
                discdat = purrr::map(discdat, replace_wrong_dist, distkeylo = distkey))

# # confirm
# all( wrongIBD$discdat[[1]]$geodist == wrongIBD$discdat[[2]]$geodist )
# all( which(wrongIBD$discdat[[1]]$geodist == 0) == which(rightdist == 0) )

# bring together
ret <- ret %>%
  dplyr::bind_rows(., wrongIBD) %>%
  dplyr::arrange(rep, modname)

#............................................................
# save out
#...........................................................
dir.create("simdata/sim_results/", recursive = T)
saveRDS(ret, "simdata/sim_results/discdat_from_polySimIBD_maestro.RDS")
