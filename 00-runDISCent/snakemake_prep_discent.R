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
library(here)
setwd(here::here()) # respect proj
source("R/utils.R")

#............................................................
# read in discdat from polySimIBD to run DISCent on
#...........................................................
simdat <- readRDS("simdata/simulation_maestro.RDS")
# get file list of results
fn <- data.frame(path = list.files("simdata/swf_results", full.names = T),
                 randseedkey = as.numeric( stringr::str_extract(list.files("simdata/swf_results/"), "\\d+") ) )
# drop readme
fn <- fn[stringr::str_detect(fn$path, ".md", negate = T),]

# bring together
simdat <- dplyr::inner_join(simdat, fn)

# trim it down
discdat <- simdat %>%
  dplyr::select(c("path", "randseedkey")) %>%
  dplyr::mutate(discdat = purrr::map(path, readRDS)) %>%
  dplyr::mutate(discdat = purrr::map(discdat, "ibddat"))

#............................................................
# make search grid of start params
#...........................................................
# look up tables
fstartsvec <- c(0.1, 0.2, 0.3)
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
# save out discent plan for Snakemake
#...........................................................
SGpath <- "simdata/discent_SG_guides/"
discretpath <- "discret/"
dir.create(SGpath)
dir.create(discretpath)
# write out for each now
writeSGrds <- function(randseedkey, data, SGpath){
  outnm <- paste0(SGpath, "_key", randseedkey, "_inputs.RDS")
  saveRDS(data, outnm)
  return(outnm)
}
 # make output file
getout <- function(randseedkey, data) {
  return(paste0(discretpath, "key", randseedkey, "_DISCret.RDS"))
}

fulldiscdat <- fulldiscdat %>%
  dplyr::select(-c("path")) %>%
  dplyr::mutate(datpath =
                  purrr::pmap_chr(., writeSGrds, SGpath = SGpath),
                outpath = purrr::pmap_chr(., getout))
fulldiscdattsv <- fulldiscdat %>%
  dplyr::select(c("datpath", "outpath"))
readr::write_tsv(x = fulldiscdattsv,
                 file = "simdata/disc_SGparamguide.tsv")

