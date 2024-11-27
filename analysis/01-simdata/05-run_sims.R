## .................................................................................
## Purpose: Run polySimIBD on generated data
##
## Author: Nick Brazeau
##
## Date: 28 August, 2024
##
## Notes:
## ...............................................................................
library(remotes)
remotes::install_github("nickbrazeau/polySimIBD")
library(polySimIBD)
library(tidyverse)
remotes::install_github("andrewparkermorgan/rplasmodium")
library(rplasmodium)
set.seed(48)
#++++++++++++++++++++++++++++++++++++++++++
###   Part 0: Magic Numbers: immutable throughout simulations     ####
#++++++++++++++++++++++++++++++++++++++++++
rep <- 1:5 # number of simulation realizations to perform
tlim <- 10 # assume IBD to 10 generations for recent coalescent
# positions across the 14 chromosomes, assume ~100 loci across all chromosomes distributed euqally
wi <- rplasmodium::chromsizes_3d7()[1:14]/sum(rplasmodium::chromsizes_3d7()[1:14])
locimarkers <- ceiling( wi * 1e3 )
pos <- c()
for(i in 1:14) {
  newpos <- sample(x = 1:rplasmodium::chromsizes_3d7()[i],
                   size = locimarkers[i],
                   replace = F)
  newpos <- newpos + 1e9*i # add distance between chromosomes to break up recombination
  pos <- append(pos, newpos)
}
pos <- sort( sample(pos, size = 1e3, replace = F) ) # downsample back to 1e3

# Miles et al. 2016 (PMC5052046) & Taylor et al. 2019 (PMC6707449) gives us a recombination rate by 7.4e-7 M/bp
# by taking the inverse of the estimate of the CO recombination rate of 13.5 kb/cM
rho <- 7.4e-7
# COI of 1 preferred and mix of superinfection/coinfection given study question
lambdaCOI <-  mean( readRDS("data/sim_data/sim_params/optim_lambda.RDS")[1:2] )
mscale <- 0.5
# number of initial individuals per deme
N <- 5

#++++++++++++++++++++++++++++++++++++++++++
### Part 1: Read in Data        ####
#++++++++++++++++++++++++++++++++++++++++++
migmatdf <- readRDS("data/sim_data/sim_params/NeVary_migmat_framework.RDS")
demesnum <- nrow(migmatdf$migmat[[1]])
# update vectors for deme size
lambdaCOI <- list( rep(lambdaCOI, demesnum) )
mscale <- list( rep(mscale, demesnum) )

# manipulate and update migmatdf
migmatdf <- migmatdf %>%
  dplyr::select(-c("modname")) %>%
  dplyr::mutate(pos = list( pos ),
                N = purrr::map(NeVaryMult, function(x){floor(x * N)}),
                m = mscale,
                rho = rho,
                mean_coi = lambdaCOI,
                tlim = tlim
  ) %>%
  dplyr::rename(migr_mat = migmat) %>%
  dplyr::select(-c("NeVaryMult"))

# expand grid
migmatdf <- tidyr::expand_grid(migmatdf, rep)
migmatdf <- migmatdf %>%
  dplyr::select(c("modnameNe", "rep", dplyr::everything()))

#............................................................
# Lift Over Migration Matrix for internal migration
# (right now, nothing on diagonal)
#...........................................................
# TODO think about this
@@@
migmatdf$migr_mat <- purrr::map(migmatdf$migr_mat, function(x, intmigmat){
  wi <- rowSums(x) # spread out migration prob
  intmigmat <- intmigmat/wi
  x <- apply(x, 2, function(x){x * intmigmat})
  diag(x) <- (100 - intmigmat*wi)
  return(x)
}, intmigmat = 10) # qwhile general 1% rule migration causing apanmictic pop, our generation memory is only 10, so does not have same effect


#++++++++++++++++++++++++++++++++++++++++++
### Part 2: Run polySimIBD        ####
#++++++++++++++++++++++++++++++++++++++++++
migmatdf$swfsim <- purrr::pmap(migmatdf[,3:9], polySimIBD::sim_swf)

#++++++++++++++++++++++++++++++++++++++++++
### Part 3: Get IBD from polySimIBD Realization       ####
#++++++++++++++++++++++++++++++++++++++++++
# thin columns
migmatdf_IBD <- migmatdf %>%
  dplyr::select(c("modnameNe", "rep", "migr_mat", "N", "swfsim"))

# internal function to lift over
get_swfsim_2_ibd <- function(swfsim, N, dwnsmplnum = 5){
  # get start and end ind counts for each deme (ie account for when deme size varies)
  # remember, host index is counted as 1:sum(N)
  inds <- lapply(N, function(x){seq(1, x, by = 1)}) # list of inds by deme
  end <- cumsum(sapply(inds, max))
  start <- end + 1 # next start is end + 1, except for last individual
  start <- c(1, start[1:(length(start)-1)])
  # downsample to "N" individuals per deme
  dwnsmpl <- mapply(function(x,y){sample(x:y, size = dwnsmplnum, replace = F)},
                    x = start, y = end, SIMPLIFY = F)
  dwnsmpl <- sort(unlist(dwnsmpl))
  # get combinations
  comb_hosts_df <- t(combn(dwnsmpl, 2)) %>%
    data.frame(.) %>%
    magrittr::set_colnames(c("smpl1", "smpl2"))
  # get pairwise IBD
  comb_hosts_df <- comb_hosts_df %>%
    dplyr::mutate(gendist = purrr::map2_dbl(smpl1, smpl2, function(x, y, swf) {
      return(polySimIBD::get_bvibd(swf = swf, host_index = c(x, y)))
    }, swf = swfsim))

  # apply demes
  dms <- as.numeric( cut(dwnsmpl, breaks = c(0,cumsum(N))) ) #intelligent coercion of factor to numeric to represent demes
  demeliftoverx <- tibble::tibble(smpl1 = dwnsmpl,
                                  deme1 = dms)
  demeliftovery <- tibble::tibble(smpl2 = dwnsmpl,
                                  deme2 = dms)
  #......................
  # bring together
  #......................
  comb_hosts_df <- comb_hosts_df %>%
    dplyr::left_join(., demeliftoverx, by = "smpl1") %>%
    dplyr::left_join(., demeliftovery, by = "smpl2")
  return(comb_hosts_df)
}

#......................
# run IBD
#......................
migmatdf_IBD$IBDcalc <- purrr::pmap(migmatdf_IBD[, c("swfsim", "N")],
                                    get_swfsim_2_ibd,
                                    dwnsmplnum = 5)



#............................................................
# Explaratory Data Analysis
#...........................................................
locatdat <- readRDS("data/sim_data/sim_params/locatcombo.rds")
plot_swf_sim <- function(locatdat, ibddat, threshold, alpha = 0.3){
  #......................
  # tidy
  #......................
  locatdat1 <- locatdat %>%
    dplyr::select(dplyr::contains("1")) %>%
    dplyr::filter(!duplicated(.))
  locatdat2 <- locatdat %>%
    dplyr::select(dplyr::contains("2")) %>%
    dplyr::filter(!duplicated(.))
  #......................
  # bring together
  #......................
  plotdat <- ibddat %>%
    dplyr::left_join(., y = locatdat1, by = "deme1") %>%
    dplyr::left_join(., y = locatdat2, by = "deme2")

  #......................
  # plot
  #......................
  plotdat %>%
    dplyr::filter(gendist > threshold) %>%
    ggplot() +
    geom_point(aes(x = deme1longnum, y = deme1latnum),
               color = "#d9d9d9", alpha = 0.5) +
    geom_segment(aes(x = deme1longnum, y = deme1latnum,
                     xend = deme2longnum, yend = deme2latnum,
                     color = gendist), alpha = alpha) +
    viridis::scale_color_viridis() +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
}

#......................
# viz
#......................
plot_swf_sim(locatdat = locatdat,
             ibddat = migmatdf_IBD$IBDcalc[[11]],
             threshold = 0.25)

