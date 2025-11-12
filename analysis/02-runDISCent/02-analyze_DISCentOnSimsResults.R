## .................................................................................
## Purpose: Analyze DISCent runs
##
## Author: Nick Brazeau
##
##
## .................................................................................

# import libraries
library(tidyverse)
library(cowplot)
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
# discent runs search-grid
search_discdat <- readRDS("data/sim_data/goDISC_search-DISC_simulated_gendata.RDS")
# discent runs full
discdat <- readRDS("data/sim_data/goDISC_full-DISC_simulated_gendata.RDS")
# bring together
sim_disc_dat <- dplyr::left_join(simdat, discdat, by = "modname")

# split out sections
torus <- sim_disc_dat %>%
  dplyr::filter(stringr::str_detect(modname, "torus-rep-*[0-9]"))
bound <- sim_disc_dat %>%
  dplyr::filter(stringr::str_detect(modname, "bound-rep-*[0-9]"))
dexter <- sim_disc_dat %>%
  dplyr::filter(stringr::str_detect(modname, "dexter-rep-*[0-9]"))
torusNe <- sim_disc_dat %>%
  dplyr::filter(stringr::str_detect(modname, "torus-NeVary-rep-*[0-9]"))
boundNe <- sim_disc_dat %>%
  dplyr::filter(stringr::str_detect(modname, "bound-NeVary-rep-*[0-9]"))

#............................................................
# Helper Functions
#...........................................................
plot_swf_sim <- function(locatdat, IBDcalc, mod, modname, threshold, alpha = 0.5){
  #......................
  # tidy up location data for Sim IBD Plot
  #......................
  locatdat1 <- locatdat %>%
    dplyr::select(dplyr::contains("1")) %>%
    dplyr::filter(!duplicated(.))
  locatdat2 <- locatdat %>%
    dplyr::select(dplyr::contains("2")) %>%
    dplyr::filter(!duplicated(.))
  # bring together
  simplotdat <- ibddat %>%
    dplyr::left_join(., y = locatdat1, by = "deme1") %>%
    dplyr::left_join(., y = locatdat2, by = "deme2")
  #......................
  # plot sim IBD
  #......................
  p1 <- simplotdat %>%
    dplyr::filter(gendist > threshold) %>%
    ggplot() +
    geom_point(aes(x = deme1longnum, y = deme1latnum),
               color = "#d9d9d9", alpha = 0.2) +
    geom_segment(aes(x = deme1longnum, y = deme1latnum,
                     xend = deme2longnum, yend = deme2latnum,
                     color = gendist), alpha = alpha) +
    viridis::scale_color_viridis("IBD Dist.") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())

  #......................
  # mod data clean up
  #......................
  discplotdat <- dplyr::bind_cols(mod$deme_key, mod$Final_Fis)
  colnames(discplotdat) <- c("deme1", "key", "disc")
  p2 <- discplotdat %>%
    dplyr::left_join(.,locatdat1, by = "deme1") %>%
    ggplot() +
    geom_point(aes(x = deme1longnum, y = deme1latnum, fill = disc),
               shape = 21,
               size = 3) +
    theme_minimal() +
    scale_fill_viridis("DISC") +
    theme(axis.title = element_blank())

  #......................
  # add title and subtitle
  #......................
  # Combine plots side by side
  plots <- cowplot::plot_grid(p1, p2, labels = NULL)

  # Create title and subtitle as separate ggdraws
  titlechar <- sim_disc_dat
  title <- ggdraw() +
    draw_label(modname,
               fontface = 'bold', size = 18, x = 0.5, hjust = 0.5)

  subtitle1 <- ggdraw() +
    draw_label(paste("IBD cutoff for viz: ", threshold),
               size = 12, x = 0.5, hjust = 0.5)
  subtitle2 <- ggdraw() +
    draw_label(paste("Final M calculated:", round(mod$Final_m, digits = 3)),
               size = 12, x = 0.5, hjust = 0.5)
  subtitle <- cowplot::plot_grid(subtitle1, subtitle2, labels = NULL)


  # Combine title, subtitle, and plots
  final_plot <- plot_grid(
    title,
    plots,
    subtitle,
    ncol = 1,
    rel_heights = c(0.1,1,0.05)
  )

  # Display
  final_plot
}




#++++++++++++++++++++++++++++++++++++++++++
### Part 1: Summary Statistics of DISC Search     ####
#++++++++++++++++++++++++++++++++++++++++++
summary(search_discdat$cost)
hist(search_discdat$cost)

search_discdat %>%
  dplyr::mutate(
    overallmodname = stringr::str_extract(string = modname, pattern = "^(.*?)(?=-rep)")) %>%
  dplyr::group_by(overallmodname) %>%
  dplyr::summarise(
    mincost = min(cost),
    firstqcost = quantile(cost, prob = 0.25),
    mediancost = quantile(cost, prob = 0.5),
    meancost = mean(cost),
    firstqcost = quantile(cost, prob = 0.75),
    sdcost = sd(cost),
    maxcost = max(cost) ) %>%
  DT::datatable(.,
              rownames = F,
              extensions='Buttons',
              options = list(
                searching = T,
                pageLength = 10,
                dom = 'Bfrtip',
                autoWidth = TRUE,
                buttons = c('csv')))


finalMs <- unlist(purrr::map(sim_disc_dat$mod, "Final_m"))
finalFs <- unlist(purrr::map(sim_disc_dat$mod, "Final_Fis"))

summary( finalMs )
hist( finalMs )
summary( finalFs )
hist(finalFs)



torusfinalFs <- unlist(purrr::map(torus$mod, "Final_Fis"))
dexterfinalFs <- unlist(purrr::map(dexter$mod, "Final_Fis"))
summary(torusfinalFs)
summary(dexterfinalFs)



#++++++++++++++++++++++++++++++++++++++++++
### Part 3: Visualize Results     ####
#++++++++++++++++++++++++++++++++++++++++++
sim_disc_dat$viz <- purrr::pmap(sim_disc_dat[,c("IBDcalc", "mod", "modname")], plot_swf_sim,
                                threshold = 0.1, alpha = 0.5, locatdat = locatdat)


# # save out
# sim_disc_dat %>%
#   dplyr::select(c("modname", "viz")) %>%
#   saveRDS(., file = "data/sim_data/goDISC_visualization-full-DISC_simulated_gendata.RDS")
