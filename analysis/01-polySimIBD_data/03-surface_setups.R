## .................................................................................
## Purpose: Make various surfaces (aka "migration" matrices) for polySimIBD simulations
##
## Author: Nick Brazeau
##
## Date: 17 November, 2022
##
## Notes: Will have three types of "migration":
##    1) Torus: Isotropic Homogenous with Periodic Boundaries
##              Essentially a flat surface with no barriers to movement
##              Expect quick coalescence given high connectivity and little population structure
##
##    1) Bounding-Box: Isotropic Homogenous with Reflecting Boundaries
##              Essentially a flat surface but with edge effects
##              Expect quick coalescence given high conn in center but population structure at boundaries/more differentiation
##
##    1) Dexter-Box: Anisotropic (no left) Homogenous with Reflecting Boundaries
##              Essentially a flat surface with no barriers to movement
## .................................................................................
library(tidyverse)
library(cowplot)

#............................................................
##### PART 0: Template Pieces #####
#...........................................................
# read in coords
squarecoords <- readRDS("data/sim_data/sim_params/squarecoords.rds")

# storage
migmatdf <- tibble::tibble(modname = c("torus",
                                       "bound",
                                       "dexter"),
                           migmat = NA)


# template
nInds <- nrow(squarecoords)
nMov <- sqrt(nInds)
tempmigmat <- matrix(0, nInds, nInds)


#............................................................
##### PART 1: Isotropic Homogenous with Periodic Boundaries #####
#...........................................................
# store x and y for
torusmigmat <- tempmigmat

# deme looks right  = row + 1 unless divisible by sqrt(nInds) (wall)
for (i in 1:nrow(torusmigmat)) {
  if (i %% nMov != 0) {
    torusmigmat[i, i + 1] <- 1
  }
}

# deme looks up = row + sqrt(nInds) in square
for (i in 1:nrow(torusmigmat)) {
  if (i + nMov <= ncol(torusmigmat)  ) {
    torusmigmat[i, i + nMov] <- 1
  }
}

# deme looking down and left is just the reflection of the right and up matrix we made
torusmigmat[lower.tri(torusmigmat)] <- t(torusmigmat)[lower.tri(torusmigmat)]

# deme wrapping around right to left and left to right
wrparnd <- seq(nMov, nInds, by = nMov)
wrparnd_back <- seq(1, nInds, by = nMov)
torusmigmat[cbind(wrparnd, wrparnd_back)] <- 1
torusmigmat[cbind(wrparnd_back, wrparnd)] <- 1

# deme wrapping around top to bottom and bottom to top
wrparnd <- 1:nMov
wrparnd_back <- (nInds-nMov+1):nInds
torusmigmat[cbind(wrparnd, wrparnd_back)] <- 1
torusmigmat[cbind(wrparnd_back, wrparnd)] <- 1

# store
migmatdf$migmat[migmatdf$modname == "torus"] <- list(torusmigmat)


#............................................................
##### PART 2: Isotropic Homogenous with Reflecting Boundaries #####
#...........................................................
# store x and y for
boundboxmigmat <- tempmigmat

# deme looks right  = row + 1 unless divisible by sqrt(nInds) (wall)
for (i in 1:nrow(boundboxmigmat)) {
  if (i %% nMov != 0) {
    boundboxmigmat[i, i + 1] <- 1
  }
}

# deme looks up = row + sqrt(nInds) in square
for (i in 1:nrow(boundboxmigmat)) {
  if (i + nMov <= ncol(boundboxmigmat)  ) {
    boundboxmigmat[i, i + nMov] <- 1
  }
}

# deme looking down and left is just the reflection of the right and up matrix we made
boundboxmigmat[lower.tri(boundboxmigmat)] <- t(boundboxmigmat)[lower.tri(boundboxmigmat)]

# store
migmatdf$migmat[migmatdf$modname == "bound"] <- list(boundboxmigmat)

#............................................................
##### PART 3: Anisotropic Homogenous with Reflecting Boundaries #####
#...........................................................
# store x and y for
dextermigmat <- tempmigmat

# deme looks up = row + sqrt(nInds) in square
for (i in 1:nrow(dextermigmat)) {
  if (i + nMov <= ncol(dextermigmat)  ) {
    dextermigmat[i, i + nMov] <- 1
  }
}

# deme looking down reflect to look up
dextermigmat[lower.tri(dextermigmat)] <- t(dextermigmat)[lower.tri(dextermigmat)]

# deme looks right  = row + 1 unless divisible by sqrt(nInds) (wall)
for (i in 1:nrow(dextermigmat)) {
  if (i %% nMov != 0) {
    dextermigmat[i, i + 1] <- 1
  }
}


# store
migmatdf$migmat[migmatdf$modname == "dexter"] <- list(dextermigmat)

#............................................................
# save out
#...........................................................
dir.create("data/sim_data/sim_params/migmat_surface_grids/")
saveRDS(migmatdf$migmat[[1]],
        "data/sim_data/sim_params/migmat_surface_grids/torus.RDS")
saveRDS(migmatdf$migmat[[2]],
        "data/sim_data/sim_params/migmat_surface_grids/bound.RDS")
saveRDS(migmatdf$migmat[[3]],
        "data/sim_data/sim_params/migmat_surface_grids/dexter.RDS")
saveRDS(migmatdf,
        "data/sim_data/sim_params/migmat_framework.RDS")

#............,................................................
##### Plot Out Results #####
#...........................................................
# liftovers for plot
squarecoordsnmleft <- squarecoords %>%
  dplyr::mutate(demex = 1:25)
squarecoordsnmright <- squarecoords %>%
  dplyr::mutate(demey = 1:25)

plot_migmat <- function(migmatdbl, squarecoordsnmleft, squarecoordsnmright, nInds) {
  # liftover dist matrix
  plotdf <- tibble::tibble(
    demex = rep(seq(1,nInds), nInds),
    demey = sort(rep(seq(1,nInds), nInds)),
    distance = as.numeric(migmatdbl))

  #plot df
  plotdf <- plotdf %>%
    dplyr::left_join(., y = squarecoordsnmleft, by = "demex") %>%
    dplyr::left_join(., y = squarecoordsnmright, by = "demey") %>%
    dplyr::filter(distance == 1)

  # plot
  ggplot() +
    geom_point(data = squarecoords, aes(x = longnum, y = latnum), size = 3, alpha = 0.8, color = "#bdbdbd") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 6)) +
    geom_curve(data = plotdf, aes(x = longnum.x, y = latnum.x,
                                  xend = longnum.y, yend = latnum.y),
               arrow = arrow(length = unit(0.03, "npc")), color = "#737373",
               curvature = 0.1, alpha = 0.5)

}


plotObjs <- lapply(migmatdf$migmat, plot_migmat,
                   squarecoordsnmleft = squarecoordsnmleft,
                   squarecoordsnmright = squarecoordsnmright,
                   nInds = nInds)

# tidy up
plotObjs[[1]] <- plotObjs[[1]] +
  ggtitle("Isotropic Homogenous with Periodic Boundaries") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", size = 12)) +
  theme(plot.margin = unit(c(0.8, 0.6, 0.4, 0.8), "cm"))

plotObjs[[2]] <- plotObjs[[2]] +
  ggtitle("Isotropic Homogenous with \n Reflecting Boundaries") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", size = 12)) +
  theme(plot.margin = unit(c(0.8, 0.6, 0.4, 0.8), "cm"))

plotObjs[[3]] <- plotObjs[[3]] +
  ggtitle("Anisotropic (no left) Homogenous \n with Reflecting Boundaries") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", size = 12)) +
  theme(plot.margin = unit(c(0.8, 0.6, 0.4, 0.8), "cm"))

# out
p1 <- cowplot::plot_grid(plotObjs[[1]], plotObjs[[2]], plotObjs[[3]],
                   nrow = 1)
p1
jpeg("figures/migmat_surfaces_arrows.jpg", width = 24, height = 8,
     units = "in", res = 500)
p1
graphics.off()

# save plot for future use
saveRDS(p1,
        "data/sim_data/sim_params/migmat_framework_ggplot.RDS")



