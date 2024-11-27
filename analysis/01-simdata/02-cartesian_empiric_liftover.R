## .................................................................................
## Purpose: Make Simple 5x5 Grid for backbone
##
## Author: Nick Brazeau
##
##
## Notes:
## .................................................................................
library(tidyverse)
#............................................................
##### PART 0: Make a Square Matrix #####
#...........................................................
nCell <- 25
coords <- round(seq(1, nCell, by = 5))
squarecoords <- expand.grid(coords, coords)
plot(squarecoords)
colnames(squarecoords) <- c("longnum", "latnum")
demeNames <- 1:nrow(squarecoords)

squarecoords <- squarecoords %>%
  dplyr::mutate(deme = demeNames)

# save out basic coords
saveRDS(object = squarecoords,
        "data/sim_data/sim_params/squarecoords.rds")



#.....................................................................
##### PART 2: cartesian distance matrix for discent downstream #####
#........................................................................
# get combinations I need
locatcomb <- t(combn(sort(squarecoords$deme), 2)) %>%
  tibble::as_tibble(., .name_repair = "minimal") %>%
  magrittr::set_colnames(c("deme1", "deme2"))
# get selfs
selves <- tibble::tibble(deme1 = unique(c(locatcomb$deme1, locatcomb$deme2)),
                         deme2 = unique(c(locatcomb$deme1, locatcomb$deme2)))
locatcomb <- dplyr::bind_rows(locatcomb, selves)
# euc
locatcomb <- locatcomb %>%
  dplyr::mutate(geodist = purrr::pmap_dbl(locatcomb, function(deme1, deme2){
    # get long lat
    xy1 <- squarecoords[squarecoords$deme == deme1, c("longnum", "latnum")]
    xy2 <- squarecoords[squarecoords$deme == deme2, c("longnum", "latnum")]
    # euclidean distance
    euc <- dist(rbind(xy1, xy2), method = "euclidean")
    return(euc)})
  )
# make symmetrical
locatcomb_expand <- locatcomb
colnames(locatcomb_expand) <- c("deme2", "deme1", "geodist")
locatcomb <- dplyr::bind_rows(locatcomb, locatcomb_expand)
# remove duplicated diagonals
locatcomb <- locatcomb %>%
  dplyr::filter(!duplicated(locatcomb))
# now tidy up
squarecoords_x <- squarecoords %>%
  dplyr::rename(deme1 = deme,
                deme1longnum = longnum,
                deme1latnum = latnum)
squarecoords_y <- squarecoords %>%
  dplyr::rename(deme2 = deme,
                deme2longnum = longnum,
                deme2latnum = latnum)
locatcomb <- locatcomb %>%
  dplyr::left_join(., squarecoords_x, by = "deme1") %>%
  dplyr::left_join(., squarecoords_y, by = "deme2")

# expect this to be lower tri + upper tri + diagonals
goodegg:::assert_eq(nrow(locatcomb), choose(25,2)*2 + 25)
# save out for downstream
saveRDS(locatcomb, "data/sim_data/sim_params/locatcombo.rds")

