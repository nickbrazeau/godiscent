#' @title simple function for extracting final cost
extract_final_cost <- function(discret) {
  return(discret$cost[length(discret$cost)])
}


#' @title simple function for getting final fs
get_fs <- function(discret) {
  final_fs <- discret$Final_Fis
  demekey <- discret$deme_key

  demekey$Final_Fis <- final_fs
  ret <- demekey %>%
    dplyr::select(-c("key"))
  return(ret)
}

#' @title simple function for getting final ms
get_ms <- function(discret) {
  return(discret$Final_m)
}

#' @title Basic utility function to subset disc data for start params
sub_maestro <- function(rettargets, lvl) {
  lvl <- rlang::sym(lvl)
  out <- rettargets %>%
    dplyr::filter(modname == !!lvl)
  return(out[1,])
}


#' @title Basic utility function to pull together start param results
find_best_start_param <- function(combined_start, discdat) {
  optimstart <- combined_start %>%
    dplyr::group_by(modname) %>%
    dplyr::filter(cost == min(cost))
  # join and out
  discdatfull <- dplyr::left_join(discdat, optimstart, by = "modname")
  return(discdatfull)

}

#' @title Basic utility function to add in wrong distance test
add_misspec_dist <- function(prediscdat, seed = 48) {
  set.seed(seed)
  # extract out for liftover
  base <- prediscdat %>%
    dplyr::filter(modname == "IsoByDist")
  dists <- base$discdat$geodists[[1]]
  wrongdist <- rexp(n = length(dists), rate = mean(dists))
  # liftover
  base <- base %>%
    dplyr::mutate(discdat =
                    purrr::map(discdat,
                               function(x){ x$geodists <- wrongdist
                               return(x) }))
  # out
  return(dplyr::bind_rows(prediscdat, base))
}
