

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


