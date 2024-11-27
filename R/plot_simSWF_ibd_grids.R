locatcombo <- readRDS("data/sim_data/sim_params/locatcombo.rds")

#' @title
#' @param
#' @description
#' @details
#' @returns
#' @export

plot_bvIBD_godisc_sim <- function(IBDdf, locatcombo) {
  # core
  IBDdflc <- dplyr::left_join(IBDdf, locatcombo, by = c("deme1", "deme2"))
  ggplot(data = IBDdflc) +
    geom_segment(aes(x = deme1longnum, y = deme1latnum,
                     xend = deme2longnum, yend = deme2latnum,
                     color = gendist)) +
    scale_colour_viridis_c() +
    geom_point(aes(x = deme1longnum, y = deme1latnum),
               color = "#252525")

  # out

  return(ret)
}

