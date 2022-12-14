

#' @title Wrapper for Search Grid Optimal Start Parameters
#' @details going to assume one "model" search is suffice
#'
get_GS_cost <- function(start_params, f_learn, m_learn, discdat) {
  # filter internal
  discdat <- discdat %>%
    dplyr::filter(deme1 != deme2)
  # run
  discret <- discent::deme_inbreeding_spcoef(discdat = discdat,
                                          start_params = start_params,
                                          f_learningrate = f_learn,
                                          m_learningrate = m_learn,
                                          m_lowerbound = 1e-5,
                                          m_upperbound = 1,
                                          momentum = 0.9,
                                          steps = 1e4,
                                          report_progress = FALSE,
                                          return_verbose = FALSE)

  return(discret)
}



get_discentwrapper <- function(discdat,
                               start_params, f_learn, m_learn) {
  # filter internal
  discdat <- discdat %>%
    dplyr::filter(deme1 != deme2)
  # run main
  out <- discent::deme_inbreeding_spcoef(discdat = discdat,
                                         start_params = start_params,
                                         f_learningrate = f_learn,
                                         m_learningrate = m_learn,
                                         m_lowerbound = 1e-5,
                                         m_upperbound = 1,
                                         momentum = 0.9,
                                         steps = 1e5,
                                         report_progress = FALSE,
                                         return_verbose = FALSE)
  return(out)
}
