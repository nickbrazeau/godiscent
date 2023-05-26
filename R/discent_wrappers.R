#' @details Wrapper for discent::deme_inbreeding_spcoef
#' with prespecified inputs
#'

get_discentwrapper <- function(dat) {

  #......................
  # filter internal
  #......................
  dat <- dat %>%
    dplyr::mutate(discdat = purrr::map(discdat, function(inddat){
      inddat %>%
        dplyr::filter(deme1 != deme2)
    }))
  #......................
  # set up main function
  #......................
  discrun <- function(discdat, start_params, f_learn, m_learn, start_param_realization) {
    out <- discent::deme_inbreeding_spcoef(discdat = discdat,
                                    start_params = start_params,
                                    f_learningrate = f_learn,
                                    m_learningrate = m_learn,
                                    m_lowerbound = 0,
                                    m_upperbound = Inf,
                                    b1 = 0.9,
                                    b2 = 0.999,
                                    e = 1e-8,
                                    normalize_geodist = TRUE,
                                    steps = 5e5,
                                    thin = 50,
                                    report_progress = FALSE,
                                    return_verbose = FALSE)
    return(out)
  }

  #......................
  # run over batched map
  #......................
  out <- dat %>%
    dplyr::mutate(discret = purrr::pmap(., discrun)) %>%
    dplyr::select(c("discret"))

  return(out)
}
