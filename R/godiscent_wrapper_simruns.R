## .................................................................................
## Purpose: Wrapper for discent::deme_inbreeding_spcoef with prespecified inputs
## Author: Nick Brazeau
## .................................................................................

# Temporarily suppress warnings fto avoid the issue of strict snakemake bash mode
defaultwarnings <- getOption("warn")
options(warn = -1)

#++++++++++++++++++++++++++++++++++++++++++
### dependencies     ####
#++++++++++++++++++++++++++++++++++++++++++
deps <- c("discent", "goodegg", "magrittr", "tibble", "optparse")
deps <- !sapply(deps, function(x){x %in% installed.packages()[,1]} ) # note this a named vector

# catch fomes remote
if(deps["discent"]) {
  if (!"remotes" %in% installed.packages()[,1]){
    install.packages("remotes")
  }
  remotes::install_github("nickbrazeau/discent")
  deps <- deps[names(deps) != "discent"]
}

# catch goodegg remote
if(deps["goodegg"]) {
  if (!"remotes" %in% installed.packages()[,1]){
    install.packages("remotes")
  }
  remotes::install_github("nickbrazeau/goodegg")
  deps <- deps[names(deps) != "goodegg"]
}

# rest of deps
if (any(deps)) {
  install.packages(names(deps)[deps])
}

#......................
# call dependencies
#......................
library(optparse)
library(discent)
library(goodegg)
library(tibble)
library(magrittr)

#++++++++++++++++++++++++++++++++++++++++++
### Function for wrapping discent      ####
#++++++++++++++++++++++++++++++++++++++++++

get_discentwrapper <- function(datpath) {
  #......................
  # read data from specified path from prev setup
  #......................
  dat <- readRDS(datpath)

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
    dplyr::select(-c("discret"))
  return(out)
}


#++++++++++++++++++++++++++++++++++++++++++
### Command Line     ####
#++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++
#### parse CL inputs     #####
#++++++++++++++++++++++++++++++++++++++++++
option_list=list(

  optparse::make_option(c("-i", "--datpath"),
              type = "character", default = NULL,
              help = paste("File path for simulation data to consider"),
              metavar = "character"),


  optparse::make_option(c("-o", "--outpath"),
              type = "character", default = NULL,
              help = paste("Output fn to write discent result"),
              metavar = "character"),

  optparse::make_option(c("-d", "--outdir"),
                        type = "character", default = NULL,
                        help = paste("Output dir to write discent result"),
                        metavar = "character")
)

opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)

#++++++++++++++++++++++++++++++++++++++++++
### Run What you Brung ####
#++++++++++++++++++++++++++++++++++++++++++
out <- get_discentwrapper(datpath = opt$datpath)
#save out
saveRDS(out,
        file = paste0(opt$outdir, "/", opt$outpath))

# turn warnings back to default
options(warn = defaultwarnings)
