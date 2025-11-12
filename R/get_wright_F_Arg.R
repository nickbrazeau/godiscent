
#' @title Calculate Wright's Fst from an Ancestral Recombination Graph (ARG)
#' @param swf Structured Wright Fisher Model for IBD that is a list of length seven that contains \enumerate{
#'  \item pos: The simulated genetic coordinates
#'  \item coi: The COI of each individual
#'  \item recomb: A recombination list of length of tlim where each element contains
#'  the recombination block -- as a boolean -- of the two parental haplotypes.
#'  \item parent_host1: the parental host assignments for the "paternal" haplotype
#'  \item parent_host2: the parental host assignments for the "maternal" haplotype
#'  \item parent_haplo1 "paternal" haplotype assigment (as above)
#'  \item parent_haplo2 "maternal" haplotype assigment (as above)
#'  }
#' @param weight_loci Optional vector of weights (e.g., genomic lengths) based on physical distance. Default is equal weighting.
#' @return A single numeric value representing genome-wide Fst.
#' @description This function computes Wright's Fst based on coalescent times extracted from an Ancestral Recombination Graph (ARG).
#' It partitions pairwise coalescent times into within- and between-population groups and estimates Fst as:
#' \deqn{Fst = (T_between - T_within) / T_between}
#' @export
#'
calculate_demeij_Fst_from_ARG <- function(swf, deme1smpls, deme2smpls, weight_loci = 1) {

  #............................................................
  # checks
  #............................................................
  goodegg::assert_class(swf, "swfsim")
  goodegg::assert_vector_numeric(deme1smpls)
  goodegg::assert_vector_numeric(deme2smpls)
  goodegg::assert_vector_numeric(weight_loci)

  #............................................................
  # setup (const, storage, etc)
  #............................................................
  # ensure nxn matrix for diagonal
  fstdistmat <- matrix(NA, nrow = length(deme1smpls), ncol = length(deme2smpls))

  #............................................................
  # core
  #............................................................
  # Between Deme Fst
  for (i in 1:length(deme1smpls)) { # for every combination between
    for (j in 1:length(deme2smpls)) {
      arg <- polySimIBD::get_arg(swf, host_index = c(deme1smpls[i], deme2smpls[j]))
      l <- unlist(purrr::map(arg, "t"))
      # if not coalesced, then NA
      l[l == -1] <- NA
      if ( all(is.na(l)) ) {
        fstdistmat[i,j] <- NA
      } else {
        # weighted average
        if (weight_loci != 1){
          lw <- sum(l * weight_loci)/sum(weight_loci)
        } else {
          lw = l
        }
        fstdistmat[i,j] <- mean(lw, na.rm = T)
      }
    }
  }
  T_between <- mean(fstdistmat, na.rm = T)

  # Within Deme-1 Fst
  d1c <- do.call("rbind", combn(deme1smpls, 2, simplify = F))
  withinFst_D1 <- rep(NA, nrow(d1c))
  for (i in 1:nrow(d1c)) { # for every combination within d1 v
    arg <- polySimIBD::get_arg(swf, host_index = d1c[i,])
    l <- unlist(purrr::map(arg, "t"))
    # if not coalesced, then NA
    l[l == -1] <- NA
    if ( all(is.na(l)) ) {
      withinFst_D1[i] <- NA
    } else {
      # weighted average
      if (weight_loci != 1){
        lw <- sum(l * weight_loci)/sum(weight_loci)
      } else {
        lw = l
      }
      withinFst_D1[i] <- mean(lw, na.rm = T)
    }
  }
  withinFst_D1 <- mean(withinFst_D1, na.rm = T)

  # Within Deme-2 Fst
  d2c <- do.call("rbind", combn(deme2smpls, 2, simplify = F))
  withinFst_D2 <- rep(NA, nrow(d2c))
  for (i in 1:nrow(d2c)) { # for every combination within d2
    arg <- polySimIBD::get_arg(swf, host_index = d2c[i,])
    l <- unlist(purrr::map(arg, "t"))
    # if not coalesced, then NA
    l[l == -1] <- NA
    if ( all(is.na(l)) ) {
      withinFst_D2[i] <- NA
    } else {
      # weighted average
      if (weight_loci != 1){
        lw <- sum(l * weight_loci)/sum(weight_loci)
      } else {
        lw = l
      }
      withinFst_D2[i] <- mean(lw, na.rm = T)
    }
  }
  withinFst_D2 <- mean(withinFst_D2, na.rm = T)


  #............................................................
  # out
  #............................................................
  # Calculate Fst
  T_within_mn <- (withinFst_D1 + withinFst_D2)/2
  return( (T_between - T_within_mn) / T_between )
}
