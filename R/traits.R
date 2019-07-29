#' @name get_growth_rate
#' @title get_growth_rate
#' @usage Uses all alleles at loci to determine growth rate with minimum value of 1.
#' @authors Rose McKeon and Brad Duthie
#' @param genome A dataframe contining the genome of an individual
#' @param loci a numeric vector containing positive integer/s (eg: 1 or 1:5) which represents the locus/loci to use for the trait growth rate (default = 1).
#' @param ploidy_benefit A number between 0 and 1 that represents the proportion by which being polyploid improves growth rate (default = 1, maximum benefit).
#' @param max_rate A number representing the maximum rate which can be output no matter the genes (default = 2, so individuals can never more than double in size in a generation).
#' @return number greater than or equal to 1 used to multiply size in grow("individuals").
#' @export
get_growth_rate <- function(
  genome,
  ploidy_benefit = 1,
  loci = 1,
  max_rate = 2
){
  # make sure we have the right parameters
  stopifnot(
    is.data.frame(genome),
    "allele" %in% colnames(genome),
    "locus" %in% colnames(genome),
    "value" %in% colnames(genome),
    nrow(genome) > 0,
    is.numeric(ploidy_benefit),
    between(ploidy_benefit, 0, 1),
    is.numeric(loci),
    is.numeric(max_rate)
  )
  # work out the genome size
  genome_size <- nlevels(as.factor(genome$locus))
  # now we have the genome make sure loci parameter is appropriate
  stopifnot(
    between(loci, 0, genome_size)
  )
  # get all the loci values that contribute to growth rate
  growth_rate_loci <- genome %>%
    filter(locus %in% loci) %>%
    pull(value)

  # see how many alelles we have
  n <- length(growth_rate_loci)

  # Determine trait value for ploidy_benefit = 0
  # --------------------------------------------
  # (the average value across the genome determines growth rate)
  # seeing as max_trait_val = min_trait_val * n
  # min_trait_val is also equal to 1/n of the max_trait_val
  min_trait_val <- mean(growth_rate_loci)

  # Get the number of n used to find max_trait_val - min_trait_val
  # --------------------------------------------------------------
  # so, if n = 4, then we need 3/4 of max_trait_val, ie: 3
  # Or, if n = 8, then we need 7/8 of of max_trait_val, ie: 7
  numerator <- n - 1

  # Determine trait value for all ploidy_benefit scenarios
  # -------------------------------------------------------------
  # the product of the maths contained in the brackets will range
  # from 0 to numerator/n of the max_trait_val depending on ploidy_benefit.
  # adding to 1/n of the max_trait_val (ie: min_trait_val) produces a
  # trait value which is properly adjusted by the ploidy_benefit constant.
  trait_val <- min_trait_val + (min_trait_val * numerator * ploidy_benefit)

  # Return trait value as rate
  # ---------------------------
  # work out max trait value possible
  # (allele values are constrained between 0 and 100)
  ploidy_lvl <- nlevels(as.factor(genome$allele))
  max_trait_val <- n * 100
  # do conversion (with rate constrained to max_rate)
  growth_rate <- exp((log(max_rate) / max_trait_val) * trait_val)
  # return rate
  return(growth_rate)
}

#' @name get_inbreeding_value
#' @title get_inbreeding_values
#' @usage Uses all alleles at the requested locus to determine whether or not a fiteness disadvantage due to inbreeding should be applied.
#' @authors Rose McKeon and Brad Duthie
#' @param genome A dataframe contining the genome of an individual
#' @param inbreeding_locus a positive integer which represents the locus used to detect inbreeding (default = 2).
#' @export
get_inbreeding_value <- function(genome, inbreeding_locus = 2){
  # make sure we have the right parameters
  stopifnot(
    is.data.frame(genome),
    "allele" %in% colnames(genome),
    "locus" %in% colnames(genome),
    "value" %in% colnames(genome),
    nrow(genome) >= 2,
    is.numeric(inbreeding_locus),
    inbreeding_locus%%1==0
  )

  # get all the allele values at the locus
  values <- genome %>%
    filter(locus == inbreeding_locus) %>%
    pull(value)

  # see if we have all matching values
  if(nlevels(as.factor(values)) > 1){
    return(FALSE)
  } else {
    return(TRUE)
  }
}
