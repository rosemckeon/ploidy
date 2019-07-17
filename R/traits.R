#' @name get_growth_rate
#' @details Uses all alleles at loci to determine growth rate with minimum value of 1.
#' @author Rose McKeon
#' @param genome A dataframe contining the genome of an individual
#' @param loci a numeric vector of integers over 0 (eg 1:5) which represent the loci to use for this trait (default = 1:genome_size).
#' @param ploidy_benefit A number between 0 and 1 that represents the proportion by which being polyploid improves growth rate.
#' @param max_rate A number representing the maximum rate which can be output no matter the genes (default = 2, so individuals can never more than double in size in a generation).
#' @return number greater than or equal to 1 used to multiply size in grow("individuals").
get_growth_rate <- function(genome, ploidy_benefit = 1, loci = NULL, max_rate = 2){
  # make sure we have the right parameters
  stopifnot(
    is.data.frame(genome),
    "allele" %in% colnames(genome),
    "locus" %in% colnames(genome),
    "value" %in% colnames(genome),
    nrow(genome) > 0,
    is.numeric(ploidy_benefit),
    between(ploidy_benefit, 0, 1)
  )
  # set loci defaults
  genome_size <- nlevels(genome$locus)
  if(is.null(loci)){
    loci <- 1:genome_size
  } else {
    stopifnot(
      is.numeric(loci),
      max(loci) <= genome_size,
      min(loci) > 0
    )
  }
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
  numerator <- ((n - 1) / n) * n

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
  # (if allele values are constrained between 1 and 100)
  ploidy_lvl <- nlevels(genome$allele)
  max_trait_val <- ploidy_lvl * genome_size * 100
  # do conversion (with rate constrained to max_rate)
  growth_rate <- exp((log(max_rate) / max_trait_val) * trait_val)
  # return rate
  return(growth_rate)
}
