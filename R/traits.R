#' @name get_growth_rate
#' @details Uses all alleles at loci to determine growth rate with minimum value of 1.
#' @author Rose McKeon
#' @param genome A dataframe contining the genome of an individual
#' @param loci the loci to use for this trait.
#' @param ploidy_benefit A number between 0 and 1 that represents the proportion by which being polyploid improves growth rate.
#' @return number greater than or equal to 1 used to multiply size in grow("individuals").
get_growth_rate <- function(genome, loci = 1:2, ploidy_benefit = 1){
  # make sure we have the right parameters
  stopifnot(
    is.data.frame(genome),
    "allele" %in% colnames(genome),
    "locus" %in% colnames(genome),
    "value" %in% colnames(genome),
    nrow(genome) < 0,
    is.numeric(loci),
    is.numeric(ploidy_benefit),
    between(ploidy_benefit, 0, 1)
  )
  # get all the loci values that contribute to growth rate
  growth_rate_loci <- genome %>%
    filter(locus %in% loci) %>%
    pull(value)

  # see how many alelles we have
  n <- length(growth_rate_loci)

  # determine trait value for ploidy_benefit = 0
  # (the average value across the genome determines growth rate)
  # seeing as max_trait_val = min_trait_val * n
  # min_trait_val is also equal to 1/n of the max_trait_val
  min_trait_val <- mean(growth_rate_loci)

  # get the number of n used to find max_trait_val - min_trait_val
  # so, if n = 4, then we need 3/4 of max_trait_val, ie: 3
  # Or, if n = 8, then we need 7/8 of of max_trait_val, ie: 7
  numerator <- ((n - 1) / n) * n

  # determine trait value for all other ploidy_benefit scenarios
  # the product of the maths contained in the brackets will range
  # from 0 to the full difference between max and min trait vals
  # depending on ploidy_benefit.
  trait_val <- min_trait_val + (min_trait_val * numerator * ploidy_benefit)

  # convert trait value to rate
  # BD: I don't think that there is anything wrong with the below to increase
  #     growth rate, but it might still get out of hand if there is no maximum
  #     on `value` in the genome. If the value maximum is 100, then this should
  #     cap things nicely.
  growth_rate <- exp(.001 * trait_val)
  # BD: I think it would be good to somehow avoid the hard code of `0.001`, and
  #     instead do some quick calculation to allow for a maximum growth rate of
  #     a diploid population (i.e., if all alleles at all loci had a value of
  #     100). If, e.g., we want the maximum possible diploid growth rate to be
  #     2, then we can do the following (growth_rates = 2000, x is unknown):
  #
  #     2     = e^(2000 * x)
  #     ln(2) = 2000 * x
  #     x     = ln(2) / 2000 = 0.0003465736
  #
  #     So the code could calculate the above if a parameter `max_growth_rate`
  #     was set to 2, solving for x to flexibly replace the 0.001 above.
  #     Let me know if you need some help coding this.

  return(growth_rate)
}
