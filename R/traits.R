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
    is.numeric(loci),
    is.numeric(ploidy_benefit),
    between(ploidy_benefit, 0, 1)
  )
  # get all the loci values that contribute to growth rate
  growth_rate_loci <- genome %>%
    filter(locus %in% loci) %>%
    pull(value)

  # work out ploidy level
  ploidy_lvl <- length(growth_rate_loci)

  # make sure we can't have shrinking
  min_rate <- mean(growth_rate_loci)
  max_rate <- sum(growth_rate_loci)

  # create a range of growth rates
  # (these vector keys now correspond to ploidy_benefit)
  growth_rates <- seq.int(
    min_rate, # zero benefit for polyploids
    max_rate, # maximum benefit for polyploids
    length.out = 101
    )

  # convert to rates
  # (not really sure what I'm doing here)
  # I'm trying to make sure we have reasonable
  # rate values > 1 but not by too much.
  # I've used .001 as it gives a straight line
  # if you qplot(0:100, exp(.001 * growth_rates))
  growth_rates <- exp(.001 * growth_rates)

  # choose growth rate to return
  growth_rate <- growth_rates[1 + ploidy_benefit * 100]

  # make sure we can't have shrinking
  # not sure this is needed now?
  if(growth_rate > 1){
    return(growth_rate)
  } else {
    # will monitor with output messages
    message("  Warning: Growth rate was < 1 and had to be adjusted.")
    return(1)
  }
}
