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
  # BD: Got this -- so for the default, loci 1 and 2 affect growth
  growth_rate_loci <- genome %>%
    filter(locus %in% loci) %>%
    pull(value)

  # work out ploidy level
  # BD: Looks good
  ploidy_lvl <- length(growth_rate_loci)

  # make sure we can't have shrinking
  # BD: I'm less clear about this part, or how it prevents shrinking
  min_rate <- mean(growth_rate_loci)

  # create a range of growth rates
  # (these vector keys now correspond to ploidy_benefit)
  # BD: Okay, I'm starting to get the idea now. So `min_rate` is the situation
  #     in which the average value across the genome determines growth rate,
  #     hence giving no benefit to polyploids, whereas the `max_rate` gives an
  #     opposite extreme in which the summation affects growth rate (so more
  #     alleles, faster growth).
  # BD: Since max_rate = min_rate * ploidy_lvl, it might be easier to set the
  #     value `ploidy_benefit` to tweak `growth_rates`. I've suggested the code
  #     below, but feel free to change if it doesn't make any sense or you don't
  #     like it quite as much!
  adjusted_lvl <- ((ploidy_lvl - 1) / ploidy_lvl) * ploidy_lvl;
  # BD: The code above tweaks the ploidy_lvl to accomodate the baseline mean
  #     So if there are 4 loci, then 3/4 of the mean is returned
  #     If there are 7 loci, then 6/7 of the mean is returned, etc.
  growth_rates <- min_rate + (min_rate * adjusted_lvl * ploidy_benefit);
  # BD: Now that 1/4 or 1/7 is added back as `min_rate`, plus up to 3/4 or 6/7
  #     of the max, giving a total possible maximum of your previous `max_rate`
  #     Hence, now `ploidy_benefit` can take a range from 0 (giving `min_rate`)
  #     to 1 (giving `max_rate`), all by just adjusting the one parameter value


  # convert to rates
  # (not really sure what I'm doing here)
  # I'm trying to make sure we have reasonable
  # rate values > 1 but not by too much.
  # I've used .001 as it gives a straight line
  # if you qplot(0:100, exp(.001 * growth_rates))
  # BD: I don't think that there is anything wrong with the below to increase
  #     growth rate, but it might still get out of hand if there is no maximum
  #     on `value` in the genome. If the value maximum is 100, then this should
  #     cap things nicely.
  growth_rates <- exp(.001 * growth_rates)
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

  # BD: I removed the growth rate return (no longer needed?).

  # make sure we can't have shrinking # BD: Might not be entirely unrealistic?
  # not sure this is needed now? # Maybe remove this for now?
  if(growth_rate > 1){
    return(growth_rate)
  } else {
    # will monitor with output messages
    message("  Warning: Growth rate was < 1 and had to be adjusted.")
    return(1)
  }
}
