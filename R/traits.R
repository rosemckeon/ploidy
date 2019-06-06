#' @name get_growth_rate
#' @details Uses all alleles at loci to determine growth rate with minimum value of 1.
#' @author Rose McKeon
#' @param genome A dataframe contining the genome of an individual
#' @param loci the loci to use for this trait.
#' @return number greater than or equal to 1 used to multiply size in grow("individuals").
get_growth_rate <- function(genome, loci = 1:10){
  # calculate growth rate
  growth_rate <- genome %>%
    filter(locus %in% loci) %>%
    pull(value) %>%
    sum() / 100
  # make sure we can't have shrinking
  if(growth_rate > 1){
    return(growth_rate)
  } else {
    return(1)
  }
}
