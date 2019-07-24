#' @name get_growth_rate
#' @details Uses all alleles at loci to determine growth rate with minimum value of 1.
#' @author Rose McKeon
#' @param genome A dataframe contining the genome of an individual
#' @param loci a numeric vector of positive integers (eg: 1:5) which represent the loci to use for the trait growth rate (default = NULL, which forces the simulation to use the first half of the genome to calculate this trait).
#' @param ploidy_benefit A number between 0 and 1 that represents the proportion by which being polyploid improves growth rate (default = 1, maximum benefit).
#' @param max_rate A number representing the maximum rate which can be output no matter the genes (default = 2, so individuals can never more than double in size in a generation).
#' @return number greater than or equal to 1 used to multiply size in grow("individuals").
get_growth_rate <- function(
  genome,
  ploidy_benefit = 1,
  loci = NULL,
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
    between(ploidy_benefit, 0, 1)
  )
  # work out the genome size
  genome_size <- nlevels(as.factor(genome$locus))
  # make sure it's even
  stopifnot(
    (genome_size / 2)%%1==0
  )
  # set loci defaults
  if(is.null(loci)){
    # use half the genome
    loci <- 1:(genome_size / 2)
  } else {
    # or the specified loci
    stopifnot(
      is.numeric(loci),
      between(loci, 0, genome_size)
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
  max_trait_val <- ploidy_lvl * genome_size * 100
  # do conversion (with rate constrained to max_rate)
  growth_rate <- exp((log(max_rate) / max_trait_val) * trait_val)
  # return rate
  return(growth_rate)
}

#' @name get_inbreeding_value
#' @details Uses all alleles at requested loci to determine how inbred an individual is and returns a proportion representing how inbred an individual is.
#' @author Rose McKeon
#' @param genome A dataframe contining the genome of an individual
#' @param loci a numeric vector of integers over 0 (eg 1:5) which represent the loci to use for the trait inbreeding (default = NULL, which forces the simulation to use the second half on the genome ot calculate this trait).
#' @return number between 0 and 1 representing proportion of matching alleles which have the most duplicates.
get_inbreeding_value <- function(genome, loci = NULL){
  # make sure we have the right parameters
  stopifnot(
    is.data.frame(genome),
    "allele" %in% colnames(genome),
    "locus" %in% colnames(genome),
    "value" %in% colnames(genome),
    nrow(genome) > 0
  )
  # set loci defaults
  genome_size <- nlevels(as.factor(genome$locus))
  # make sure it's even
  stopifnot(
    (genome_size / 2)%%1==0
  )
  # set loci defaults
  if(is.null(loci)){
    # use half the genome
    loci <- (genome_size / 2):genome_size
  } else {
    stopifnot(
      is.numeric(loci),
      between(loci, 0, genome_size)
    )
  }

  # get all the loci values that we will check for inbreeding
  inbreeding_loci <- genome %>%
    filter(locus %in% loci) %>%
    pull(value)

  # see how many alelles we have
  n <- length(inbreeding_loci)

  # check for matching values
  max_identical <- max(as.numeric(table(inbreeding_loci)))

  # return inbreeding value as a proportion
  return(n/max_identical)
}
