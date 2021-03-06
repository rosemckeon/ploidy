% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traits.R
\name{get_growth_rate}
\alias{get_growth_rate}
\title{get_growth_rate}
\usage{
Uses all alleles at loci to determine growth rate with minimum value of 1.
}
\arguments{
\item{genome}{A dataframe contining the genome of an individual}

\item{ploidy_benefit}{A number between 0 and 1 that represents the proportion by which being polyploid improves growth rate (default = 1, maximum benefit).}

\item{loci}{a numeric vector containing positive integer/s (eg: 1 or 1:5) which represents the locus/loci to use for the trait growth rate (default = 1).}

\item{max_rate}{A number representing the maximum rate which can be output no matter the genes (default = 2, so individuals can never more than double in size in a generation).}
}
\value{
number greater than or equal to 1 used to multiply size in grow("individuals").
}
\description{
get_growth_rate
}
\author{
Rose McKeon and Brad Duthie
}
