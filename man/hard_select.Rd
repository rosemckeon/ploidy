% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{hard_select}
\alias{hard_select}
\title{hard_select}
\usage{
hard_select(pop, trait, Z = 0.001, inbreeding_sensitivity = 0)
}
\arguments{
\item{pop}{A population dataframe}

\item{trait}{A character string matching a column name which contains trait values.}

\item{Z}{a constant used to turn trait values into probabilities.}

\item{inbreeding_sensitivity}{number between 0 and 1 representing the strength of inbreeding. 0 = no effect and 1 is maximum effect. Checking for identical alleles at the specified inbreeding locus is used as a proxy for having homozygous deleterious alleles. When this happens survival chances are modified according to inbreeding sensitivity (default = 0, so inbreeding is ignored unless set).}
}
\value{
pop reduced based on survival probabilities created from trait values.
}
\description{
hard selection based on trait values. Large trait values give higher survival probabilities.
}
\author{
Rose McKeon
}
