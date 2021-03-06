% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{sample_genome}
\alias{sample_genome}
\title{sample_genome}
\usage{
sample_genome(seed, parents = NULL, genome = NULL, genome_size = 10)
}
\arguments{
\item{seed}{vector of seed data with mum and dad columns nested in genome list-column.}

\item{parents}{population dataframe containing all possible parents.}

\item{genome}{Default genome as output by create_genome().}
}
\value{
genome with value filled by sampling parent genomes, defined by parent IDs in seed.
}
\description{
Takes a vector of seed data that has parent IDs in the genome instead of alleles, and returns a genome of alleles which has been sampled from both parents.
}
\author{
Rose McKeon
}
