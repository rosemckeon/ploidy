rm(list=ls())
library(tidyverse)
library(devtools)
library(tictoc)
# library(disturploidy)
# or source the scripts from this dir as wd
source("../R/disturploidy.R")
source("../R/functions.R")
source("../R/traits.R")

name <- "null-200"
generations <- 200
simulations <- 1
runs <- 2

# saves a new data file for every run which contains
# the number of simulations specified. A separate
# logfile is stored for each simulation.
for(run in runs){
  this_run <- paste0(name, "-", run)
  disturploidy(
    pop_size = 1000,
    grid_size = 40,
    juvenile_selection_constant = .1,
    adult_survival_prob = .5,
    inbreeding_cost = 0,
    germination_prob = .4,
    seed_survival_prob = 0,
    ploidy_prob = 0,
    adult_size = 1.3,
    ploidy_growth_benefit = 0,
    max_growth_rate = 4,
    N_ovules = 25,
    pollen_range = 40,
    fertilisation_prob = .75,
    uneven_matching_prob = .75,
    selfing_diploid_prob = .75,
    selfing_polyploid_prob = .75,
    triploid_mum_prob = .75,
    seed_dispersal_range = 39,
    disturbance_freq = 0,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run
  )
}
