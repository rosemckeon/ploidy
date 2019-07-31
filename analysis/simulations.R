rm(list=ls())
library(tidyverse)
library(devtools)
library(tictoc)
# library(disturploidy)
source("../R/disturploidy.R")
source("../R/functions.R")
source("../R/traits.R")

# Saving lots of output...
name <- "null"
generations <- 200
simulations <- 4
runs <- 2

for(run in runs){
  # save a new object for every simulation
  this_run <- paste0(name, "-", run)
  disturploidy(
    pop_size = 400,
    grid_size = 40,
    juvenile_selection_constant = .1,
    adult_survival_prob = .7,
    inbreeding_sensitivity = 0,
    germination_prob = .5,
    seed_survival_prob = 0,
    ploidy_growth_benefit = 0,
    N_ovules = 25,
    pollen_range = 40,
    fertilisation_prob = .5,
    uneven_matching_prob = .5,
    selfing_diploid_prob = 0,
    selfing_polyploid_prob = 0,
    triploid_mum_prob = .5,
    disturbance_freq = 1000,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run
  )
}
