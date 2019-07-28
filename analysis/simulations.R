rm(list=ls())
#library(DisturPloidy)
library(tidyverse)
library(devtools)
library(tictoc)
source("../R/disturploidy.R")
source("../R/functions.R")
source("../R/traits.R")

# Saving lots of output...
name <- "null"
generations <- 200
simulations <- 4
runs <- 2:4

for(run in runs){
  # save a new object for every simulation
  this_run <- paste0(name, "-", run)
  disturploidy(
    pop_size = 500,
    grid_size = 40,
    N_ovules = 25,
    pollen_range = 40,
    seed_survival_prob = .534,
    germination_prob = .5,
    ploidy_growth_benefit = 0,
    inbreeding_sensitivity = 0,
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
