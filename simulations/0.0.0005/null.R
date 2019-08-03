rm(list=ls())
library(tidyverse)
library(devtools)
library(tictoc)
library(disturploidy)
# or source the scripts from this dir as wd
# source("../R/disturploidy.R")
# source("../R/functions.R")
# source("../R/traits.R")

name <- "null"
generations <- 200
simulations <- 4
runs <- 1:10

# saves a new data file for every run which contains
# the number of simulations specified. A separate
# logfile is stored for each simulation.
for(run in runs){
  this_run <- paste0(name, "-", run)
  disturploidy(
    pop_size = 4000,
    grid_size = 40,
    juvenile_selection_constant = .1,
    adult_survival_prob = .7,
    inbreeding_sensitivity = 0,
    germination_prob = .5,
    seed_survival_prob = 0,
    ploidy_prob = 0,
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
