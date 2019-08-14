rm(list=ls())
library(tidyverse)
library(devtools)
library(tictoc)
# library(disturploidy)
# or source the scripts from this dir as wd
source("R/disturploidy.R")
source("R/functions.R")
source("R/traits.R")

name <- "quick-test"
generations <- 5
simulations <- 1
runs <- 2

# saves a new data file for every run which contains
# the number of simulations specified. A separate
# logfile is stored for each simulation.
for(run in runs){
  this_run <- paste0(name, "-", run)
  disturploidy(
    pop_size = 500,
    grid_size = 10,
    juvenile_selection_constant = .5,
    adult_survival_prob = .7,
    inbreeding_cost = 0,
    germination_prob = .4,
    seed_survival_prob = 0,
    ploidy_prob = .5,
    ploidy_growth_benefit = 0,
    adult_size = 1.25,
    N_ovules = 25,
    pollen_range = 10,
    fertilisation_prob = .75,
    uneven_matching_prob = .75,
    selfing_diploid_prob = 0,
    selfing_polyploid_prob = 0,
    triploid_mum_prob = .75,
    seed_dispersal_range = 9,
    disturbance_freq = 2,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    filepath = "simulations/data/",
    logfilename = this_run,
    logfilepath = "simulations/data/logs/"
  )
}
