rm(list=ls())
library(tidyverse)
library(devtools)
library(tictoc)
library(disturploidy)

name <- "null-200"
generations <- 200
simulations <- 1
runs <- 1:10

# saves a new data file for every run which contains
# the number of simulations specified. A separate
# logfile is stored for each simulation.
for(run in runs){
  this_run <- paste0(name, "-", run)
  disturploidy(
    pop_size = 1000,
    grid_size = 40,
    pollen_range = 39,
    seed_dispersal_range = 39,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run
  )
}
