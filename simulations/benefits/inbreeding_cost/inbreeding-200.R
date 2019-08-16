rm(list=ls())
library(tidyverse)
library(devtools)
library(tictoc)
library(disturploidy)

name <- "inbreeding"
generations <- 200
simulations <- 1
runs <- 1:3

# saves a new data file for every run which contains
# the number of simulations specified. A separate
# logfile is stored for each simulation.
for(run in runs){
  # include a null result
  this_run <- paste0(name, "_0.0_disturbance_000_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run
  )
  # then vary the benefit
  this_run <- paste0(name, "_0.5_disturbance_000_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them inbreeding tolerance
    ploidy_prob = 0.01,
    inbreeding_cost = 0.5
  )
  this_run <- paste0(name, "_1.0_disturbance_000_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them inbreeding tolerance
    ploidy_prob = 0.01,
    inbreeding_cost = 1
  )

  # then repeat all 3 with varied disturbance
  # 100
  this_run <- paste0(name, "_0.0_disturbance_100_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # add disturbance
    disturbance_freq = 100
  )
  # then vary the benefit
  this_run <- paste0(name, "_0.5_disturbance_100_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them inbreeding tolerance
    ploidy_prob = 0.01,
    inbreeding_cost = 0.5,
    # and add disturbance
    disturbance_freq = 100
  )
  this_run <- paste0(name, "_1.0_disturbance_100_", run)
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them inbreeding tolerance
    ploidy_prob = 0.01,
    inbreeding_cost = 1,
    # and add disturbance
    disturbance_freq = 100
  )

  # 50
  this_run <- paste0(name, "_0.0_disturbance_050_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # add disturbance
    disturbance_freq = 50
  )
  # then vary the benefit
  this_run <- paste0(name, "_0.5_disturbance_050_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them inbreeding tolerance
    ploidy_prob = 0.01,
    inbreeding_cost = 0.5,
    # and add disturbance
    disturbance_freq = 50
  )
  this_run <- paste0(name, "_1.0_disturbance_050_", run)
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them inbreeding tolerance
    ploidy_prob = 0.01,
    inbreeding_cost = 1,
    # and add disturbance
    disturbance_freq = 50
  )

  # 25
  this_run <- paste0(name, "_0.0_disturbance_025_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # add disturbance
    disturbance_freq = 25
  )
  # then vary the benefit
  this_run <- paste0(name, "_0.5_disturbance_025_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them inbreeding tolerance
    ploidy_prob = 0.01,
    inbreeding_cost = 0.5,
    # and add disturbance
    disturbance_freq = 25
  )
  this_run <- paste0(name, "_1.0_disturbance_025_", run)
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them inbreeding tolerance
    ploidy_prob = 0.01,
    inbreeding_cost = 1,
    # and add disturbance
    disturbance_freq = 25
  )

  # 10
  this_run <- paste0(name, "_0.0_disturbance_010_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # add disturbance
    disturbance_freq = 10
  )
  # then vary the benefit
  this_run <- paste0(name, "_0.5_disturbance_010_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them inbreeding tolerance
    ploidy_prob = 0.01,
    inbreeding_cost = 0.5,
    # and add disturbance
    disturbance_freq = 10
  )
  this_run <- paste0(name, "_1.0_disturbance_010_", run)
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them inbreeding tolerance
    ploidy_prob = 0.01,
    inbreeding_cost = 1,
    # and add disturbance
    disturbance_freq = 10
  )
}
