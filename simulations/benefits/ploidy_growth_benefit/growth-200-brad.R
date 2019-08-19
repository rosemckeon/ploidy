rm(list=ls())
library(tidyverse)
library(devtools)
library(tictoc)
library(disturploidy)

name <- "growth-benefit"
generations <- 200
simulations <- 1
runs <- 10

# saves a new data file for every run which contains
# the number of simulations specified. A separate
# logfile is stored for each simulation.
for(run in runs){

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
    # turn on polyploids and give them a growth benefit
    ploidy_prob = 0.01,
    ploidy_growth_benefit = 0.5,
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
    # turn on polyploids and give them a growth benefit
    ploidy_prob = 0.01,
    ploidy_growth_benefit = 1,
    # and add disturbance
    disturbance_freq = 10
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
    # turn on polyploids and give them a growth benefit
    ploidy_prob = 0.01,
    ploidy_growth_benefit = 0.5,
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
    # turn on polyploids and give them a growth benefit
    ploidy_prob = 0.01,
    ploidy_growth_benefit = 1,
    # and add disturbance
    disturbance_freq = 25
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
    # turn on polyploids and give them a growth benefit
    ploidy_prob = 0.01,
    ploidy_growth_benefit = 0.5,
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
    # turn on polyploids and give them a growth benefit
    ploidy_prob = 0.01,
    ploidy_growth_benefit = 1,
    # and add disturbance
    disturbance_freq = 50
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
    # turn on polyploids and give them a growth benefit
    ploidy_prob = 0.01,
    ploidy_growth_benefit = 0.5,
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
    # turn on polyploids and give them a growth benefit
    ploidy_prob = 0.01,
    ploidy_growth_benefit = 1,
    # and add disturbance
    disturbance_freq = 100
  )

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
    # turn on polyploids and give them a growth benefit
    ploidy_prob = 0.01,
    ploidy_growth_benefit = 0.5
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
    # turn on polyploids and give them a growth benefit
    ploidy_prob = 0.01,
    ploidy_growth_benefit = 1
  )

}
