rm(list=ls())
library(tidyverse)
library(devtools)
library(tictoc)
library(disturploidy)

name <- "triploid-sterility"
generations <- 200
simulations <- 1
runs <- 1:3

# saves a new data file for every run which contains
# the number of simulations specified. A separate
# logfile is stored for each simulation.
for(run in runs){
  # include a null result
  # default triploid mum prob prob same as fertilisation prob
  this_run <- paste0(
    name, "_0.750_disturbance_000_", run
  )
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids but don't give them a cost
    ploidy_prob = 0.01
  )
  # then turn on the cost and vary it
  this_run <- paste0(
    name, "_0.375_disturbance_000_", run
  )
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them a cost
    ploidy_prob = 0.01,
    triploid_mum_prob = .375
  )
  this_run <- paste0(
    name, "_0.000_disturbance_000_", run
  )
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them a cost
    ploidy_prob = 0.01,
    triploid_mum_prob = 0
  )

  # then repeat all with varied disturbance
  # 100 -------------------------------------------
  this_run <- paste0(
    name, "_0.750_disturbance_100_", run
  )
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids but don't give them a cost
    ploidy_prob = 0.01,
    # and add disturbance
    disturbance_freq = 100
  )
  # then reduce fecundity
  this_run <- paste0(
    name, "_0.375_disturbance_100_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them a cost
    ploidy_prob = 0.01,
    triploid_mum_prob = .375,
    # and add disturbance
    disturbance_freq = 100
  )
  this_run <- paste0(
    name, "_0.000_disturbance_100_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them a cost
    ploidy_prob = 0.01,
    triploid_mum_prob = 0,
    # and add disturbance
    disturbance_freq = 100
  )

  # 50 -------------------------------------------
  this_run <- paste0(
    name, "_0.750_disturbance_050_", run
  )
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids but don't give them a cost
    ploidy_prob = 0.01,
    # and add disturbance
    disturbance_freq = 50
  )
  # then reduce fecundity
  this_run <- paste0(
    name, "_0.375_disturbance_050_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them a cost
    ploidy_prob = 0.01,
    triploid_mum_prob = .375,
    # and add disturbance
    disturbance_freq = 50
  )
  this_run <- paste0(
    name, "_0.000_disturbance_050_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them a cost
    ploidy_prob = 0.01,
    triploid_mum_prob = 0,
    # and add disturbance
    disturbance_freq = 50
  )

  # 25 -------------------------------------------
  this_run <- paste0(
    name, "_0.750_disturbance_025_", run
  )
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids but don't give them a cost
    ploidy_prob = 0.01,
    # and add disturbance
    disturbance_freq = 25
  )
  # then reduce fecundity
  this_run <- paste0(
    name, "_0.375_disturbance_025_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them a cost
    ploidy_prob = 0.01,
    triploid_mum_prob = .375,
    # and add disturbance
    disturbance_freq = 25
  )
  this_run <- paste0(
    name, "_0.000_disturbance_025_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them a cost
    ploidy_prob = 0.01,
    triploid_mum_prob = 0,
    # and add disturbance
    disturbance_freq = 25
  )

  # 10 -------------------------------------------
  this_run <- paste0(
    name, "_0.750_disturbance_010_", run
  )
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids but don't give them a cost
    ploidy_prob = 0.01,
    # and add disturbance
    disturbance_freq = 10
  )
  # then reduce fecundity
  this_run <- paste0(
    name, "_0.375_disturbance_010_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them a cost
    ploidy_prob = 0.01,
    triploid_mum_prob = .375,
    # and add disturbance
    disturbance_freq = 10
  )
  this_run <- paste0(
    name, "_0.000_disturbance_010_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them a cost
    ploidy_prob = 0.01,
    triploid_mum_prob = 0,
    # and add disturbance
    disturbance_freq = 10
  )

}