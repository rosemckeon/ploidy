rm(list=ls())
library(tidyverse)
library(devtools)
library(tictoc)
library(disturploidy)

name <- "selfing"
generations <- 200
simulations <- 1
runs <- 1:3

# saves a new data file for every run which contains
# the number of simulations specified. A separate
# logfile is stored for each simulation.
for(run in runs){
  # include a null result
  this_run <- paste0(name, "_0.0_disturbance_000_pollen_29_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids but don't give them ability to self
    ploidy_prob = 0.01
  )
  # then turn on selfing
  this_run <- paste0(name, "_1.0_disturbance_000_pollen_29_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1
  )

  # then repeat both with varied disturbance
  # AND varied pollen range
  # 100
  this_run <- paste0(name, "_0.0_disturbance_100_pollen_29_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids but don't give them ability to self
    ploidy_prob = 0.01,
    # and add disturbance
    disturbance_freq = 100
  )
  # then turn on selfing
  this_run <- paste0(name, "_1.0_disturbance_100_pollen_29_", run)
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 100
  )
  # then reduce mate-choice
  this_run <- paste0(
    name, "_1.0_disturbance_100_pollen_19_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    # reduce mate-choice
    pollen_range = 19,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 100
  )
  this_run <- paste0(
    name, "_1.0_disturbance_100_pollen_09_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    # reduce mate-choice
    pollen_range = 9,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 100
  )

  # 50
  this_run <- paste0(name, "_0.0_disturbance_050_pollen_29_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids but don't give them ability to self
    ploidy_prob = 0.01,
    # add disturbance
    disturbance_freq = 50
  )
  # then turn on selfing
  this_run <- paste0(name, "_1.0_disturbance_050_pollen_29_", run)
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 50
  )
  # then reduce mate-coice
  this_run <- paste0(
    name, "_1.0_disturbance_050_pollen_19_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    # reduce mate-choice
    pollen_range = 19,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 50
  )
  this_run <- paste0(
    name, "_1.0_disturbance_050_pollen_09_", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    # reduce mate-choice
    pollen_range = 9,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 50
  )

  # 25
  this_run <- paste0(name, "_0.0_disturbance_025_pollen_29_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids but don't give them ability to self
    ploidy_prob = 0.01,
    # add disturbance
    disturbance_freq = 25
  )
  # then turn on selfing
  this_run <- paste0(name, "_1.0_disturbance_025_pollen_29", run)
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 25
  )
  # then redce mate-choice
  this_run <- paste0(
    name, "_1.0_disturbance_025_pollen_19", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    # reduce mate-choice
    pollen_range = 19,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 25
  )
  this_run <- paste0(
    name, "_1.0_disturbance_025_pollen_09", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    # reduce mate-choice
    pollen_range = 9,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 25
  )


  # 10
  this_run <- paste0(name, "_0.0_disturbance_010_pollen_29_", run)
  disturploidy(
    pop_size = 750,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids but don't give them ability to self
    ploidy_prob = 0.01,
    # add disturbance
    disturbance_freq = 10
  )
  # then turn on selfing
  this_run <- paste0(name, "_1.0_disturbance_010_pollen_29", run)
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    pollen_range = 29,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 10
  )
  # then reduce mate-choice
  this_run <- paste0(
    name, "_1.0_disturbance_010_pollen_19", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    # reduce mate-choice
    pollen_range = 19,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 10
  )
  this_run <- paste0(
    name, "_1.0_disturbance_010_pollen_9", run
  )
  disturploidy(
    pop_size = 7500,
    grid_size = 30,
    # reduce mate-choice
    pollen_range = 9,
    seed_dispersal_range = 29,
    generations = generations,
    simulations = simulations,
    filename = this_run,
    logfilename = this_run,
    # turn on polyploids and give them ability to self
    ploidy_prob = 0.01,
    selfing_polyploid_prob = 1,
    # and add disturbance
    disturbance_freq = 10
  )
}
