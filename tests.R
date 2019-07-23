#' ---
#' title: "Testing disturploidy()"
#' author: "Rose McKeon"
#' date: "June 10th 2019"
#' ---

#' This script tests:
#'
#' - Selection on growth rate
#' - Genome duplication
#'
#' Using: `sim <- disturploidy()`
#'

#+ setup, message=F, warning=F, include=F --------
# clear the workspace
rm(list=ls())
# load dependencies
library(tidyverse)
library(ggplot2)
library(tictoc)
source("R/init.R")
source("R/functions.R")
source("R/traits.R")
# simulation
disturploidy(
  pollen_range = 10,
  generations = 2,
  simulations = 2
)
# load the results
use(plants)

# check the structure
str(plants, max.level = 1)

#' \pagebreak
# Do some counting
plants %>%
  group_by(sim, gen) %>%
  summarise(N = n())

plants %>%
  group_by(sim, gen, life_stage) %>%
  summarise(N = n())

plants %>%
  group_by(sim, gen, ploidy) %>%
  summarise(N = n())

plants %>%
  group_by(sim, gen, ID) %>%
  summarise(ramets = n())


#+ plots, warning=F -----------------------------
#' \pagebreak
# quick plot selection
# looks less clear since distrubance added
qplot(
  gen,
  growth_rate,
  data = plants,
  geom = "jitter"
) + geom_smooth(
  method = "lm"
) + scale_y_continuous(
  breaks = c(1, 1.5, 2),
  limits = c(1, 2)
) + theme_classic()

#' \pagebreak
# quick plot ploidy levels and pop growth/decline
qplot(
  gen,
  ploidy,
  data = plants,
  geom = "jitter"
) + scale_y_continuous(
  breaks = c(2, 3, 4),
  limits = c(2, 4)
) + theme_classic()

#' \pagebreak
# how to plot the landscape
qplot(
  X - 1.5,
  Y - 1.5,
  data = sim %>% filter(gen == 0, sim == 1),
  geom = "point"
) + scale_x_continuous(
  breaks = seq(0, 100, by = 10),
  limits = c(0, 100),
  minor_breaks = 0:100
) + scale_y_continuous(
  breaks = seq(0, 100, by = 10),
  limits = c(0, 100),
  minor_breaks = 0:100
) + theme_classic() + theme(
  panel.grid.major = element_line(
    size = .5,
    linetype = "solid",
    colour = "gray"
  ),
  panel.grid.minor = element_line(
    size = .25,
    linetype = "solid",
    colour = "gray"
  )
)
