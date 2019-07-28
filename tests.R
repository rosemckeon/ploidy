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
source("R/disturploidy.R")
source("R/functions.R")
source("R/traits.R")
# simulation
disturploidy(
  pop_size = 500,
  grid_size = 40,
  N_ovules = 25,
  pollen_range = 40,
  seed_survival_prob = .535,
  germination_prob = .5,
  ploidy_growth_benefit = 0,
  inbreeding_sensitivity = 0,
  fertilisation_prob = .5,
  uneven_matching_prob = .5,
  selfing_diploid_prob = 0,
  selfing_polyploid_prob = 0,
  triploid_mum_prob = .5,
  generations = 100,
  disturbance_freq = 1000,
  simulations = 4
)
# load the results
data(plants)

# check the structure
str(plants, max.level = 1)

#' \pagebreak
#' Doing some counting
# pop size over time
pop_sizes <- plants %>%
  group_by(sim, gen) %>%
  tally()

# Max pop size in a generation
max(pop_sizes$n)

# life stages over time
life_stage_pop_sizes <- plants %>%
  group_by(sim, gen, life_stage) %>%
  tally()

# when do we get booms of adults?
life_stage_pop_sizes %>% filter(life_stage == 2)

# pop size over time organised by ploidy
ploidy_pop_sizes <- plants %>%
  group_by(sim, gen, ploidy) %>%
  tally()

# genet size over time
# genet_sizes <- plants %>%
#   group_by(sim, gen, ID, ploidy) %>%
#   summarise(ramets = n()) %>%
#   arrange(desc(ramets))
#
# # biggest genet
# max(genet_sizes$ramets)

#+ plots, warning=F -----------------------------
#' \pagebreak
# population growth/decline
qplot(
  as.numeric(gen),
  n,
  data = pop_sizes,
  geom = "line",
  facets = ~sim
) + theme_classic()

# population growth/decline
# by life stage
qplot(
  as.numeric(gen),
  n,
  data = life_stage_pop_sizes,
  geom = "line",
  colour = as.factor(life_stage),
  facets = ~sim
) + theme_classic() + theme(
  legend.position = "top"
)

# population growth/decline
# by ploidy level
qplot(
  as.numeric(gen),
  n,
  data = ploidy_pop_sizes,
  geom = "line",
  colour = as.factor(ploidy),
  facets = ~sim
) + theme_classic() + theme(
  legend.position = "top"
)

#' \pagebreak
# quick plot selection
qplot(
  as.numeric(gen),
  growth_rate,
  data = plants,
  geom = "jitter",
  facets = ~sim
) + geom_smooth(
  method = "lm"
) + scale_y_continuous(
  breaks = c(1, 1.5, 2),
  limits = c(1, 2)
) + theme_classic()

#' \pagebreak
# quick plot ploidy levels and pop growth/decline
qplot(
  as.numeric(gen),
  ploidy,
  data = plants,
  geom = "jitter"
) + scale_y_continuous(
  breaks = c(2, 3, 4),
  limits = c(1.5, 4.5)
) + theme_classic()


#' \pagebreak
# how to plot the landscape
qplot(
  X + .5,
  Y + .5,
  data = plants %>% filter(gen == 0, sim == 1),
  geom = "point"
) + scale_x_continuous(
  breaks = seq(0, 40, by = 10),
  limits = c(0, 40),
  minor_breaks = 0:40
) + scale_y_continuous(
  breaks = seq(0, 40, by = 10),
  limits = c(0, 40),
  minor_breaks = 0:40
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

#' \pagebreak
# allele values
qplot(
  as.numeric(gen),
  value,
  data = plants %>% unnest(),
  geom = "jitter",
  facets = ~as.factor(locus)
)
