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
sim <- disturploidy(pollen_range = 10, data_type = "df")

# summaries
summary(sim$life_stage)
summary(as.factor(sim$gen))
summary(sim$growth_rate)
summary(simf$ploidy)

#' \pagebreak
str(sim, list.len = 10)

#+ plots, warning=F -----------------------------
#' \pagebreak
# quick plot selection
# looks less clear since distrubance added
qplot(
  gen,
  growth_rate,
  data = sim,
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
  data = sim,
  geom = "jitter"
) + scale_y_continuous(
  breaks = c(2, 3, 4),
  limits = c(2, 4)
) + theme_classic()
