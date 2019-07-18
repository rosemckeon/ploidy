#' ---
#' title: "Testing disturploidy()"
#' author: "Rose McKeon"
#' date: "June 10th 2019"
#' ---

#' This script tests:
#'
#' - Selection on growth rate
#' - Disturbance
#' - Genome duplication
#'
#' Using: `sim <- disturploidy(generations = 15)`
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
sim <- disturploidy()

#+ data -----------------------
# trim to remove extinction
#sim <- sim[1:length(sim) - 1]
# convert sim output to dataframe
sim_df <- do.call("bind_rows", sim)
# add generations
gen <- NULL
for(pop in 1:length(sim)){
  this_gen <- rep(pop, nrow(sim[[pop]]))
  gen <- c(gen, this_gen)
}
sim_df$gen <- gen
# add growth rates
sim_df$growth_rate <- sapply(
  sim_df$genome, get_growth_rate
)
# add ploidy_lvl
sim_df$ploidy <- sim_df$genome %>%
  map("allele") %>%
  sapply(nlevels)
# format and check data structure
sim_df$ID <- as.factor(sim_df$ID)
sim_df$life_stage <- as.factor(sim_df$life_stage)
summary(sim_df$life_stage)
summary(as.factor(sim_df$gen))
summary(sim_df$growth_rate)
summary(sim_df$ploidy)

#' \pagebreak
str(sim_df, list.len = 10)


#+ plots, warning=F -----------------------------
#' \pagebreak
# quick plot selection
# looks less clear since distrubance added
qplot(
  gen,
  growth_rate,
  data = sim_df,
  geom = "jitter",
  size = factor(ploidy)
)

#' \pagebreak
# quick plot disturbance
# should see disturbance reducing plants on right
qplot(
  X, Y,
  data = sim_df,
  xlim = c(1, 100),
  size = factor(ploidy),
  facets = ~gen
)

#' \pagebreak
# quick histogram of ploidy levels
qplot(
  factor(ploidy),
  data = sim_df,
  facets = ~gen
)
