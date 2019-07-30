#' ---
#' title: "Analysis"
#' author: "Rose McKeon"
#' date: "2019"
#' ---

#+ setup, message=F, warning=F, include=F --------
# clear the workspace
rm(list=ls())
# load dependencies
library(tidyverse)
library(ggplot2)
library(rlang)
source("analysis/R/functions.R")

name <- "null"
runs <- 3:4

for(run in runs){
  # get the data
  sim <- readRDS(
    paste0("analysis/data/", name, "-", run, ".rds")
  )
  # export plot for pop size over time by life stage
  plot_sim_pop_sizes(
    name, run, data = sim$data,
    colour = "life_stage",
    colour.name = "Life stage",
    colour.labels = c("Seeds", "Juveniles", "Adults"),
    colour.h = c(20, 180),
    colour.l = 50
  )
  # export plot for pop size over time by ploidy level
  plot_sim_pop_sizes(
    name, run, data = sim$data,
    colour = "ploidy",
    colour.name = "Ploidy level",
    colour.h = c(200, 360),
    colour.l = 60
  )
}

# genet size over time
# genet_sizes <- plants %>%
#   group_by(sim, gen, ID, ploidy) %>%
#   summarise(ramets = n()) %>%
#   arrange(desc(ramets))
#
# # biggest genet
# max(genet_sizes$ramets)

# # quick plot selection
# qplot(
#   as.numeric(gen),
#   growth_rate,
#   data = plants,
#   geom = "jitter",
#   facets = ~sim
# ) + geom_smooth(
#   method = "lm"
# ) + scale_y_continuous(
#   breaks = c(1, 1.5, 2),
#   limits = c(1, 2)
# ) + theme_classic()
#
# # how to plot the landscape
# qplot(
#   X + .5,
#   Y + .5,
#   data = plants %>% filter(gen == 0, sim == 1),
#   geom = "point"
# ) + scale_x_continuous(
#   breaks = seq(0, 40, by = 10),
#   limits = c(0, 40),
#   minor_breaks = 0:40
# ) + scale_y_continuous(
#   breaks = seq(0, 40, by = 10),
#   limits = c(0, 40),
#   minor_breaks = 0:40
# ) + theme_classic() + theme(
#   panel.grid.major = element_line(
#     size = .5,
#     linetype = "solid",
#     colour = "gray"
#   ),
#   panel.grid.minor = element_line(
#     size = .25,
#     linetype = "solid",
#     colour = "gray"
#   )
# )
#
# # allele values
# qplot(
#   as.numeric(gen),
#   value,
#   data = plants %>% unnest(),
#   geom = "jitter",
#   facets = ~as.factor(locus)
# )
