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

# load the data
# data(dploidy); plants <- dploidy
sim <- readRDS("analysis/data/null-1.rds")
plants <- sim$data

#' \pagebreak
# life stages over time
life_stage_pop_sizes <- plants %>%
  group_by(sim, gen, life_stage) %>%
  tally()

# ploidy over time
ploidy_pop_sizes <- plants %>%
  group_by(sim, gen, ploidy) %>%
  tally()

#+ plots, warning=F -----------------------------
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
