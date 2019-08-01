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

name <- "null"
exportpath = "analysis/plots/population-size/"

runs <- 1:2

for(run in runs){
  # get the data
  sim <- readRDS(
    paste0("analysis/data/", name, "-", run, ".rds")
  )

  # plot poulation by life stage
  life_stage_pop_sizes <- sim$data %>%
    group_by(sim, gen, life_stage) %>%
    tally()

  filename <- paste0(name, "-", run, "-life_stage.pdf")

  qplot(
    as.numeric(gen),
    n,
    data = life_stage_pop_sizes,
    geom = "line",
    colour = as.factor(life_stage),
    facets = ~sim
  ) + theme_classic() + theme(
    legend.position = "top",
    strip.background = element_rect(
      colour = "#d1d1d1",
      fill = "#d1d1d1"
    )
  ) + geom_hline(
    yintercept = 1600,
    color = "#d1d1d1",
    size = 4
  ) + scale_colour_discrete(
    name = "Life stage",
    labels = c("Seeds", "Juveniles", "Adults"),
    h = c(20, 180),
    l = 50
  ) + labs(
    tag = toupper(paste(name, run)),
    caption = "Population size over time."
  ) + xlab("Generation")

  ggsave(
    filename, path = exportpath, device = "pdf",
    width = 11.69, height = 8.27, units = "in",
    dpi = "retina"
  )

  # plot population by ploidy level
  ploidy_pop_sizes <- sim$data %>%
    group_by(sim, gen, ploidy) %>%
    tally()

  filename <- paste0(name, "-", run, "-ploidy.pdf")

  qplot(
    as.numeric(gen),
    n,
    data = ploidy_pop_sizes,
    geom = "line",
    colour = as.factor(ploidy),
    facets = ~sim
  ) + theme_classic() + theme(
    legend.position = "top",
    strip.background = element_rect(
      colour = "#d1d1d1",
      fill = "#d1d1d1"
    )
  ) + scale_colour_discrete(
    name = "Ploidy level",
    h = c(200, 360),
    l = 60
  ) + labs(
    tag = toupper(paste(name, run)),
    caption = "Population size over time (all life stages)."
  ) + xlab("Generation")

  ggsave(
    filename, path = exportpath, device = "pdf",
    width = 11.69, height = 8.27, units = "in",
    dpi = "retina"
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
