#' ---
#' title: "Analysis: Population Growth/Decline"
#' author: "Rose McKeon"
#' date: "2019"
#' ---

# clear the workspace
rm(list=ls())
# load dependencies
library(tidyverse)
library(ggplot2)

name <- "quick-test"
exportpath = "simulations/plots/population-size/"
runs <- 3

for(run in runs){
  # get the data
  sim <- readRDS(
    paste0("simulations/data/", name, "-", run, ".rds")
  )
  counts = list(
    juveniles = sim$data$juveniles %>%
      group_by(sim, gen, life_stage) %>%
      tally(),
    adults = sim$data$adults %>%
      group_by(sim, gen, life_stage) %>%
      tally(),
    seedoutput = sim$data$seedoutput %>%
      group_by(sim, gen, life_stage) %>%
      tally()
  )

  # plot poulation by life stage
  filename <- paste0(name, "-", run, "-life_stage.pdf")

  ggplot(
    data = counts$seedoutput,
    aes(
      x = gen,
      y = n,
      colour = life_stage
      )
    ) +
    theme_classic() +
    geom_line() +
    geom_line(
      data = counts$juveniles
    ) +
    geom_line(
      data = counts$adults
    ) +
    geom_hline(
      yintercept = 1600,
      color = "#d1d1d1",
      size = 1
    ) +
    scale_colour_discrete(
      name = "Life stage",
      labels = c("Seeds", "Juveniles", "Adults"),
      h = c(20, 180),
      l = 50
    ) +
    labs(
      tag = toupper(paste(name, run)),
      caption = "Population size over time (grey line represents K)."
    ) +
    xlab("Generation") +
    ylab("Population size") +
    theme(
      legend.position = "top",
      strip.background = element_rect(
        colour = "#d1d1d1",
        fill = "#d1d1d1"
      )
    )


  ggsave(
    filename, path = exportpath, device = "pdf",
    width = 11.69, height = 8.27, units = "in",
    dpi = "retina",
    title = paste(toupper(paste(name, run)), "population size by life stage.")
  )
  #
  # # plot population by ploidy level
  # ploidy_pop_sizes <- sim$data %>%
  #   filter(life_stage != 0) %>%
  #   group_by(sim, gen, ploidy) %>%
  #   tally()
  #
  # filename <- paste0(name, "-", run, "-ploidy.pdf")
  #
  # qplot(
  #   as.numeric(gen),
  #   n,
  #   data = ploidy_pop_sizes,
  #   geom = "line",
  #   colour = as.factor(ploidy)
  # ) + theme_classic() + theme(
  #   legend.position = "top",
  #   strip.background = element_rect(
  #     colour = "#d1d1d1",
  #     fill = "#d1d1d1"
  #   )
  # ) + scale_colour_discrete(
  #   name = "Ploidy level",
  #   h = c(200, 360),
  #   l = 60
  # ) + labs(
  #   tag = toupper(paste(name, run)),
  #   caption = "Population size over time (seeds not included)."
  # ) + xlab("Generation") + ylab("Populations size")
  #
  # ggsave(
  #   filename, path = exportpath, device = "pdf",
  #   width = 11.69, height = 8.27, units = "in",
  #   dpi = "retina",
  #   title = paste(toupper(paste(name, run)), "population size by ploidy level.")
  # )
  #
}
