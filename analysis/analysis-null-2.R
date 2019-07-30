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
sim <- readRDS("analysis/data/null-2.rds")
plants <- sim$data

#' ## Population growth/decline
#'
#' ### By life stage:
#'
life_stage_pop_sizes <- plants %>%
  group_by(sim, gen, life_stage) %>%
  tally()

#+ life_stage, warning=F -----------------------------
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
) + scale_colour_discrete(
  name = "Life stage",
  labels = c("Seeds", "Juveniles", "Adults"),
  h = c(20, 180),
  l = 50
) + labs(
  tag = "NULL 2",
  caption = "Population size over time."
) + xlab("Generation")

#' ### By ploidy level:
#'
ploidy_pop_sizes <- plants %>%
  group_by(sim, gen, ploidy) %>%
  tally()

#+ ploidy, warning=F -----------------------------
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
  tag = "NULL 2",
  caption = "Population size over time."
) + xlab("Generation")

#' Let's look at the kinds of numbers of seeds we have per cell in **sim 2** - when the population begins to boom around generation 100.
#'
sim2 <- sim$data %>%
  filter(sim == 2)

seeds <- sim4 %>%
  filter(life_stage == 0)

seed_counts <- seeds %>%
  group_by(X, Y, gen) %>%
  tally() %>%
  arrange(gen)

boom <- seeds %>%
  filter(between(as.numeric(gen), 91, 99))

boom_counts <- boom %>%
  group_by(X, Y, gen) %>%
  tally() %>%
  arrange(gen)

#+ seeds, warning=F -----------------------------
qplot(
  n,
  data = boom_counts,
  geom = "histogram",
  facets = ~gen,
  binwidth = 1
) + theme_classic() + labs(
  caption = "Comparison of frequencies of seed counts in any populated cell on the landscape\nfrom generation 90 to 98 of null run 2, sim 2 (cells with 0 seeds not counted)."
)

qplot(
  n,
  data = seed_counts %>%
    filter(gen == "100" | gen == "200"),
  geom = "histogram",
  facets = ~gen,
  binwidth = 1
) + theme_classic() + theme(
  legend.position = "top"
) + labs(
  caption = "Comparison of frequencies of seed counts in any populated cell on the landscape, between\ngeneration 100 and generation 200 of null run 2, sim 2 (cells with 0 seeds not counted)."
)

qplot(
  X, Y,
  data = seeds %>%
    filter(gen == "100" | gen == "200"),
  geom = "count",
  facets = ~gen
) + theme_classic() + theme(
  legend.position = "top"
) + labs(
  caption = "Comparison of seed counts by coordinate, between generation 100 and generation 200 of null run 2, sim 2."
)
