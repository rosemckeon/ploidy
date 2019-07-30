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
sim <- readRDS("analysis/data/null-3.rds")
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
  legend.position = "top"
)

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
  legend.position = "top"
)

#' Let's look at the kinds of numbers of seeds we have per cell in **sim 4** to see if a K for different lifestages might be a good idea.
#'
sim4 <- sim$data %>% filter(sim == 4)
seeds <- sim4 %>% filter(life_stage == 0)
seed_counts <- seeds %>%
  group_by(X, Y) %>%
  tally() %>%
  arrange(desc(n))

#+ seeds, warning=F -----------------------------
qplot(
  n,
  data = seed_counts,
  geom = "histogram"
) + theme_classic()

qplot(
  X, Y,
  data = seeds,
  geom = "count"
) + theme_classic()
