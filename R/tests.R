#' ---
#' title: "Testing disturploidy()"
#' author: "Rose McKeon"
#' date: "June 7th 2019"
#' ---

#' This script tests:
#' - Selection on growth rate
#' - Disturbance
#' - Genome duplication

# -----------------------------------
# run the simulation
sim <- disturploidy(generations = 15)
# trim to remove extinction
sim <- sim[1:length(sim) - 1]
# prepare empty objects
gen <- NULL
# track growth rates over generations
# and count plants in each gen for sim_df
for(pop in 1:length(sim)){
  # fill gen with column data for sim_df
  this_gen <- rep(pop, nrow(sim[[pop]]))
  gen <- c(gen, this_gen)
}
# convert sim output to dataframe
sim_df <- do.call("bind_rows", sim)
sim_df$gen <- gen
# add growth rates
sim_df$growth_rate <- sapply(
  sim_df$genome, get_growth_rate
)
# add ploidy_lvl
sim_df$ploidy <- sim_df$genome %>%
  map("allele") %>%
  sapply(nlevels)

# quick plot selection
# looks messed up when lots of disturbance
qplot(
  gen,
  growth_rate,
  data = sim_df,
  geom = "jitter",
  size = factor(ploidy)
)

# quick plot disturbance
# should see disturbance reducing plants on right
# dot size reflects amount of genome duplication
qplot(
  X, Y,
  data = sim_df,
  xlim = c(1, 100),
  size = factor(ploidy),
  facets = ~gen
)

# quick histogram of ploidy levels
qplot(
  factor(ploidy),
  data = sim_df,
  facets = ~gen
)
