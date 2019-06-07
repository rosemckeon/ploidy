#' ---
#' title: "Testing disturploidy()"
#' author: "Rose McKeon"
#' date: "June 7th 2019"
#' ---

#' This script tests:
#' - Selection on growth rate
#' - Disturbance

# -----------------------------------
# run the simulation
sim <- disturploidy(generations = 15)
# trim to remove extinction
sim <- sim[1:length(sim) - 1]
# prepare empty gen object
gen <- NULL
# prepare empty selction object
selection = tibble(
  population = integer(),
  growth_rate = numeric()
)
# track growth rates over generations
# and count plants in each gen for sim_df
for(pop in 1:length(sim)){
  # add data
  this_pop  = tibble(
    time = rep(
      pop, nrow(sim[[pop]])
    ),
    growth_rate = sapply(
      sim[[pop]]$genome, get_growth_rate
    )
  )
  # bind rows
  selection <- bind_rows(
    selection,
    this_pop
  )
  # fill gen with column data for sim_df
  this_gen <- rep(pop, nrow(sim[[pop]]))
  gen <- c(gen, this_gen)
}
# convert sim output to dataframe
sim_df <- do.call("bind_rows", sim)
sim_df$gen <- gen

# quick plot selection
# looks messed up when lots of disturbance
qplot(
  time,
  growth_rate,
  data = selection,
  geom = "jitter"
)

# quick plot disturbance
# should see disturbance reducing plants on right
qplot(
  X, Y,
  data = sim_df,
  xlim = c(1, 100),
  alpha = .5,
  facets = ~gen
)
