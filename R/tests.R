# testing selection on growth rate
# -----------------------------------
# run the simulation
sim <- disturploidy(generations = 25)
# trim to remove extinction
sim <- sim[1:length(sim) - 1]

# define how to calculate growth rate
get_growth_rate <- function(genome){
  genome %>% filter(locus %in% 1:10) %>% pull(value) %>% sum() / 100
}

# track growth rates over generations
selection = tibble(
  population = integer(),
  growth_rate = numeric()
)
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
}

# quick plot selection
qplot(
  time,
  growth_rate,
  data = selection,
  geom = "jitter"
)
