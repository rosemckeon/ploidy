# get the parameters from the call
params <- as.list(sim$call)

# summerise the data as life stage counts for each generation
counts <- sim$data %>%
  group_by(gen, sim, life_stage) %>%
  tally()

if(params$seed_survival_prob == 0){
  # correct the seed counts
  counts <- counts %>% mutate(
    n = replace(
      n, which(life_stage == 0), n * (1 / params$germination_prob)
    )
  )
}
