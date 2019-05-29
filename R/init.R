# Running the model
#---------------------------

#' @name disturploidy
#' @details Runs the model to determine an answer to our question.
#' @author Rose McKeon
#' @param pop_size size of the starting population
#' @param grid_size size of the landscape grid
#' @return population data for each generation
disturploidy <- function(
  pop_size = 100,
  grid_size = 100,
  genome_size = 100,
  generations = 2,
  germination_prob = .3,
  adult_size = 10,
  adult_survival_prob = 0,
  seedling_survival_prob = .5,
  seed_survival_prob = .9
){
  out <- list()
  # populate landscape
  out$pop_0 <- populate_landscape(
    pop_size, grid_size, genome_size
  )
  # advance time
  for(i in 1:generations){
    # change the right pop data
    last_gen <- out[[paste0("pop_", i-1)]]

    # germination
    # still needs density dependence
    seeds <- last_gen %>% filter(
      life_stage == 0
    ) %>% germinate(germination_prob)

    seedlings <- seeds %>% filter(
      life_stage == 1
    )
    seeds <- seeds %>% filter(
      life_stage == 0
    )

    # growth
    # still needs density dependence and actual growth:
    # both individual growth and clonal growth
    old_seedlings <- last_gen %>% filter(
      life_stage == 1
    )
    seedlings <- bind_rows(
      old_seedlings, seedlings
    )# %>% grow()

    adults <- seedlings %>% filter(
      size >= adult_size
    )
    seedlings <- seedlings %>% filter(
      size < adult_size
    )

    # reproduction
    # still needs density dependence and actual reproduction
    # new_seeds <- adults %>% reproduce() %>% move(grid_size)
    # seeds <- bind_rows(seeds, new_seeds)

    # survival
    # still needs density dependence
    adults <- adults %>% survive(adult_survival_prob)
    seedlings <- seedlings %>% survive(seedling_survival_prob)
    seeds <- seeds %>% survive(seed_survival_prob)

    # data storage and recalculation of N
    this_gen <- bind_rows(
      seeds, seedlings, adults
    ) %>% nest_by_location() %>% unnest()

    out[[paste0("pop_", i)]] <- this_gen
  }
  # return data
  return(out)
}
