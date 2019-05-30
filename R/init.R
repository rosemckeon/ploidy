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
  carrying_capacity = 2,
  genome_size = 100,
  generations = 30,
  germination_prob = .5,
  clonal_size = 1,
  adult_size = 9,
  adult_survival_prob = .5,
  seedling_survival_prob = .5,
  seed_survival_prob = .5
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

    # subset by lifestage
    seeds <- last_gen %>% filter(
      life_stage == 0
    )
    seedlings <- last_gen %>% filter(
      life_stage == 1
    )
    adults <- last_gen %>% filter(
      life_stage == 2
    )

    # germination
    if(nrow(seeds) > 0){
      seeds <- seeds %>% germinate(
        germination_prob
      )
      new_seedlings <- seeds %>% filter(
        life_stage == 1
      )
      seeds <- seeds %>% filter(
        life_stage == 0
      )
      seedlings <- bind_rows(
        seedlings, new_seedlings
      )
    }

    # growth
    # combine all plants that are able to grow
    plants <- bind_rows(seedlings, adults)
    if(nrow(plants) > 0){
      # grow plants
      plants <- plants %>% grow("individuals")
      # resubset based on new size
      seedlings <- plants %>% filter(
        size < adult_size
      )
      adults <- plants %>% filter(
        size >= adult_size
      )
      # and update life stages
      if(nrow(adults) > 0){
        adults$life_stage <- 2
      }
    }
    # subset plants that are able to clone
    # (adults don't clone as they invest in reproduction)
    clonal_seedlings <- seedlings %>% filter(
      size >= clonal_size
    )
    non_clonal_seedlings <- seedlings %>% filter(
      size < clonal_size
    )
    if(nrow(clonal_seedlings) > 0){
      # clone plants
      clonal_seedlings <- clonal_seedlings %>% grow(
        "clones", clonal_size
      )
    }
    # recombine all seedlings
    seedlings <- bind_rows(
      clonal_seedlings, non_clonal_seedlings
    )

    # control population size with carrying capacity (K)
    # recombine all life stages
    this_gen <- bind_rows(
      seeds, seedlings, adults
    )
    if(nrow(this_gen) > 0){
      # recalculate N for combined data
      this_gen <- this_gen %>% nest_by_location()
      # only control population if some cells have N > K
      if(max(this_gen$N) > carrying_capacity){
        # reduce N to near K
        this_gen <- pop_control(
          this_gen, carrying_capacity
        ) %>% unnest()
        # resubset by life stage
        # only needed if population controlled
        seeds <- this_gen %>% filter(
          life_stage == 0
        )
        seedlings <- this_gen %>% filter(
          life_stage == 1
        )
        adults <- this_gen %>% filter(
          life_stage == 2
        )
      }
    }

    # reproduction
    # if(nrow(adults) > 0){
    #   new_seeds <- adults %>% reproduce() %>% move(grid_size)
    #   seeds <- bind_rows(seeds, new_seeds)
    # }

    # survival
    if(nrow(adults) > 0){
      adults <- adults %>% survive(adult_survival_prob)
    }
    if(nrow(seedlings) > 0){
      seedlings <- seedlings %>% survive(seedling_survival_prob)
    }
    if(nrow(seeds) > 0){
      seeds <- seeds %>% survive(seed_survival_prob)
    }

    # output
    this_gen <- bind_rows(
      seeds, seedlings, adults
    )
    if(nrow(this_gen) > 0){
      # recalculate N
      this_gen <- this_gen %>% nest_by_location() %>% unnest()
      # store and continue
      out[[paste0("pop_", i)]] <- this_gen
    } else {
      # extinction
      out[[paste0("pop_", i)]] <- "Plants are extinct."
      break
    }
  }
  # return data
  return(out)
}
