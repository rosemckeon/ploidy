# Running the model
#---------------------------

#' @name disturploidy
#' @details Runs the model to determine an answer to our question.
#' @author Rose McKeon
#' @param pop_size size of the starting population
#' @param grid_size size of the landscape grid
#' @return population data for each generation
disturploidy <- function(
  pop_size = 1000,
  grid_size = 100,
  carrying_capacity = 20,
  genome_size = 10,
  generations = 20,
  germination_prob = .6,
  clonal_size = 1.5,
  adult_size = 2,
  N_gametes = 500,
  pollen_finds_ova_prob = 1,
  adult_survival_prob = 0,
  seedling_selection_constant = 5,
  seed_survival_prob = .9,
  disturbance_freq = 100,
  disturbance_mortality_prob = .75,
  disturbance_xlim = c(50, 100),
  ploidy_prob = .01,
  mutation_rate = .001
){
  out <- list()
  # populate landscape
  out$pop_0 <- populate_landscape(
    pop_size, grid_size, genome_size
  )
  message("Starting population created with ", pop_size, " seeds...")
  # advance time
  for(gen in 1:generations){
    # change the right pop data
    last_gen <- out[[paste0("pop_", gen-1)]]

    tic("Generation")
    message("GENERATION ", gen, " BEGINNING ------------------")

    # don't do survival or disturbance in 1st generation
    if(gen > 1){
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
      n_seeds <- nrow(seeds)
      n_seedlings <- nrow(seedlings)
      n_adults <- nrow(adults)

      message("Survival:")
      tic("  Survival")
      # see who survives
      if(nrow(seeds) > 0){
        seeds <- seeds %>% survive(seed_survival_prob)
        message("  Surviving seeds: ", nrow(seeds), "/", n_seeds)
      }
      if(nrow(seedlings) > 0){
        seedlings <- seedlings %>% select(
          "size", seedling_selection_constant
        )
        message("  Surviving seedlings: ", nrow(seedlings), "/", n_seedlings)
      }
      if(nrow(adults) > 0){
        adults <- adults %>% survive(adult_survival_prob)
        message("  Surviving adults: ", nrow(adults), "/", n_adults)
      }

      # prepare pop for disturbance
      last_gen <- bind_rows(
        seeds, seedlings, adults
      )
      # output
      if(nrow(last_gen) > 0){
        # disturbance only occurs in generations
        # that are divisible by the frequency.
        if(gen %% disturbance_freq == 0){
          before <- nrow(last_gen)
          last_gen <- last_gen %>% disturb(
            disturbance_mortality_prob,
            disturbance_xlim,
            grid_size
          )
          after <- nrow(last_gen)
          message("  Disturbance killed ", before - after)
        } else {
          message("  No disturbance this generation.")
        }
        message("  Total survivors ", nrow(last_gen))
        last_gen <- last_gen %>% nest_by_location() %>% unnest()
      } else {
        # extinction
        message("  *** EXTINCTION ***")
        message("  Ending simulation (generation ", gen, ")")
        break
      }
      toc()
    }
    # subset survivors by lifestage
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
    message("Germination:")
    if(nrow(seeds) > 0){
      tic("  Germination")
      seeds <- seeds %>% germinate(
        germination_prob
      )
      new_seedlings <- seeds %>% filter(
        life_stage == 1
      )
      seeds <- seeds %>% filter(
        life_stage == 0
      )
      if(nrow(new_seedlings) > 0){
        message("  ", nrow(new_seedlings), " new seedlings created...")
        seedlings <- bind_rows(
          seedlings, new_seedlings
        )
        message("  Seedling total: ", nrow(seedlings), ".")
      }
      toc()
    } else {
      message("  No seeds to germinate.")
    }
    # growth
    message("Growth:")
    tic("  Growth")
    # combine all plants that are able to grow
    plants <- bind_rows(seedlings, adults)
    if(nrow(plants) > 0){
      message("  Adults before growth: ", nrow(adults))
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
      message("  Adults after growth: ", nrow(adults))
      message("  Plant size ranges from ", min(plants$size), " to ", max(plants$size))
    } else {
      message("  No plants ready to grow.")
    }
    # subset plants that are able to clone
    # (adults invest in reproduction instead)
    clonal_seedlings <- seedlings %>% filter(
      size >= clonal_size
    )
    non_clonal_seedlings <- seedlings %>% filter(
      size < clonal_size
    )
    if(nrow(clonal_seedlings) > 0){
      # clone plants
      # use new object so we can count the new ramets
      new_clonal_seedlings <- clonal_seedlings %>% grow(
        "clones", clonal_size
      )
      message(
        "  Population increased by: ",
        nrow(new_clonal_seedlings) - nrow(clonal_seedlings),
        " ramets."
      )
      # make sure we can bind rows even if this does't happen
      # by overwriting original object
      clonal_seedlings <- new_clonal_seedlings
    } else {
      message("  No plants ready to clone.")
    }
    # recombine all seedlings
    seedlings <- bind_rows(
      clonal_seedlings, non_clonal_seedlings
    )
    toc()

    # control population size with carrying capacity (K)
    message("Population control:")
    # recombine all life stages that aren't seeds
    this_gen <- bind_rows(
      seedlings, adults
    )
    message("  Total population size: ", nrow(this_gen))
    message("  Carrying capacity (K) per landscape cell: ", carrying_capacity)
    if(nrow(this_gen) > 0){
      tic("  Population control")
      # recalculate N for combined data
      this_gen <- this_gen %>% nest_by_location()
      # only control population if some cells have N > K
      if(max(this_gen$N) > carrying_capacity){
        message("  Cells with N > K found (max ", max(this_gen$N), ").")
        # reduce N to near K
        this_gen <- pop_control(
          this_gen, carrying_capacity
        ) %>% unnest()
        # resubset by life stage
        # only needed if population controlled
        message("  Population reduced to: ", nrow(this_gen))
        seedlings <- this_gen %>% filter(
          life_stage == 1
        )
        adults <- this_gen %>% filter(
          life_stage == 2
        )
        message(
          "  ",
          nrow(seedlings), " seedlings, and ",
          nrow(adults), " adults."
        )
      } else {
        message("  No landscape cells have N > K.")
      }
      toc()
    }

    # reproduction
    message("Reproduction:")
    message("  Adults ready to reproduce: ", nrow(adults))
    if(nrow(adults) > 0){
      #message("  The slow bit...")
      tic("  Reproduction")
      new_seeds <- adults %>% reproduce(
        N_gametes,
        pollen_finds_ova_prob,
        gen, # generation used for seed ID
        genome_size,
        ploidy_prob,
        mutation_rate
      )
      # make sure we have some new seeds
      if(!is.logical(new_seeds)){
        # before counting rows and continuing
        if(nrow(new_seeds) > 0){
          # then do seed dispersal
          new_seeds <- new_seeds %>% move(grid_size) %>%
            # make sure has column N for binding
            nest_by_location() %>% unnest()
          message("  Seeds dispersed.")
          # and combine with old seeds
          seeds <- bind_rows(seeds, new_seeds)
          message("  Total seeds (including seed bank): ", nrow(seeds))
        }
      } else {
        message("  No new seeds created.")
      }
      toc()
    }

    # output data
    this_gen <- bind_rows(
      seeds, seedlings, adults
    )
    this_gen <- this_gen %>% nest_by_location() %>% unnest()
    out[[paste0("pop_", gen)]] <- this_gen
    message("  *Data stored*")
    toc()
  }
  # return data
  return(out)
}
