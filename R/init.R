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
  N_gametes = 500,
  pollen_finds_ova_prob = .5,
  adult_survival_prob = .5,
  seedling_survival_prob = .5,
  seed_survival_prob = .5
){
  out <- list()
  # populate landscape
  out$pop_0 <- populate_landscape(
    pop_size, grid_size, genome_size
  )
  if(nrow(out$pop_0) == pop_size){
    message("Starting population created...")
    # advance time
    for(generation in 1:generations){
      message("*** GENERATION ", generation, " BEGINNING ***")
      # change the right pop data
      last_gen <- out[[paste0("pop_", generation-1)]]

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
      message(
        "*** With ",
        nrow(seeds), " seeds, ",
        nrow(seedlings), " seedlings, and ",
        nrow(adults), " adults."
      )

      # germination
      message("Germination:")
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
        if(nrow(new_seedlings) > 0){
          message("  ", nrow(new_seedlings), " new seedlings created...")
          seedlings <- bind_rows(
            seedlings, new_seedlings
          )
          message("  Seedling total: ", nrow(seedlings), ".")
        }
      } else {
        message("  No seeds to germinate.")
      }

      # growth
      message("Growth (individual):")
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
      } else {
        message("  No plants ready to grow.")
      }
      message("Growth (clonal):")
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
        clonal_seedlings <- clonal_seedlings %>% grow(
          "clones", clonal_size
        )
        message(
          "  Population increased by: ",
          nrow(clonal_seedlings) - nrow(seedlings),
          " ramets."
        )
      } else {
        message("  No plants ready to clone.")
      }
      # recombine all seedlings
      seedlings <- bind_rows(
        clonal_seedlings, non_clonal_seedlings
      )

      # control population size with carrying capacity (K)
      message("Population control:")
      # recombine all life stages
      this_gen <- bind_rows(
        seeds, seedlings, adults
      )
      message("  Total population size: ", nrow(this_gen))
      message("  Carrying capacity (K) per landscape cell: ", carrying_capacity)
      if(nrow(this_gen) > 0){
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
          seeds <- this_gen %>% filter(
            life_stage == 0
          )
          seedlings <- this_gen %>% filter(
            life_stage == 1
          )
          adults <- this_gen %>% filter(
            life_stage == 2
          )
          message(
            "  ",
            nrow(seeds), " seeds, ",
            nrow(seedlings), " seedlings, and ",
            nrow(adults), " adults."
          )
        } else {
          message("  No landscape cells have N > K.")
        }
      }

      # reproduction
      message("Reproduction:")
      message("  Adults ready to reproduce: ", nrow(adults))
      if(nrow(adults) > 0){
        new_seeds <- adults %>% reproduce(
          N_gametes,
          pollen_finds_ova_prob,
          generation # generation used for seed ID
        )
        # make sure we have some new seeds
        if(!is.logical(new_seeds)){
          # before counting rows and continuing
          if(nrow(new_seeds) > 0){
            # then do seed dispersal
            new_seeds <- new_seeds %>% move(grid_size) %>%
              # make sure has column N for binding
              nest_by_location() %>% unnest()
            message("  New seeds dispersed: ", nrow(new_seeds))
            # and combine with old seeds
            seeds <- bind_rows(seeds, new_seeds)
            message("  Total seeds (including seed bank): ", nrow(seeds))
          }
        } else {
          message("  No new seeds created.")
        }
      }

      # survival
      message("Survival:")
      if(nrow(adults) > 0){
        adults <- adults %>% survive(adult_survival_prob)
        message("  Surviving adults: ", nrow(adults))
      }
      if(nrow(seedlings) > 0){
        seedlings <- seedlings %>% survive(seedling_survival_prob)
        message("  Surviving seedlings: ", nrow(adults))
      }
      if(nrow(seeds) > 0){
        seeds <- seeds %>% survive(seed_survival_prob)
        message("  Surviving seeds: ", nrow(adults))
      }

      # output
      this_gen <- bind_rows(
        seeds, seedlings, adults
      )
      if(nrow(this_gen) > 0){
        message("  Total population size: ", nrow(this_gen))
        # recalculate N
        this_gen <- this_gen %>% nest_by_location() %>% unnest()
        # store and continue
        out[[paste0("pop_", generation)]] <- this_gen
      } else {
        # extinction
        message("  *** EXTINCTION ***")
        message("  Ending simulation (generation ", generation, ")")
        out[[paste0("pop_", generation)]] <- "Plants are extinct."
        break
      }
    }
  }
  # return data
  return(out)
}
