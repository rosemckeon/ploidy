# Running the model
#---------------------------

#' @name disturploidy
#' @details Runs the model to determine an answer to our question.
#' @author Rose McKeon
#' @param pop_size integer representing starting population size, all individuals begin as seeds (default = 100).
#' @param grid_size integer representing the size of the landscape grid (default = 100, so the grid is 100 x 100 cells big).
#' @param carrying_capacity integer representing K, the carrying capacity (max population size) of any given cell. Seeds are not taken into account for K, only seedlings and adults (default = 5). carrying_capacity used by population_control function which occurs after growth but before reproduction.
#' @param genome_size integer representing the number of loci in each individuals genome. Should be an even number as by default half the genome is used for growth rate and the other half for inbreeding depression (default = 10).
#' @param ploidy_growth_benefit A number between 0 and 1 that represents the proportion by which being polyploid improves growth rate.
#' @param growth_rate_loci a numeric vector of positive integers (eg: 1:5) which represent the loci to use for the trait growth rate (default = NULL, which forces the simulation to use the first half of the genome to calculate this trait).
#' @param inbreeding_loci a numeric vector of positive integers (eg: 1:5) which represent the loci to use to check for inbreeding (default = NULL, which forces the simulation to use the second half of the genome to calculate this trait).
#' @param germination_prob number between 0 and 1 representing the probability that any seed will germinate.
#' @param max_growth_rate A number representing the maximum rate which can be output no matter the genes (default = 2, so individuals can never more than double in size in a generation).
#' @param clonal_size number representing the size at which any seedling can vegatatively reproduce by making clones (default = 1.5).
#' @param adult_size number representing the size at which any seedling becomes a mature adult, capable of sexual reproduction (default = 2).
#' @param N_ovules integer representing the number of ovules any individual plant can create (default = 50).
#' @param pollen_range positive integer representing the dispersal rage of pollen default = 100 so, as grid_size default is also 100, all plants in the landscape will be used as potential pollen donors for all ovules. When < 100 only plants within range will be used as pollen donors, so alleles movement will be restricted into regions of the landscape. Must ot be greater than grid_size, or be a negative value.
#' @param fertilisation_prob number between 0 and 1 representing probability fertilisation between gametes is successful (default = 0.5).
#' @param uneven_matching_prob number between 0 and 1 representing fertlisation_prob applied to zygotes with gametes whose ploidy levels do not match (default = 0.1 so triploids are rare but do occur).
#' @param selfing_polyploid_prob number between 0 and 1 representing fertilisation_prob applied to polyploids which are selfing (default = , so polyploids can always self)..
#' @param selfing_diploid_prob number between 0 and 1 representing fertilisation_prob applied to diploids which are selfing (default = 0, so diploids can never self).
#' @param triploid_mum_prob number between 0 and 1 representing fertilisation_prob applied to zygotes with triploid mums (default = 0.1 to reduce the number of seeds that triploid plants produce).
#' @param adult_survival_prob number between 0 and 1 representing survival probability of adults between generations (default = 0, simulating monocarpic plants that die after sexual reproduction).
#' @param seedling_selection_constant number representing the constant which converts trait values into probabilities. Used to select for plants wth higher growth rates by weighting survival chances of larger seedlings between generations (default = 0.25).
#' @param seed_survival_prob number between 0 and 1 representing survival probability of seeds between generations (default = .05). New seeds are pooled with surviving seeds from previous generations after reproduction. Survival takes place before germination.
#' @param disturbance_freq integer representing the number of generation between distrubances (default = 100).
#' @param disturbance_mortality_prob number between 0 and 1 representing the chance of death during a disturbance. Mortality during a disturbance does not affect seeds (default = 0.75).
#' @param disturbance_xlim The X range of the landscape that should be affected by diturbance. All Y values in this range are affected (default = c(50, 100), so half the landscape is disturbed).
#' @param ploidy_prob number between 0 and 1 representing the chance that genome duplication will occur (default = 0.01).
#' @param mutation_rate number between 0 and 1 representing the chance any given allele will mutate (default = 0.001).
#' @param generations integer representing the number of generations the model should attempt to run for (default = 5). The simulation will break early if extinction occurs.
#' @param simulations integer representing the number of simulations which should be run with these parameters (default = 1).
#' @param return logical value which indicates whether or not to return output at the end of the simulation/s.
#' @param filepath character string defining the file path where output files should be stored. Only used if filename not NULL (default = "data/").
#' @param filename character string defining the name of the output file. Output files are RDS format and the file extension will be appended automatically (default = NULL).
#' @return if return == T, a dataframe of all simulations will be returned showing the population state at the end of each generation (immediately after reproduction, before survival). If return == F, data/plants.rda will be stored automatically and can be accessed with `data(plants)`.
disturploidy <- function(
  pop_size = 100,
  grid_size = 100,
  carrying_capacity = 5,
  genome_size = 10,
  ploidy_growth_benefit = 1,
  growth_rate_loci = NULL,
  inbreeding_loci = NULL,
  germination_prob = .6,
  max_growth_rate = 2,
  clonal_size = 1.5,
  adult_size = 2,
  N_ovules = 50,
  pollen_range = 100,
  fertilisation_prob = .5,
  uneven_matching_prob = .1,
  selfing_polyploid_prob = 1,
  selfing_diploid_prob = 0,
  triploid_mum_prob = .1,
  adult_survival_prob = 0,
  seedling_selection_constant = 0.25,
  seed_survival_prob = .5,
  disturbance_freq = 100,
  disturbance_mortality_prob = .75,
  disturbance_xlim = c(50, 100),
  ploidy_prob = .01,
  mutation_rate = .001,
  generations = 5,
  simulations = 1,
  return = FALSE,
  filepath = "data/",
  filename = NULL
){
  # parameter checking
  stopifnot(
    is.numeric(
      c(
        pop_size,
        grid_size,
        carrying_capacity,
        genome_size,
        ploidy_growth_benefit,
        growth_rate_loci,
        inbreeding_loci,
        germination_prob,
        max_growth_rate,
        clonal_size,
        adult_size,
        N_ovules,
        pollen_range,
        fertilisation_prob,
        uneven_matching_prob,
        selfing_diploid_prob,
        selfing_polyploid_prob,
        triploid_mum_prob,
        adult_survival_prob,
        seedling_selection_constant,
        seed_survival_prob,
        disturbance_freq,
        disturbance_mortality_prob,
        disturbance_xlim,
        ploidy_prob,
        mutation_rate,
        generations,
        simulations
      )
    ),
    is.logical(return),
    is.character(filepath),
    c(
      pop_size,
      grid_size,
      carrying_capacity,
      genome_size,
      growth_rate_loci,
      inbreeding_loci,
      N_ovules,
      pollen_range,
      disturbance_freq,
      disturbance_xlim,
      generations,
      simulations
    )%%1==0,
    between(
      c(
        ploidy_growth_benefit,
        germination_prob,
        fertilisation_prob,
        selfing_diploid_prob,
        selfing_polyploid_prob,
        triploid_mum_prob,
        adult_survival_prob,
        seed_survival_prob,
        disturbance_mortality_prob,
        ploidy_prob,
        mutation_rate
      ),
      0, 1
    ),
    between(pollen_range, 0, grid_size),
    length(disturbance_xlim) == 2,
    (genome_size / 2)%%1==0
  )
  # prepare an object for output
  plants <- NULL
  for(this_sim in 1:simulations){
    # add the starting population for each simulation
    plants <- bind_rows(
      plants,
      populate_landscape(
        pop_size, grid_size, genome_size, this_sim
      )
    )
    message("SIMULATION ", this_sim, ":")
    message("*************")
    message("Starting population of ", pop_size, " random seeds created.")
    # update RDA file for gen 0
    usethis::use_data(plants, overwrite = T)
    # advance time
    for(gen in 1:generations){
      tic("Generation")
      message("Simulation ", this_sim, ", Generation ", gen, ":")
      # change the right pop data
      last_gen <- gen - 1
      this_gen <- plants %>%
        filter(gen == last_gen) %>%
        filter(sim == this_sim)
      # update gen data
      this_gen$gen <- gen
      # make sure we really only have last gen data in this_gen
      stopifnot(
        is.data.frame(this_gen),
        nrow(this_gen) > 0,
        all(this_gen$gen == gen),
        all(this_gen$sim == this_sim)
      )
      # don't do survival or disturbance in 1st generation
      if(gen > 1){
        # subset by lifestage
        seeds <- this_gen %>% filter(
          life_stage == 0
        )
        seedlings <- this_gen %>% filter(
          life_stage == 1
        )
        adults <- this_gen %>% filter(
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
        this_gen <- bind_rows(
          seeds, seedlings, adults
        )
        # output
        if(nrow(this_gen) > 0){
          # disturbance only occurs in generations
          # that are divisible by the frequency.
          if(gen %% disturbance_freq == 0){
            before <- nrow(this_gen)
            this_gen <- this_gen %>% disturb(
              disturbance_mortality_prob,
              disturbance_xlim,
              grid_size
            )
            after <- nrow(this_gen)
            message("  Disturbance killed ", before - after)
          } else {
            message("  No disturbance this generation.")
          }
          message("  Total survivors ", nrow(this_gen))
          this_gen <- this_gen %>% nest_by_location() %>% unnest()
        } else {
          # extinction
          message("  *** EXTINCTION ***")
          message("  Ending simulation.")
          break
        }
        toc()
      } else {
        # gen 1 should only have starting pop
        stopifnot(
          nrow(this_gen) == pop_size
        )
      }

      # subset starting pop/survivors by lifestage
      # should only really be seeds in gen 1
      seeds <- this_gen %>% filter(
        life_stage == 0
      )
      seedlings <- this_gen %>% filter(
        life_stage == 1
      )
      adults <- this_gen %>% filter(
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
      these_plants <- bind_rows(seedlings, adults)
      if(nrow(plants) > 0){
        message("  Adults before growth: ", nrow(adults))
        # grow plants
        these_plants <- these_plants %>% grow("individuals")
        # resubset based on new size
        seedlings <- these_plants %>% filter(
          size < adult_size
        )
        adults <- these_plants %>% filter(
          size >= adult_size
        )
        # and update life stages
        if(nrow(adults) > 0){
          adults$life_stage <- 2
        }
        message("  Adults after growth: ", nrow(adults))
        message("  Plant size ranges from ", min(these_plants$size), " to ", max(these_plants$size))
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
        clones <- clonal_seedlings %>% grow(
          "clones", clonal_size
        )
        # make sure clones are in adjacent cells
        clones <- clones %>% move(grid_size, always_away = T)
        message(
          "  Population increased by: ",
          nrow(clones) - nrow(clonal_seedlings),
          " ramets."
        )
      } else {
        clones <- NULL
        message("  No plants ready to clone.")
      }
      # recombine all seedlings
      seedlings <- bind_rows(
        clonal_seedlings, clones, non_clonal_seedlings
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
        tic("  Reproduction")
        new_seeds <- adults %>% reproduce(
          N_ovules,
          pollen_range,
          fertilisation_prob,
          uneven_matching_prob,
          selfing_polyploid_prob,
          selfing_diploid_prob,
          triploid_mum_prob,
          gen, # generation used for seed ID
          genome_size,
          ploidy_prob,
          mutation_rate,
          grid_size
        )
        # make sure we have some new seeds
        # (stop any mutate errors)
        if(!is.logical(new_seeds)){
          # before counting rows and continuing
          if(nrow(new_seeds) > 0){
            # ensure generation and simulation data correct
            new_seeds <- new_seeds %>% mutate(
              gen = as.integer(gen),
              sim = as.integer(this_sim)
            )
            # calculate growth rates
            new_seeds$growth_rate <- sapply(
              new_seeds$genome, get_growth_rate
            )
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
      # store data
      this_gen <- bind_rows(
        seeds, seedlings, adults
      )
      # recalculate N
      this_gen <- this_gen %>% nest_by_location() %>% unnest()
      # combine with data so far
      plants <- bind_rows(
        plants,
        this_gen
      )
      # update RDA and RDS file every generation
      usethis::use_data(plants, overwrite = T)
      if(!is.null(filename)){
        # make sure it's allowed characters
        stopifnot(
          is.character(filename)
        )
        rds <- paste0(filepath, filename, ".rds")
        saveRDS(plants, rds)
        message("  ", rds, " saved too!")
      }
      message("  Generation data stored.")
      this_gen <- NULL
      toc()
    }
    # end generation loop
  }
  # end simulation loop
  # format the final data
  plants$ID <- as.factor(plants$ID)
  plants$life_stage <- as.factor(plants$life_stage)
  plants$gen <- as.factor(plants$gen)
  plants$sim <- as.factor(plants$sim)
  message("  Factor levels set.")
  # update RDA file
  usethis::use_data(plants, overwrite = T)
  message("  HINT: load with `data(plants)`")
  if(!is.null(filename)){
    # make sure it's allowed characters
    stopifnot(
      is.character(filename)
    )
    saveRDS(plants, rds)
    message("  ", rds, " saved too!")
    message("  HINT: load with `whatever <- readRDS(", rds, ")`")
  }
  if(return){
    # only return if requested
    return(plants)
  }
}
