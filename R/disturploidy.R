#' @name disturploidy
#' @title disturploidy
#' @usage Runs a simulation, or repeated simulations, of a plant population over time.
#' @author Rose McKeon
#' @param pop_size integer representing starting population size, all individuals begin as seeds (default = 100).
#' @param grid_size integer representing the size of the landscape grid (default = 100, so the grid is 100 x 100 cells big).
#' @param carrying_capacity integer representing K, the carrying capacity (max population size) of any given cell. Seeds and juveniles are not taken into account for K, only adults who compete for resouces after growth (which creates adults) but before reproduction (default = 1, so only 1 new adult per square can survive to reproduce).
#' @param genome_size integer > 2 representing the number of loci in each individuals genome. Should be big enough to hold all loci chosen for traits, growth rate and inbreeding (default = 2).
#' @param ploidy_growth_benefit A number between 0 and 1 that represents the proportion by which being polyploid improves growth rate.
#' @param growth_rate_loci a numeric vector of positive integers (eg: 1 or 1:5) which represents the locus/loci to use for the trait growth rate (default = 1).
#' @param inbreeding_locus positive integer which represents the locus to use to check for inbreeding. Should not match loci used for growth rate (default = 2).
#' @param inbreeding_sensitivity number between 0 and 1 representing the strength of inbreeding. 0 = no effect and 1 is maximum effect. Checking for identical alleles at the specified inbreeding locus is used as a proxy for having homozygous deleterious alleles. When this happens survival chances are modified according to inbreeding sensitivity (default = 0.5, so survival chances are halved when fitness disadvantages due to inbreeding are detected).
#' @param germination_prob number between 0 and 1 representing the probability that any seed will germinate.
#' @param max_growth_rate A number representing the maximum rate which can be output no matter the genes (default = 2, so individuals can never more than double in size in a generation).
#' @param clonal_growth logical value which determines whether or not adults can reproduce asexually via vegetative clonal growth (default = FALSE).
#' @param adult_size number representing the size at which any juvenile becomes a mature adult, capable of sexual reproduction (default = 2).
#' @param N_ovules integer representing the number of ovules any individual plant can create (default = 50).
#' @param pollen_range positive integer representing the dispersal rage of pollen default = 100 so, as grid_size default is also 100, all plants in the landscape will be used as potential pollen donors for all ovules. When < 100 only plants within range will be used as pollen donors, so alleles movement will be restricted into regions of the landscape. Must ot be greater than grid_size, or be a negative value.
#' @param fertilisation_prob number between 0 and 1 representing probability fertilisation between gametes is successful (default = 0.5).
#' @param uneven_matching_prob number between 0 and 1 representing fertlisation_prob applied to zygotes with gametes whose ploidy levels do not match (default = 0.1 so triploids are rare but do occur).
#' @param selfing_polyploid_prob number between 0 and 1 representing fertilisation_prob applied to polyploids which are selfing (default = , so polyploids can always self)..
#' @param selfing_diploid_prob number between 0 and 1 representing fertilisation_prob applied to diploids which are selfing (default = 0, so diploids can never self).
#' @param triploid_mum_prob number between 0 and 1 representing fertilisation_prob applied to zygotes with triploid mums (default = 0.1 to reduce the number of seeds that triploid plants produce).
#' @param adult_survival_prob number between 0 and 1 representing survival probability of adults between generations (default = 0, simulating monocarpic plants that die after sexual reproduction).
#' @param juvenile_selection_constant number representing the constant which converts trait values into probabilities. Used to select for plants wth higher growth rates by weighting survival chances of larger juveniles between generations (default = 0.25).
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
#' @param logfilepath character string defining the file path where output log files should be stored. Only used if logfilename not NULL (default = "data/logs/").
#' @param logfilename character string defining the name of the output log file. Log files are txt format and the file extension will be appended automatically (default = NULL).
#' @return if return == T, a dataframe of all simulations will be returned showing the population state at the end of each generation (immediately after reproduction, before survival). If return == F, data/dploidy.rda will be stored automatically and can be accessed with `data(dploidy)`.
#' @examples
#' # with default parameters
#' disturploidy()
#' data(dploidy)
#' dploidy
#'
#' # with minimal console output
#' # (the rest logged to TXT files)
#' disturploidy(logfilename = "whatever")
#'
#' # with stored data object as RDS file
#' disturploidy(filename = "whatever")
#'
#' # assigning output to a new object
#' whatever <- disturploidy(return = T)
#'
#' @export
disturploidy <- function(
  pop_size = 100,
  grid_size = 100,
  carrying_capacity = 1,
  genome_size = 2,
  ploidy_growth_benefit = 1,
  growth_rate_loci = 1,
  inbreeding_locus = 2,
  inbreeding_sensitivity = .5,
  germination_prob = .6,
  max_growth_rate = 2,
  clonal_growth = FALSE,
  adult_size = 2,
  N_ovules = 50,
  pollen_range = 100,
  fertilisation_prob = .5,
  uneven_matching_prob = .1,
  selfing_polyploid_prob = 1,
  selfing_diploid_prob = 0,
  triploid_mum_prob = .1,
  adult_survival_prob = 0,
  juvenile_selection_constant = 0.25,
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
  filename = NULL,
  logfilepath = "data/logs/",
  logfilename = NULL
){
  tic.clearlog()
  tic("Entire run time")
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
        inbreeding_locus,
        inbreeding_sensitivity,
        germination_prob,
        max_growth_rate,
        adult_size,
        N_ovules,
        pollen_range,
        fertilisation_prob,
        uneven_matching_prob,
        selfing_diploid_prob,
        selfing_polyploid_prob,
        triploid_mum_prob,
        adult_survival_prob,
        juvenile_selection_constant,
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
    is.logical(c(clonal_growth, return)),
    is.character(c(filepath, logfilepath)),
    c(
      pop_size,
      grid_size,
      carrying_capacity,
      genome_size,
      growth_rate_loci,
      inbreeding_locus,
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
        inbreeding_sensitivity,
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
    !any(inbreeding_locus == growth_rate_loci),
    length(inbreeding_locus) == 1,
    length(disturbance_xlim) == 2,
    genome_size >= 2,
    genome_size >= max(growth_rate_loci),
    genome_size >= inbreeding_locus
  )
  # prepare an object for output
  dploidy <- list(
    call = match.call(),
    data = NULL,
    time = list(
      start = Sys.time(),
      end = NULL
    ),
    R = R.version.string,
    "DisturPloidy" = "0.0.0005"
  )
  if(!is.null(logfilename)){
    dploidy$log = list()
  }
  # Run the simulations
  for(this_sim in 1:simulations){
    tic("Simulation")
    # Start logging if requested
    if(!is.null(logfilename)){
      # make sure it's allowed characters
      stopifnot(
        is.character(logfilename)
      )
      connection <- paste0(logfilepath, logfilename, "-sim-", this_sim, ".txt")
      dploidy$log[[this_sim]] <- connection
      message("Output being logged to: ", connection)
      logfile <- file(connection, open = "wt")
      sink(logfile, append = F, type = "message")
    }
    # add the starting population for each simulation
    dploidy$data <- bind_rows(
      dploidy$data,
      populate_landscape(
        pop_size, grid_size, genome_size, this_sim
      )
    )
    message("SIMULATION ", this_sim, ":")
    message("*************")
    message("Starting population of ", pop_size, " random seeds created.")
    # advance time
    for(generation in 1:generations){
      tic("Generation")
      if(!is.null(logfilename)){
        # Allow minimal screen output for each generation being run.
        sink(type = "message")
        message("Simulation ", this_sim, ", Generation ", generation, ":")
        sink(logfile, append = T, type = "message")
      }
      message("Simulation ", this_sim, ", Generation ", generation, ":")
      # change the right pop data
      last_gen <- generation - 1
      this_gen <- dploidy$data %>%
        filter(gen == last_gen) %>%
        filter(sim == this_sim)
      # update gen data
      this_gen$gen <- as.integer(generation)
      # make sure we really only have last gen data in this_gen
      stopifnot(
        is.data.frame(this_gen),
        nrow(this_gen) > 0,
        all(this_gen$gen == generation),
        all(this_gen$sim == this_sim)
      )
      # subset by lifestage
      seeds <- this_gen %>% filter(
        life_stage == 0
      )
      juveniles <- this_gen %>% filter(
        life_stage == 1
      )
      adults <- this_gen %>% filter(
        life_stage == 2
      )

      # only do survival or disturbance after 1st generation
      if(generation > 1){
        message("Survival:")
        tic("Survival")
        # see if we have lifestages subject to winter survival in the population
        n_juveniles <- nrow(juveniles)
        n_adults <- nrow(adults)
        # see who survives
        # seed survival occurs later (after germination)
        if(nrow(juveniles) > 0){
          juveniles <- juveniles %>%
            hard_select(
              "size",
              juvenile_selection_constant,
              inbreeding_sensitivity
            )
          message("  Surviving juveniles: ", nrow(juveniles), "/", n_juveniles)
        }
        if(nrow(adults) > 0){
          adults <- adults %>%
            survive(adult_survival_prob, inbreeding_sensitivity)
          message("  Surviving adults: ", nrow(adults), "/", n_adults)
        }
        # prepare survivors for disturbance
        survivors <- bind_rows(
          juveniles, adults
        )
        n_survivors <- nrow(survivors)
        # output
        if(n_survivors > 0){
          # disturbance only occurs in generations
          # that are divisible by the frequency.
          if(generation %% disturbance_freq == 0){
            before <- n_survivors
            survivors <- survivors %>% disturb(
              disturbance_mortality_prob,
              disturbance_xlim,
              grid_size
            )
            n_survivors <- nrow(survivors)
            message("  Disturbance killed: ", before - n_survivors)
          } else {
            message("  No disturbance this generation.")
          }
          message("  Total survivors: ", n_survivors)
          if(n_survivors < 1 & nrow(seeds) < 1){
            # extinction caused by disturbance
            message("  *** EXTINCTION ***")
            message("  Ending simulation.")
            break
          }
        } else if(n_survivors < 1 & nrow(seeds) < 1){
          # extinction caused by winter
          message("  *** EXTINCTION ***")
          message("  Ending simulation.")
          break
        }
        # update life stage subsets with survivors
        juveniles <- survivors %>% filter(
          life_stage == 1
        )
        adults <- survivors %>% filter(
          life_stage == 2
        )
        toc(log = T, quiet = T)
      } else {
        # gen 1 should only have starting pop
        stopifnot(
          nrow(this_gen) == pop_size
        )
      }

      # germination
      message("Germination:")
      if(nrow(seeds) > 0){
        tic("Germination")
        # decide which seeds germimate
        if(seed_survival_prob == 0){
          # we decided the germination fate already to reduce computation
          # so germinate all seeds
          seeds <- seeds %>% germinate(1)
          message("  Germinating all seeds (germination fate already decided).")
        } else {
          # germination needs to happen as usual
          seeds <- seeds %>% germinate(germination_prob)
        }
        new_juveniles <- seeds %>% filter(
          life_stage == 1
        )
        # and which don't
        seeds <- seeds %>% filter(
          life_stage == 0
        )
        n_seeds <- nrow(seeds)
        # then decide if ungerminated seeds survive
        if(n_seeds > 0){
          seeds <- seeds %>% survive(seed_survival_prob)
          message("  Surviving ungerminated seeds: ", nrow(seeds), "/", n_seeds)
        }
        # combine new juveniles with any surivors from last gen
        if(nrow(new_juveniles) > 0){
          message("  New juveniles created: ", nrow(new_juveniles))
          juveniles <- bind_rows(
            juveniles, new_juveniles
          )
          message("  Juvenile total: ", nrow(juveniles), ".")
        } else {
          message("  No seeds germinated.")
        }
        toc(log = T, quiet = T)
      } else {
        message("  No seeds to germinate.")
      }

      # growth
      message("Growth:")
      tic("Growth")
      # combine all plants that are able to grow
      these_plants <- bind_rows(juveniles, adults)
      if(nrow(these_plants) > 0){
        message("  Growth rate min: ", round(min(these_plants$growth_rate), 3))
        message("  Growth rate max: ", round(max(these_plants$growth_rate), 3))
        message("  Adults before growth: ", nrow(adults))
        # grow plants
        these_plants <- these_plants %>% grow("individuals")
        # resubset based on new size
        juveniles <- these_plants %>% filter(
          size < adult_size
        )
        adults <- these_plants %>% filter(
          size >= adult_size
        )
        message("  Adults after growth: ", nrow(adults))
        # and update life stages
        if(nrow(adults) > 0){
          if(clonal_growth){
            message("  Juveniles before clonal growth: ", nrow(juveniles))
            # clone plants
            # use new object so we can count the new ramets
            clones <- adults %>% grow(
              "clones", adult_size
            )
            # make sure clones are in adjacent cells
            clones <- clones %>% move(grid_size, always_away = T)
            # recombine all juveniles
            juveniles <- bind_rows(
              juveniles, clones
            )
            message("  Juveniles after clonal growth: ", nrow(juveniles))
            message("  (", nrow(clones), " new ramets.)")
          }
          adults$life_stage <- 2
        }
        message("  Plant size min: ", round(min(these_plants$size), 3))
        message("  Plant size max: ", round(max(these_plants$size), 3))
      } else {
        message("  No plants ready to grow.")
      }
      toc(log = T, quiet = T)

      # control population size with carrying capacity (K)
      message("Competition (between adults):")
      message("  K = ", carrying_capacity)
      message("  Size increases chances.")
      # get the competitors
      competitors <- adults
      if(nrow(competitors) > 0){
        tic("Competition")
        # Work out N
        N <- competitors %>%
          group_by(X, Y) %>%
          tally() %>%
          pull(n)
        # Subset those that don't compete
        competitors <- competitors %>% nest_by_location()
        non_competitors <- competitors[which(N <= carrying_capacity), ]
        # from those that do...
        competitors <- competitors[which(N > carrying_capacity), ]
        message("  Populated locations with competition: ", nrow(competitors))
        message("  Populated locations without competition: ", nrow(non_competitors))
        # only control population if needed
        if(nrow(competitors) > 0){
          # reduce to just winners
          winners <- apply(
            competitors, 1,
            compete,
            carrying_capacity,
            "size"
          )
          # make sure we have less winners than competitors
          stopifnot(
            nrow(winners) < nrow(competitors)
          )
          # replace whole column
          competitors$plants <- winners
          message("  Loosers removed.")

          non_competitors <<- non_competitors
          competitors <<- competitors
          # put the adults back together
          adults <- bind_rows(
            competitors, non_competitors
          ) %>% unnest()
        }
        toc(log = T, quiet = T)
      } else {
        message("  No adults to compete.")
      }

      # reproduction
      message("Reproduction:")
      message("  Adults ready to reproduce: ", nrow(adults))
      if(nrow(adults) > 0){
        tic("Reproduction")
        new_seeds <- adults %>% reproduce(
          N_ovules,
          pollen_range,
          fertilisation_prob,
          uneven_matching_prob,
          selfing_polyploid_prob,
          selfing_diploid_prob,
          triploid_mum_prob,
          generation, # generation used for seed ID
          genome_size,
          ploidy_prob,
          mutation_rate,
          grid_size,
          germination_prob,
          seed_survival_prob
        )
        # make sure we have some new seeds
        # (stop any mutate errors)
        if(!is.logical(new_seeds)){
          # before counting rows and continuing
          if(nrow(new_seeds) > 0){
            # ensure generation and simulation data correct
            new_seeds <- new_seeds %>% mutate(
              gen = as.integer(generation),
              sim = as.integer(this_sim)
            )
            # calculate growth rates
            new_seeds$growth_rate <- sapply(
              new_seeds$genome, get_growth_rate
            )
            # then do seed dispersal
            new_seeds <- new_seeds %>% move(grid_size)
            message("  Seeds dispersed.")
            # and combine with old seeds
            seeds <- bind_rows(seeds, new_seeds)
            message("  Total seeds in system: ", nrow(seeds))
          }
        } else {
          message("  No new seeds created.")
        }
        toc(log = T, quiet = T)
      }
      # store data
      this_gen <- bind_rows(
        seeds, juveniles, adults
      )
      # make sure we have some population to continue with
      if(nrow(this_gen) > 0 ){
        # combine with data so far
        dploidy$data <- bind_rows(
          dploidy$data,
          this_gen
        )
        # update RDA and RDS file every generation
        usethis::use_data(dploidy, overwrite = T)
        if(!is.null(filename)){
          # make sure it's allowed characters
          stopifnot(
            is.character(filename)
          )
          rds <- paste0(filepath, filename, ".rds")
          saveRDS(dploidy, rds)
          message("  ", rds, " saved too!")
        }
        this_gen <- NULL
        if(!is.null(logfilename)){
          # Allow minimal screen output for each generation being run.
          sink(type = "message")
          message(paste(tic.log(), collapse = "\n"))
          sink(logfile, append = T, type = "message")
          toc(log = T, quiet = T)
        } else {
          toc(log = F, quiet = F)
        }
        message(paste(tic.log(), collapse = "\n"))
        tic.clearlog()
        message("+++++++++++++++++++++++++++")
      } else {
        # extinction
        message("  *** EXTINCTION ***")
        message("  Ending simulation.")
        break
      }
    }
    # simulation loop ending
    toc(log = T, quiet = F)
    if(!is.null(logfilename)){
      sink(type = "message")
      close(logfile)
    }
  }
  # format the final data
  dploidy$data$ID <- as.factor(dploidy$data$ID)
  dploidy$data$life_stage <- as.factor(dploidy$data$life_stage)
  dploidy$data$gen <- as.factor(dploidy$data$gen)
  dploidy$data$sim <- as.factor(dploidy$data$sim)
  dploidy$time$end <- Sys.time()
  usethis::use_data(dploidy, overwrite = T)
  message("HINT: load with `data(dploidy)`")
  # Save a non-temp custom file
  if(!is.null(filename)){
    # make sure it's allowed characters
    stopifnot(
      is.character(filename)
    )
    message(rds, " saved too!")
    message("HINT: load with `whatever <- readRDS(", rds, ")`")
    saveRDS(dploidy, rds)
  }
  toc(log = F, quiet = F)
  # only return if requested
  if(return){
    return(dploidy)
  }
}
