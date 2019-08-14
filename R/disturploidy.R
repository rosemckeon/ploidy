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
#' @param inbreeding_cost number between 0 and 1 representing the cost of inbreeding. If an individual is inbred it's survival probability will be reduced according to this figure: 0 = no cost, ie: normal survival chances, and 1 is full cost, ie: complete mortality. Inbreeding is determined by checking for homozygosity at a specific locus which can be set with inbreeding_locus.
#' @param germination_prob number between 0 and 1 representing the probability that any seed will germinate on cells which are not yet populated by adults.
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
#' @param seed_dispersal_range whole number between 0 and grid_size - 1 representing the maximum distance a seed can travel (default = 1).
#' @param adult_survival_prob number between 0 and 1 representing survival probability of adults between generations (default = 0, simulating monocarpic plants that die after sexual reproduction).
#' @param juvenile_selection_constant number representing the constant which converts trait values into probabilities. Used to select for plants wth higher growth rates by weighting survival chances of larger juveniles between generations (default = 0.25).
#' @param seed_survival_prob number between 0 and 1 representing survival probability of seeds between generations (default = .05). New seeds are pooled with surviving seeds from previous generations after reproduction. Survival takes place before germination.
#' @param disturbance_freq positive integer representing the frequency of disturbance, where 0 is never and any number greater represents a chance of disturbance equal to once in that many generations (default = 0). When disturbance occurs it increases juvenile and adult mortality over the winter survival period according to disturbance_mortality_prob.
#' @param disturbance_mortality_prob number between 0 and 1 representing the increased chance of death during a disturbance. This increased chance of mortality is applied to juveniles and adults during winter survival. So mortality increased by 0.75 reduces the juvenile_selection_constant and adult_survival_prob by 75 percent (default = 0.75).
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
  inbreeding_cost = .5,
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
  seed_dispersal_range = 1,
  adult_survival_prob = 0,
  juvenile_selection_constant = 0.25,
  seed_survival_prob = .5,
  disturbance_freq = 0,
  disturbance_mortality_prob = .75,
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
        inbreeding_cost,
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
        seed_dispersal_range,
        adult_survival_prob,
        juvenile_selection_constant,
        seed_survival_prob,
        disturbance_freq,
        disturbance_mortality_prob,
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
      seed_dispersal_range,
      disturbance_freq,
      generations,
      simulations
    )%%1==0,
    between(
      c(
        ploidy_growth_benefit,
        inbreeding_cost,
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
    between(seed_dispersal_range, 0, grid_size - 1),
    !any(inbreeding_locus == growth_rate_loci),
    length(inbreeding_locus) == 1,
    genome_size >= 2,
    genome_size >= max(growth_rate_loci),
    genome_size >= inbreeding_locus
  )
  # prepare an object for output
  dploidy <- list(
    call = match.call(),
    time = list(
      start = Sys.time(),
      end = NULL,
      duration = NULL,
      sim_duration = NULL
    ),
    R = R.version.string,
    "disturploidy" = "DisturPloidy version 0.0.0007",
    notes = "Dataframes contain all members of a generation that will face survival over the winter and go on to begin the next generation. Seedoutput and seedbank stored seperately so that fecundity can be calculated when dormancy exists. Seeds never have real genomes, instead they contain lineage details in $genome.",
    data = list(
      seedbank = NULL,
      juveniles = NULL,
      adults = NULL,
      seedoutput = NULL,
      disturbance = NULL
    )
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
    message("SIMULATION ", this_sim, ":")
    message("*************")
    message("Starting population of ", pop_size, " random seeds.")
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
      this_gen <- list(
        seedbank = NULL,
        juveniles = NULL,
        adults = NULL,
        seedoutput = NULL
      )
      if(generation > 1){
        # did seeds survive?
        if(!is.null(dploidy$data$seedbank)){
          this_gen$seedbank <- dploidy$data$seedbank %>%
            filter(gen == last_gen) %>%
            filter(sim == this_sim)
          # update gen data
          if(nrow(this_gen$seedbank) > 0){
            this_gen$seedbank <- this_gen$seedbank %>%
              mutate(gen = generation)
          }
        }
        # did juveniles survive and appear?
        if(!is.null(dploidy$data$juveniles)){
          this_gen$juveniles <- dploidy$data$juveniles %>%
            filter(gen == last_gen) %>%
            filter(sim == this_sim)
          # update gen data
          if(nrow(this_gen$juveniles) > 0){
            this_gen$juveniles <- this_gen$juveniles %>%
              mutate(gen = generation)
          }
        }
        # did adults survive and appear?
        if(!is.null(dploidy$data$adults)){
          this_gen$adults <- dploidy$data$adults %>%
            filter(gen == last_gen) %>%
            filter(sim == this_sim)
          # update gen data
          if(nrow(this_gen$adults) > 0){
            this_gen$adults <- this_gen$adults %>%
              mutate(gen = generation)
          }
        }
        # and did adults reproduce?
        if(!is.null(dploidy$data$seedoutput)){
          this_gen$seedoutput <- dploidy$data$seedoutput %>%
            filter(gen == last_gen) %>%
            filter(sim == this_sim)
          # update gen data
          if(nrow(this_gen$seedoutput) > 0){
            this_gen$seedoutput <- this_gen$seedoutput %>%
              mutate(gen = generation)
          }
        }

        # go on to do survival...
        message("Survival:")
        tic("Survival")
        # see if we have disturbance
        if(disturbance_freq > 0){
          disturbance <- rbinom(1, 1, 1/disturbance_freq) == 1
          if(disturbance){
            # change survival probabilities
            disturbance_survival_prob <- 1 - disturbance_mortality_prob
            juvenile_selection_constant <- juvenile_selection_constant * disturbance_survival_prob
            adult_survival_prob <- adult_survival_prob * disturbance_survival_prob
            message(
              "  Disturbance reduced survival chances by ",
              disturbance_mortality_prob * 100,
              " %."
            )
          }
          # keep a disturbance record
          dploidy$data$disturbance <- bind_rows(
            dploidy$data$disturbance,
            tibble(
              sim = as.integer(this_sim),
              gen = as.integer(generation),
              occurred = disturbance
            )
          )
        }
        # see if we have lifestages subject to winter survival in the population
        # seed survival occurs later (after germination)
        if(!is.null(this_gen$juveniles)){
          n_juveniles <- nrow(this_gen$juveniles)
          if(n_juveniles > 0){
            this_gen$juveniles <- this_gen$juveniles %>%
              hard_select(
                "size",
                juvenile_selection_constant,
                inbreeding_cost
              )
            message(
              "  Surviving juveniles: ",
              nrow(this_gen$juveniles), "/", n_juveniles
            )
          }
        }
        if(!is.null(this_gen$adults)){
          n_adults <- nrow(this_gen$adults)
          if(n_adults > 0){
            this_gen$adults <- this_gen$adults %>%
              survive(adult_survival_prob, inbreeding_cost)
            message(
              "  Surviving adults: ",
              nrow(this_gen$adults), "/", n_adults
            )
          }
        }
        # check for extinction
        total <- sum(
          nrow(this_gen$seedbank),
          nrow(this_gen$juveniles),
          nrow(this_gen$adults),
          nrow(this_gen$seedoutput)
        )
        if(total == 0){
          message("  *** EXTINCTION ***")
          message("  Ending simulation.")
          break
        }
        toc(log = T, quiet = T)
      }

      # germination
      message("Germination:")
      tic("Germination")
      n_juveniles <- 0
      if(last_gen == 0){
        # germinate from random seed population that has genomes
        # all starting seeds germinate so they never contribute to the seedbank
        # manual germination uses less computation than germinate() in this case
        new_juveniles <- populate_landscape(
          pop_size, grid_size, genome_size, this_sim,
          ploidy_growth_benefit, growth_rate_loci, max_growth_rate
          ) %>%
          mutate(gen = 1, size = 1, life_stage = 1)
        # and we don't need to really count either
        n_juveniles <- pop_size
      } else if(!is.null(this_gen$seedbank) | !is.null(this_gen$seedoutput)){
        # it's not the first gen
        seeds <- bind_rows(this_gen$seedbank, this_gen$seedoutput)
        # are there seeds from last gen?
        if(nrow(seeds) > 0){
          # see which seeds (without genomes) germinate
          seeds <- germinate(seeds, this_gen$adults)
          new_juveniles <- seeds %>%
            filter(life_stage == 1)
          # and which don't
          this_gen$seedbank <- seeds %>%
            filter(life_stage == 0)
          # then empty seedoutput as it's used up
          this_gen$seedoutput <- NULL
          # leave seeds without genomes but give them to juveniles
          if(nrow(new_juveniles) > 0){
            # get the right parent genomes
            if(seed_survival_prob > 0){
              # parents could be from any generation as we have dormancy
              parents <- dploidy$data$adults %>%
                filter(sim == this_sim)
            } else {
              # parents can only be from last generation
              parents <- dploidy$data$adults %>%
                filter(gen == last_gen) %>%
                filter(sim == this_sim)
            }
            new_juveniles <- fill_seeds_with_genomes(
              new_juveniles, parents, mutation_rate, genome_size
            )
            n_juveniles <- nrow(new_juveniles)
            # calculate growth rates from genomes
            new_juveniles$growth_rate <- sapply(
              new_juveniles$genome, get_growth_rate,
              ploidy_growth_benefit, growth_rate_loci, max_growth_rate
            )
            # and check for inbreeding
            new_juveniles$inbreeding <- sapply(
              new_juveniles$genome, get_inbreeding_value
            )
          }
        } else {
          message("  No seeds to germinate.")
        }
      } else {
        message("  No seeds to germinate.")
      }
      message("  New juveniles: ", n_juveniles)
      # store all the juveniles together with survivors
      if(n_juveniles > 0){
        # combine with survivors
        this_gen$juveniles <- bind_rows(
          this_gen$juveniles,
          new_juveniles
        )
      }
      # deal with ungerminated seeds
      if(!is.null(this_gen$seedbank)){
        n_seeds <- nrow(this_gen$seedbank)
        if(n_seeds > 0){
          this_gen$seedbank <- this_gen$seedbank %>% survive(seed_survival_prob)
          message(
            "  Surviving ungerminated seeds: ",
            nrow(this_gen$seedbank), "/", n_seeds
          )
        }
      }
      toc(log = T, quiet = T)

      # growth
      message("Growth:")
      tic("Growth")
      # combine all plants that are able to grow
      if(!is.null(this_gen$juveniles) | !is.null(this_gen$adults)){
        these_plants <- bind_rows(this_gen$juveniles, this_gen$adults)
        if(nrow(these_plants) > 0){
          message("  Growth rate min: ", round(min(these_plants$growth_rate), 3))
          message("  Growth rate max: ", round(max(these_plants$growth_rate), 3))
          if(!is.null(this_gen$adults)){
            message("  Adults before growth: ", nrow(this_gen$adults))
          } else {
            message("  Adults before growth: ", 0)
          }
          # grow plants
          these_plants <- these_plants %>% grow("individuals", adult_size)
          # store the results
          this_gen$juveniles <- these_plants %>%
            filter(life_stage == 1)

          this_gen$adults <- these_plants %>%
            filter(life_stage == 2)

          message("  Adults after growth: ", nrow(this_gen$adults))
          # do we have adults?
          if(nrow(this_gen$adults) > 0){
            # clones if turned on
            if(clonal_growth){
              message("  Juveniles before clonal growth: ", nrow(this_gen$juveniles))
              # clone plants
              these_plants <- this_gen$adults %>% grow("clones")
              # store the results
              new_juveniles <- these_plants %>%
                filter(life_stage == 1)

              this_gen$juveniles <- bind_rows(
                this_gen$juveniles, new_juveniles
              )
              message("  Juveniles after clonal growth: ", nrow(this_gen$juveniles))
              message("  (", nrow(new_juveniles), " new ramets.)")
            }
          }
        } else {
          message("  No plants ready to grow.")
        }
      }
      toc(log = T, quiet = T)

      # control population size with carrying capacity (K)
      message("Competition (between adults):")
      message("  K = ", carrying_capacity)
      message("  Size increases chances.")
      # get the competitors
      competitors <- this_gen$adults
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
          # put the adults back together
          this_gen$adults <- bind_rows(
            competitors, non_competitors
          ) %>% unnest()
          # if we have juveniles
          if(nrow(this_gen$juveniles) > 0){
            # update the juveniles to include removals
            # (if adults emerge on a cell, only they can persist)
            this_gen$juveniles <- get_those_not_outcompeted(
              this_gen$adults, this_gen$juveniles,
              "Juveniles outcompeted by adults in the same location: "
            )
            message("  Juveniles remaining: ", nrow(this_gen$juveniles))
          }
        }
        toc(log = T, quiet = T)
      } else {
        message("  No adults to compete.")
      }

      # reproduction
      message("Reproduction:")
      message("  Adults ready to reproduce: ", nrow(this_gen$adults))
      if(nrow(this_gen$adults) > 0){
        tic("Reproduction")
        new_seeds <- this_gen$adults %>%
          reproduce(
            N_ovules,
            pollen_range,
            fertilisation_prob,
            uneven_matching_prob,
            selfing_polyploid_prob,
            selfing_diploid_prob,
            triploid_mum_prob,
            generation,
            ploidy_prob,
            grid_size
          )
        # make sure we have some new seeds
        # (stop any mutate errors)
        if(!is.logical(new_seeds)){
          # before counting rows and continuing
          if(nrow(new_seeds) > 0){
            message("  Seed output (F): ", nrow(new_seeds))
            # ensure generation and simulation data correct
            new_seeds <- new_seeds %>% mutate(
              gen = as.integer(generation),
              sim = as.integer(this_sim)
            )
            # then do seed dispersal and store
            this_gen$seedoutput <- new_seeds %>% move(grid_size, range = seed_dispersal_range)
            message("  Seeds dispersed.")
          }
        } else {
          message("  Seed output (F): 0")
        }
        toc(log = T, quiet = T)
      }
      # store the results of this generation
      total <- sum(
        nrow(this_gen$seedbank),
        nrow(this_gen$juveniles),
        nrow(this_gen$adults),
        nrow(this_gen$seedoutput)
      )
      # make sure we have some population to continue with
      if(total > 0 ){
        # combine with data so far
        if(!is.null(this_gen$seedbank)){
          dploidy$data$seedbank <- bind_rows(
            dploidy$data$seedbank,
            this_gen$seedbank
          )
        }
        if(!is.null(this_gen$juveniles)){
          dploidy$data$juveniles <- bind_rows(
            dploidy$data$juveniles,
            this_gen$juveniles
          )
        }
        if(!is.null(this_gen$adults)){
          dploidy$data$adults <- bind_rows(
            dploidy$data$adults,
            this_gen$adults
          )
        }
        if(!is.null(this_gen$seedoutput)){
          dploidy$data$seedoutput <- bind_rows(
            dploidy$data$seedoutput,
            this_gen$seedoutput
          )
        }
        # update RDA or RDS file every generation
        # don't bother in the last sim generation as end of sim saves too
        #if(this_sim != simulations & generation != generations){
          if(!is.null(filename)){
            stopifnot(
              is.character(filename)
            )
            rds <- paste0(filepath, filename, ".rds")
            saveRDS(dploidy, rds)
            sink(type = "message")
            message("SAVED DATA: ", rds)
            sink(logfile, append = T, type = "message")
          } else {
            usethis::use_data(dploidy, overwrite = T)
          }
        #}
        # data updated so wipe this_gen for next loop
        this_gen <- NULL
        if(!is.null(logfilename)){
          # send some timing output to screen
          toc(log = T, quiet = T)
          sink(type = "message")
          message(paste(tic.log(), collapse = "\n"))
          sink(logfile, append = T, type = "message")
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
  if(!is.null(dploidy$data$seedbank)){
    dploidy$data$seedbank$ID <- as.factor(dploidy$data$seedbank$ID)
    dploidy$data$seedbank$life_stage <- as.factor(dploidy$data$seedbank$life_stage)
    dploidy$data$seedbank$sim <- as.factor(dploidy$data$seedbank$sim)
  }

  if(!is.null(dploidy$data$juveniles)){
    dploidy$data$juveniles$ID <- as.factor(dploidy$data$juveniles$ID)
    dploidy$data$juveniles$life_stage <- as.factor(dploidy$data$juveniles$life_stage)
    dploidy$data$juveniles$sim <- as.factor(dploidy$data$juveniles$sim)

  }
  if(!is.null(dploidy$data$adults)){
    dploidy$data$adults$ID <- as.factor(dploidy$data$adults$ID)
    dploidy$data$adults$life_stage <- as.factor(dploidy$data$adults$life_stage)
    dploidy$data$adults$sim <- as.factor(dploidy$data$adults$sim)

  }
  if(!is.null(dploidy$data$seedoutput)){
    dploidy$data$seedoutput$ID <- as.factor(dploidy$data$seedoutput$ID)
    dploidy$data$seedoutput$life_stage <- as.factor(dploidy$data$seedoutput$life_stage)
    dploidy$data$seedoutput$sim <- as.factor(dploidy$data$seedoutput$sim)
  }

  # log times
  dploidy$time$end <- Sys.time()
  dploidy$time$duration <- (dploidy$time$end - dploidy$time$start)
  dploidy$time$sim_duration <- dploidy$time$duration / simulations

  # save files
  if(!is.null(filename)){
    stopifnot(
      is.character(filename)
    )
    rds <- paste0(filepath, filename, ".rds")
    saveRDS(dploidy, rds)
    message("SAVED DATA: ", rds)
    message('load with: readRDS("', rds, '")')
  } else {
    usethis::use_data(dploidy, overwrite = T)
    message("load with: data(dploidy)")
  }
  # only return if requested
  if(return){
    return(dploidy)
  }
}
