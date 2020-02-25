#' @name sploidy
#' @title What are the relative densities of polyploids vs diploids?
#' @description A spatially explicit individual-based model which runs a simulation, or repeated simulations, of a plant population over time.
#' @usage simpleploidy()
#' @author Rose McKeon
#' @param pop_size integer representing starting population size, all individuals begin as juveniles (default = 500).
#' @param grid_size integer representing the size of the landscape grid. Cells are numbered 0 to grid_size -1 along an X and Y axis (default = 10, so the grid is 10 x 10).
#' @param carrying_capacity integer representing K, the carrying capacity (max population size) of any given cell. Seeds and juveniles are not taken into account for K, only adults who compete for resouces after growth (which creates adults) but before reproduction (default = 1, so only 1 new adult per square can survive to reproduce).
#'
#' @param seed_dispersal_range whole number between 0 and grid_size - 1 representing the maximum distance a seed can travel (default = 9).
#' @param adult_survival_prob number between 0 and 1 representing survival probability of adults between generations (default = 0.5).
#' @param juvenile_selection_constant number representing the constant which converts trait values into probabilities. Used to select for plants wth higher growth rates by weighting survival chances of larger juveniles between generations (default = 0.1).
#' @param seed_survival_prob number between 0 and 1 representing survival probability of seeds between generations (default = 0, so there is no seedbank). New seeds are pooled with surviving seeds from previous generations after reproduction. Survival takes place before germination.#' @param ploidy_prob number between 0 and 1 representing the chance that genome duplication will occur (default = 0, so no genome duplication).
#' @param mutation_rate number between 0 and 1 representing the chance any given allele will mutate (default = 0.001).
#' @param generations integer representing the number of generations the model should attempt to run for (default = 10). The simulation will break early if extinction occurs.
#' @param simulations integer representing the number of simulations which should be run with these parameters (default = 1).
#' @param return logical value which indicates whether or not to return output at the end of the simulation/s.
#' @param filepath character string defining the file path where output files should be stored. Only used if filename not NULL (default = "data/").
#' @param filename character string defining the name of the output file. Output files are RDS format and the file extension will be appended automatically (default = NULL).
#' @param logfilepath character string defining the file path where output log files should be stored. Only used if logfilename not NULL (default = "data/logs/").
#' @param logfilename character string defining the name of the output log file. Log files are txt format and the file extension will be appended automatically (default = NULL).
#' @return if return == T, a dataframe of all simulations will be returned showing the population state at the end of each generation (immediately after reproduction, before survival). If return == F, data/sploidy.rda will be stored automatically and can be accessed with `data(dploidy)`.
#' @examples
#' # with default parameters
#' sploidy()
#' data(sploidy)
#' sploidy
#'
#' # with minimal console output
#' # (the rest logged to TXT files)
#' sploidy(logfilename = "whatever")
#'
#' # with stored data object as RDS file
#' sploidy(filename = "whatever")
#'
#' # assigning output to a new object
#' whatever <- sploidy(return = T)
#'
#' @export