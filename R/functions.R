# functions for the model

#' @name populate_landscape
#' @details Populates the landscape grid with individuals
#' @author Rose McKeon
#' @param pop_size integer value for number of starting individuals
#' @param grid_size integer value for size of landscape grid
#' @return list of individuals and their relevant data objects (location and genome)
#' @usage populate_landscape(100, 100)
populate_landscape <- function(pop_size = 100, grid_size = 100){
  # error handling
  if(!is.numeric(pop_size)){
    stop("Please make sure pop_size is a number.")
  }
  if(!is.numeric(grid_size)){
    stop("Please make sure grid_size is a number.")
  }
  if(is.numeric(pop_size) & !pop_size%%1==0){
    stop("Please make sure pop_size does not contain decimals.")
  }
  if(is.numeric(grid_size) & !grid_size%%1==0){
    stop("Please make sure grid_size does not contain decimals.")
  }
  # setup landscape vector
  grid = 1:grid_size
  # setup population with locations and genomes
  for(i in 1:pop_size){
    pop[[i]] <- list(
      genome = create_genome(),
      location = list(
        X = sample(grid, 1),
        Y = sample(grid, 1)
      )
    )
  }
  # return the finished population list
  return(pop)
}

#' @name create_genome
#' @details Creates dipload genomes using random numbers.
#' @author Rose McKeon
#' @param genome_size integer value for number of alleles in genome
#' @return data frame of length genome_size with 2 columns representing alelle pairs.
#' @usage create_genome()
create_genome <- function(genome_size = 100){
  return <- data.frame(
    A1 = runif(genome_size, 0, 100),
    A2 = runif(genome_size, 0, 100)
  )
}