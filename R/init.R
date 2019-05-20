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
  generations = 1
){
  out <- list()
  # populate landscape
  out$pop_0 <- populate_landscape(pop_size, grid_size)
  # advance time
  for(i in 1:generations){
    # clonal growth of annual plants
    out[[paste0("pop_", i)]] <- lapply(
      out[[paste0("pop_", i-1)]], find_coordinates, move, 1
    )
  }
  # return data
  return(out)
}

# run the model
disturploidy()