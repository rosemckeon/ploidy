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
  grid_size = 100
){
  # populate landscape
  pop_0 <- populate_landscape(pop_size, grid_size)
  # advance time
  pop_1 <- lapply(pop_0, find_coordinates, move)
  # return data
  return(
    list(
      pop_0 = pop_0,
      pop_1 = pop_1
    )
  )
}

# run the model
disturploidy()