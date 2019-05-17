# Running the model
#---------------------------

#' @name disturploidy
#' @details Runs the model to determine an answer to our question.
#' @author Rose McKeon
#' @param grid_size size of the landscape grid
#' @param pop_size size of the starting population
#' @return population data for each generation
disturploidy <- function(
  pop_size = 100,
  grid_size = 100
){
  # populate landscape
  pop <- populate_landscape(pop_size, grid_size)
  # return data
  return(pop)
}
# run the model
disturploidy()