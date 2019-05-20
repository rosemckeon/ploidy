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
    # take the generation before i
    current_gen <- out[[paste0("pop_", i-1)]]
    # and apply clonal growth (disperses population)
    new_gen <- lapply(
      current_gen, find_coordinates, move, 1
    )
    # don't add to current generation as they all die
    # label new generation and store
    out[[paste0("pop_", i)]] <- new_gen
  }
  # return data
  return(out)
}

# run the model
disturploidy()