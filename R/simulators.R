

#' Simulate neutrally evolving population with constant mutation rate
#' @param b birth rate
#' @param d death rate
#' @param start_cells init population size
#' @param max_cells maximal population size
#' @param verbose verbose
#' @return popolation list
#' @export
simulate_neutral_evolution <- function(b = 0.1,
                                       d = 0.05,
                                       start_cells = 1,
                                       max_cells = 100000,
                                       verbose = TRUE) {
  start_time <- Sys.time()
  population <- init_population(start_cells, 1)
  population <- population$cells

  i <- 0
  mut_counter <- 1
  while (length(population) < max_cells) {
    msg("Iteration ", i, ", population size: ", length(population), verbose = verbose == 2)

    to_born <- round(b * length(population))
    to_kill <- round(d * length(population))
    to_born <- if (to_born == 0) 1 else to_born
    to_kill <- if (to_kill == to_born) to_kill - 1 else to_kill

    new_born <- sample(population, size = to_born)
    for (cell in seq_along(new_born)) {
      new_born[[cell]] <- c(new_born[[cell]], mut_counter + 1)
      mut_counter <- mut_counter + 1
    }

    dead <- sample(seq_along(population), to_kill)
    if (length(dead) > 0) {
      population <- population[-dead]
    }
    population <- c(population, new_born)
    i <- i + 1
  }

  duration_time <- Sys.time() - start_time
  msg(end_message(i, duration_time), verbose = verbose)
  sim_params <- lst(b, d, u = 1, start_cells, max_cells)
  mut_rates <- rep(1, length(population))
  cevo_population(population, mut_rates, sim_params)
}




#' Simulate neutrally evolving population with constant mutation rate
#' @param b birth rate
#' @param d death rate
#' @param u_init initial mutation rate
#' @param u_increase_p probability than mutation increases mut. rate by +1
#' @param start_cells init population size
#' @param max_cells maximal population size
#' @param verbose verbose
#' @return popolation list
#' @export
simulate_increasing_mut_rate <- function(b = 0.1,
                                         d = 0.05,
                                         u_init = 1,
                                         u_increase_p = 0.1,
                                         start_cells = 1,
                                         max_cells = 100000,
                                         verbose = TRUE) {
  start_time <- Sys.time()
  population <- init_population(start_cells, u_init)

  i <- 0
  next_mut <- 2
  while (size(population) < max_cells) {
    msg("Iteration ", i, ", population size: ", length(population), verbose = verbose == 2)

    to_born <- round(b * length(population$cells))
    to_kill <- round(d * length(population$cells))
    to_born <- if (to_born == 0) 1 else to_born
    to_kill <- if (to_kill == to_born) to_kill - 1 else to_kill

    new_born_indexes <- sample(1:length(population$cells), size = to_born)
    new_born <- population$cells[new_born_indexes]
    mut_rates <- population$mut_rates[new_born_indexes]
    for (cell in seq_along(new_born)) {
      new_muts <- next_mut:(next_mut + mut_rates[[cell]] - 1)
      new_born[[cell]] <- c(new_born[[cell]], new_muts)
      next_mut <- next_mut + mut_rates[[cell]]
    }

    increase_mut_rate <- sample(seq_along(mut_rates), size = round(u_increase_p * to_born))
    mut_rates[increase_mut_rate] <- mut_rates[increase_mut_rate] + 1

    dead <- sample(seq_along(population$cells), to_kill)
    if (length(dead) > 0) {
      population$cells <- population$cells[-dead]
      population$mut_rates <- population$mut_rates[-dead]
    }
    population$cells <- c(population$cells, new_born)
    population$mut_rates <- c(population$mut_rates, mut_rates)
    i <- i + 1
  }

  duration_time <- Sys.time() - start_time
  msg(end_message(i, duration_time), verbose = verbose)
  sim_params <- lst(b, d, u_init, start_cells, max_cells)
  cevo_population(population$cells, population$mut_rates, sim_params)
}


end_message <- function(iters, difftime) {
  str_c(
    "Simulation finished in ", iters, " iterations, in ",
    format(difftime)
  )
}

