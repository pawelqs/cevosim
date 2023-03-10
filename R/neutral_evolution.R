
#' Simulate neutrally evolving population with constant mutation rate
#' @param b birth rate
#' @param d death rate
#' @param u_init initial mutation rate
#' @param start_cells init population size
#' @param max_cells maximal population size
#' @param verbose verbose
#' @return popolation list
#' @export
simulate_neutral_evolution <- function(b = 0.1, d = 0.05,
                                       u_init = 1,
                                       start_cells = 1,
                                       max_cells = 100000,
                                       verbose = FALSE) {
  init_population <- as.list(rep(1, start_cells))
  population <- init_population

  i <- 0
  mut_counter <- 1
  while (length(population) < max_cells) {
    msg("Iteration ", i, ", population size: ", length(population), verbose = verbose)

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

  population
}


#' Get SNVs from population of cells
#' @param population population
#' @export
get_snvs <- function(population) {
  all_muts <- tibble(
    sample_id = "sample",
    chrom = "chr",
    pos = reduce(population, c)
  )

  snvs <- all_muts |>
    count(.data$sample_id, .data$chrom, .data$pos) |>
    mutate(VAF = .data$n / length(population) / 2) |>
    select(-"n")

  class(snvs) <- c("cevo_snvs", class(snvs))
  snvs
}
