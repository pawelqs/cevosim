

new_cevo_population <- function(cells, mut_rates, ...) {
  lst(cells, mut_rates, ...) |>
    structure(class = c("cevo_population", "list"))
}



#' cevo_population constructor
#' @param cells list of numeric vectors, where ech vector represents positins
#'   of mutations in cell
#' @param mut_rates vector of non-zero, integer values of  the same length as
#'   `cells`, with mutation rates.
#' @param params list of params to save into the object
#' @export
cevo_population <- function(cells, mut_rates, params = NULL) {
  if (length(cells) != length(mut_rates)) {
    stop("cells and mut_rates must have equal lengths")
  }
  new_cevo_population(cells, mut_rates, params)
}


#' Init cevo_population object
#' @param start_cells initial number of cells
#' @param u_init initial mutation rate
#' @export
init_population <- function(start_cells, u_init = 1) {
  cells <- as.list(rep(1, start_cells))
  mut_rates <- rep(u_init, start_cells)
  params <- lst(start_cells, u_init)
  cevo_population(cells, mut_rates, params)
}


#' Sample cells in the population
#' @param population population
#' @param size how many cells to draw?
#' @export
sample_cells <- function(population, size = 5000) {
  indexes <- sample(1:size(population), size = size)
  cevo_population(
    get_cells(population)[indexes],
    get_mutation_rates(population)[indexes],
    c(get_params(population), size = size)
  )
}


#' @export
print.cevo_population <- function(x, ...) {
  msg("<cevo_population object>")
  if (!is.null(x$params)) {
    msg(str_c(names(x$params), ": ", x$params, collapse = " | "))
  }
  msg("Number of cells: ", size(x))
}


#' Get size of an object
#' @param obj object
#' @param ... other args
#' @export
size <- function(obj, ...) {
  UseMethod("size")
}


#' Get size of the population
#' @inheritParams size
#' @export
size.cevo_population <- function(obj, ...) {
  length(get_cells(obj))
}


#' Get SNVs from population of cells
#' @param population population
#' @export
get_snvs <- function(population) {
  all_muts <- tibble(
    sample_id = "sample",
    chrom = "chr",
    pos = reduce(get_cells(population), c)
  )

  snvs <- all_muts |>
    count(.data$sample_id, .data$chrom, .data$pos) |>
    mutate(VAF = .data$n / size(population) / 2) |>
    select(-"n")

  cevomod::as_cevo_snvs(snvs)
}


get_cells <- function(population) {
  population$cells
}


get_mutation_rates <- function(population) {
  population$mut_rates
}

get_params <- function(population) {
  population$params
}
