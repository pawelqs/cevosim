

new_cevo_population <- function(obj, sim_params, comment) {
  attr(obj, "sim_params") <- sim_params
  attr(obj, "comment") <- comment
  structure(obj, class = c("cevo_population", "list"))
}


#' @export
print.cevo_population <- function(x, ...) {
  msg("<cevo_population object>")

  params <- unlist(attr(x, "sim_params"))
  msg(str_c(names(params), ": ", params, collapse = " | "))

  msg("Number of cells: ", length(x))
  msg(attr(x, "comment"))
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
