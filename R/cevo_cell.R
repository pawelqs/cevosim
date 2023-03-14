#
#
# new_cevo_cell <- function(cell) {
#   structure(cell, class = c("cevo_cell", "numeric"))
# }
#
#
# cevo_cell <- function(u_init, muts = 1) {
#   cell <- c(muts)
#   attr(cell, "u") <- u_init
#   new_cevo_cell(cell)
# }
#
#
# # @export
# print.cevo_cell <- function(x, ...) {
#   msg("<cevo_dell> with mutation rate: ", attr(x, "u"), " and ", length(x), " mutation(s)")
#   if (length(x) < 20) {
#     cat(x)
#   } else {
#     cat(x[1:20])
#     msg(" and more")
#   }
# }
#
#
# # @export
# mutate.cevo_cell <- function(.data, next_pos, ...) {
#   # mutation_rate <- get_mutation_rate(.data)
#   # muts <- c(.data, next_pos:(next_pos + (mutation_rate - 1))
#   muts <- c(.data, next_pos)
#   # cevo_cell(mutation_rate, muts)
#   cevo_cell(1, muts)
# }
#
#
# get_mutation_rate <- function(cell) {
#   attr(cell, "u")
# }
