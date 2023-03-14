
b <- 0.1
d <- 0.05
u_init <- 1
u_increase_p <- 0.1
start_cells <- 1
max_cells <- 100000
verbose <- TRUE

population <- simulate_neutral_evolution(
  b = 0.1, d = 0.090,
  start_cells = 1,
  max_cells = 1000,
  verbose = FALSE
)

test_that("simulate_neutral_evolution() works", {
  expect_true(size(population) > 1000)
  expect_s3_class(population, "cevo_population")
})



test_that("get_snvs() works", {
  snvs <- get_snvs(population)
  expect_s3_class(snvs, "cevo_snvs")
  expect_true(all(snvs$VAF < 0.51))
})
