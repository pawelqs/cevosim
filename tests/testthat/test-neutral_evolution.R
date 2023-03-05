
population <- simulate_neutral_evolution(
  b = 0.1, d = 0.090,
  start_cells = 1,
  max_cells = 1000,
  verbose = FALSE
)

test_that("simulate_neutral_evolution() works", {
  expect_true(length(population) > 1000)
  expect_type(population, "list")
})


test_that("get_snvs() works", {
  snvs <- get_snvs(population)
  expect_s3_class(snvs, "cevo_snvs")
})
