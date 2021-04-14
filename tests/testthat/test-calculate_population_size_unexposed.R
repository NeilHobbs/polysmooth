test_that("calculates correct unexposed population", {
  expect_equal(calculate_population_size_unexposed(total.population.size = 10,
                                                   insecticide.exposure = 0.3), 7)
})


test_that("error when insecticide.exposure too low", {
  expect_error(calculate_population_size_unexposed(total.population.size = 10,
                                                   insecticide.exposure = -0.1), "insecticide.exposure must be between 0 and 1")
})


test_that("error when insecticide.exposure too high", {
  expect_error(calculate_population_size_unexposed(total.population.size = 10,
                                                   insecticide.exposure = 1.1), "insecticide.exposure must be between 0 and 1")
})
