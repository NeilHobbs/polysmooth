test_that("addition works", {
  expect_equal(calculate_female_population_size_after_selection(female.population.size.unexposed = 10,
                                                                female.population.size.exposed.survivors = 10), 20)
})

test_that("NA becomes zero", {
  expect_equal(calculate_female_population_size_after_selection(female.population.size.unexposed = NA,
                                                                female.population.size.exposed.survivors = 10), 10)
  expect_equal(calculate_female_population_size_after_selection(female.population.size.unexposed = 10,
                                                                female.population.size.exposed.survivors = NA), 10)
  expect_equal(calculate_female_population_size_after_selection(female.population.size.unexposed = NA,
                                                                female.population.size.exposed.survivors = NA), 0)
})

