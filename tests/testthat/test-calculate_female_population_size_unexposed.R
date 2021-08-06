
test_that("multiplication works", {
  expect_equal(calculate_female_population_size_unexposed(total.female.population.size = 100,
                                                          female.insecticide.exposure = 0.7), 30)
})
