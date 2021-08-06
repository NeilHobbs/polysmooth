test_that("equation works", {
  expect_equal(calculate_male_population_size_unexposed(total.male.population.size = 100,
                                                        male.insecticide.exposure = 0.5,
                                                        female.insecticide.exposure = 1), 50)
})
