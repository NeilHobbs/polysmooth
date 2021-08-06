test_that("addition works", {
  expect_equal(calculate_population_size_post_selection(population.size.exposed.survivors =2,
                                                        population.size.unexposed =2), 4)
})


test_that("addition works NAs treated as zeroes", {
  expect_equal(calculate_population_size_post_selection(population.size.exposed.survivors =NA,
                                                        population.size.unexposed =2),2)

  expect_equal(calculate_population_size_post_selection(population.size.exposed.survivors =2,
                                                        population.size.unexposed =NA), 2)
})
