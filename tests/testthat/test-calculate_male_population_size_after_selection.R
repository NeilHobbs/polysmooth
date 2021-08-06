test_that("addition works", {
  expect_equal(calculate_male_population_size_after_selection(male.population.size.exposed.survivors =2,
                                                              male.population.size.unexposed = 2), 4)
})


test_that("addition works NA removed", {
  expect_equal(calculate_male_population_size_after_selection(male.population.size.exposed.survivors =NA,
                                                              male.population.size.unexposed = 2), 2)
    expect_equal(calculate_male_population_size_after_selection(male.population.size.exposed.survivors =2,
                                                              male.population.size.unexposed = NA), 2)



})
