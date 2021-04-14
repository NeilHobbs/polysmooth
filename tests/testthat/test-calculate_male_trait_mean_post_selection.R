test_that("trait mean cannot fall below zero", {
  expect_equal(calculate_male_trait_mean_post_selection(male.population.size.exposed.survivors = 0.1,
                                                          male.trait.mean.exposed.survivors = -10,
                                                          male.population.size.unexposed = 1,
                                                          male.trait.mean = -10,
                                                          male.population.size.after.selection = 1), 0)
})


test_that("equation works properly", {
  expect_equal(calculate_male_trait_mean_post_selection(male.population.size.exposed.survivors = 1,
                                                          male.trait.mean.exposed.survivors = 10,
                                                          male.population.size.unexposed = 1,
                                                          male.trait.mean = 10,
                                                          male.population.size.after.selection = 2), 10)
})
