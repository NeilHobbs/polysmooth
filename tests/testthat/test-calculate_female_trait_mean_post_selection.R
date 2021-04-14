test_that("trait mean cannot fall below zero", {
  expect_equal(calculate_female_trait_mean_post_selection(female.population.size.exposed.survivors = 0.1,
                                                          female.trait.mean.exposed.survivors = -10,
                                                          female.population.size.unexposed = 1,
                                                          female.trait.mean = -10,
                                                          female.population.size.after.selection = 1), 0)
})


test_that("equation works properly", {
  expect_equal(calculate_female_trait_mean_post_selection(female.population.size.exposed.survivors = 1,
                                                          female.trait.mean.exposed.survivors = 10,
                                                          female.population.size.unexposed = 1,
                                                          female.trait.mean = 10,
                                                          female.population.size.after.selection = 2), 10)
})
