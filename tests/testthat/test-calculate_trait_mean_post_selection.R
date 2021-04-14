test_that("trait mean cannot fall below zero", {
  expect_equal(calculate_trait_mean_post_selection(population.size.exposed.survivors = 0.1,
                                                   mean.score.exposed.survivors = -10,
                                                   population.size.unexposed = 1,
                                                   trait.mean = -10,
                                                   population.size.post.selection =1), 0)
})


test_that("equation works properly", {
  expect_equal(calculate_trait_mean_post_selection(population.size.exposed.survivors = 1,
                                                   mean.score.exposed.survivors = 10,
                                                   population.size.unexposed = 1,
                                                   trait.mean = 10,
                                                   population.size.post.selection =2), 10)
})
