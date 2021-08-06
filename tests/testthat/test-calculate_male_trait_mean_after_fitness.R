test_that("subtraction works", {
  expect_equal(calculate_male_trait_mean_after_fitness(male.trait.mean = 10,
                                                       male.fitness.cost = 9), 1)
})
