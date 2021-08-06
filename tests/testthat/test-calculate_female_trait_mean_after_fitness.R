test_that("subtraction works", {
  expect_equal(calculate_female_trait_mean_after_fitness(female.trait.mean = 10,
                                                         female.fitness.cost = 1), 9)
})
