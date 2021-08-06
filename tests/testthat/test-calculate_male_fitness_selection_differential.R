test_that("subtraction works", {
  expect_equal(calculate_male_fitness_selection_differential(male.trait.mean = 10,
                                                             male.trait.mean.after.fitness = 9), -1)
})
