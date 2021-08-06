test_that("subtraction works", {
  expect_equal(calculate_female_fitness_selection_differential(female.trait.mean = 10,
                                                               female.trait.mean.after.fitness = 9), -1)
})
