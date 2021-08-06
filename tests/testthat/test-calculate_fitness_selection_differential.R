test_that("subtraction works", {
  expect_equal(calculate_fitness_selection_differential(trait.mean.after.fitness = 9,
                                                                   trait.mean = 10), -1)
})
