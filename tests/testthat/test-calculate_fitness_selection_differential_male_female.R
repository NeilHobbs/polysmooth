test_that("equation works", {
  expect_equal(calculate_fitness_selection_differential_male_female(male.fitness.selection.differential = 10,
                                                                               female.fitness.selection.differential = 10), 10)
})
