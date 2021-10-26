test_that("equation works", {
  expect_equal(calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = 10,
                                                                                    exposure.scaling.factor = 10,
                                                                                    male.fitness.selection.differential = -10), 90)
})
