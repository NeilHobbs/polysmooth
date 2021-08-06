test_that("multiplication works", {
  expect_equal(calculate_insecticide_fitness_selection_differential(exposure.scaling.factor = 10,
                                                                    insecticide.selection.differential = 10,
                                                                    fitness.selection.differential = -5), 95)
})
