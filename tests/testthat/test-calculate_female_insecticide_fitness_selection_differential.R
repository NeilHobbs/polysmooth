test_that("works when insecticide selection differential positive", {
  expect_equal(calculate_female_insecticide_fitness_selection_differential(female.insecticide.selection.differential = 10,
                                                                           exposure.scaling.factor = 10,
                                                                           female.fitness.selection.differential = -5), 95)
})

test_that("works when insecticide selection differential negative", {
  expect_equal(calculate_female_insecticide_fitness_selection_differential(female.insecticide.selection.differential = -10,
                                                                           exposure.scaling.factor = 10,
                                                                           female.fitness.selection.differential = -5), -105)
})
