test_that("heritability too low", {
  expect_error(breeders_equation_male_female_fitness(male.fitness.selection.differential = 10,
                                                     female.fitness.selection.differential = 10,
                                                     heritability = -0.1), "heritability must be between 0 and 1")
})

test_that("heritability too high", {
  expect_error(breeders_equation_male_female_fitness(male.fitness.selection.differential = 10,
                                                     female.fitness.selection.differential = 10,
                                                     heritability = 1.1), "heritability must be between 0 and 1")
})
