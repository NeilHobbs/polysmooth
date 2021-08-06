test_that("multiplication works", {
  expect_equal(breeders_equation_male_female_insecticide_fitness(heritability = 1,
                                                                 male.insecticide.fitness.selection.differential = 1,
                                                                 female.insecticide.fitness.selection.differential = 1)
               , 1)
})

test_that("half heritability", {
  expect_equal(breeders_equation_male_female_insecticide_fitness(heritability = 0.5,
                                                                 male.insecticide.fitness.selection.differential = 1,
                                                                 female.insecticide.fitness.selection.differential = 1)
               , 0.5)
})


test_that("half male differential", {
  expect_equal(breeders_equation_male_female_insecticide_fitness(heritability = 1,
                                                                 male.insecticide.fitness.selection.differential = 0.5,
                                                                 female.insecticide.fitness.selection.differential = 1)
               , 0.75)
})

test_that("half female differential", {
  expect_equal(breeders_equation_male_female_insecticide_fitness(heritability = 1,
                                                                 male.insecticide.fitness.selection.differential = 1,
                                                                 female.insecticide.fitness.selection.differential = 0.5)
               , 0.75)
})
