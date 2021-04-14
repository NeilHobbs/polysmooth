test_that("no fitness costs, response", {
  expect_equal(wrapper_breeders_equation_male_female_fitness(heritability = 1,
                                                             trait.mean = 10,
                                                             female.fitness.cost = 0,
                                                             male.fitness.cost = 0), 0)
})


test_that("negative mean of male and female costs", {
  expect_equal(wrapper_breeders_equation_male_female_fitness(heritability = 1,
                                                             trait.mean = 10,
                                                             female.fitness.cost = 2,
                                                             male.fitness.cost = 1), -1.5)
})
