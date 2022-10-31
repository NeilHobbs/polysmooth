test.1 = wrapper_calculate_male_insecticide_fitness_selection_differential_sd_scaled(male.trait.mean = 0,
                                                                                     female.insecticide.exposure = 0,
                                                                                     male.insecticide.exposure = 0,
                                                                                     z.sd.intercept = 24,
                                                                                     z.sd.coefficient = 0.4,
                                                                                     male.fitness.cost = 0,
                                                                                     vector.length = 10000,
                                                                                     maximum.bioassay.survival.proportion = 1,
                                                                                     michaelis.menten.slope = 1,
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     regression.coefficient = 0.5,
                                                                                     regression.intercept = 0.15,
                                                                                     current.insecticide.efficacy = 1,
                                                                                     exposure.scaling.factor = 10)



test_that("no selection or fitness costs", {
  expect_equal(test.1, 0)
})

test.2 = wrapper_calculate_male_insecticide_fitness_selection_differential_sd_scaled(male.trait.mean = 0,
                                                                                     female.insecticide.exposure = 0,
                                                                                     male.insecticide.exposure = 0,
                                                                                     z.sd.intercept = 24,
                                                                                     z.sd.coefficient = 0.4,
                                                                                     male.fitness.cost = -1,
                                                                                     vector.length = 10000,
                                                                                     maximum.bioassay.survival.proportion = 1,
                                                                                     michaelis.menten.slope = 1,
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     regression.coefficient = 0.5,
                                                                                     regression.intercept = 0.15,
                                                                                     current.insecticide.efficacy = 1,
                                                                                     exposure.scaling.factor = 10)



test_that("fitness costs", {
  expect_lt(test.2, 0)
})



test.3 = wrapper_calculate_male_insecticide_fitness_selection_differential_sd_scaled(male.trait.mean = 0,
                                                                                     female.insecticide.exposure = 0.7,
                                                                                     male.insecticide.exposure = 0.7,
                                                                                     z.sd.intercept = 24,
                                                                                     z.sd.coefficient = 0.4,
                                                                                     male.fitness.cost = 0,
                                                                                     vector.length = 10000,
                                                                                     maximum.bioassay.survival.proportion = 1,
                                                                                     michaelis.menten.slope = 1,
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     regression.coefficient = 0.5,
                                                                                     regression.intercept = 0.15,
                                                                                     current.insecticide.efficacy = 1,
                                                                                     exposure.scaling.factor = 10)



test_that("selection occurs", {
  expect_gt(test.3, 0)
})
