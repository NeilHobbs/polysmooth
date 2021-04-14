test_that("no change when fitness and insecticide is zero", {
  expect_equal(wrapper_calculate_male_insecticide_fitness_selection_differential(male.trait.mean = 10,
                                                                                 female.insecticide.exposure = 1,
                                                                                 male.insecticide.exposure = 1,
                                                                                 standard.deviation = 10,
                                                                                 vector.length = 1000,
                                                                                 maximum.bioassay.survival.proportion = 1,
                                                                                 michaelis.menten.slope = 1,
                                                                                 half.population.bioassay.survival.resistance = 900,
                                                                                 regression.coefficient = 0.48,
                                                                                 regression.intercept = 0.15,
                                                                                 current.insecticide.efficacy = 0,
                                                                                 exposure.scaling.factor = 10,
                                                                                 male.fitness.cost = 0), 0)
})

test_that("only fitness cost when no insecticide", {
  expect_equal(wrapper_calculate_male_insecticide_fitness_selection_differential(male.trait.mean = 10,
                                                                                 female.insecticide.exposure = 1,
                                                                                 male.insecticide.exposure = 1,
                                                                                 standard.deviation = 10,
                                                                                 vector.length = 1000,
                                                                                 maximum.bioassay.survival.proportion = 1,
                                                                                 michaelis.menten.slope = 1,
                                                                                 half.population.bioassay.survival.resistance = 900,
                                                                                 regression.coefficient = 0.48,
                                                                                 regression.intercept = 0.15,
                                                                                 current.insecticide.efficacy = 0,
                                                                                 exposure.scaling.factor = 10,
                                                                                 male.fitness.cost = 1), -1)
})


test_that("exposure scaling factor multiplies correctly", {
  expect_equal(wrapper_calculate_male_insecticide_fitness_selection_differential(male.trait.mean = 10,
                                                                                 female.insecticide.exposure = 1,
                                                                                 male.insecticide.exposure = 1,
                                                                                 standard.deviation = 10,
                                                                                 vector.length = 1000,
                                                                                 maximum.bioassay.survival.proportion = 1,
                                                                                 michaelis.menten.slope = 1,
                                                                                 half.population.bioassay.survival.resistance = 900,
                                                                                 regression.coefficient = 0.48,
                                                                                 regression.intercept = 0.15,
                                                                                 current.insecticide.efficacy = 1,
                                                                                 exposure.scaling.factor = 10,
                                                                                 male.fitness.cost = 0), wrapper_calculate_male_insecticide_fitness_selection_differential(male.trait.mean = 10,
                                                                                                                                                                           female.insecticide.exposure = 1,
                                                                                                                                                                           male.insecticide.exposure = 1,
                                                                                                                                                                           standard.deviation = 10,
                                                                                                                                                                           vector.length = 1000,
                                                                                                                                                                           maximum.bioassay.survival.proportion = 1,
                                                                                                                                                                           michaelis.menten.slope = 1,
                                                                                                                                                                           half.population.bioassay.survival.resistance = 900,
                                                                                                                                                                           regression.coefficient = 0.48,
                                                                                                                                                                           regression.intercept = 0.15,
                                                                                                                                                                           current.insecticide.efficacy = 1,
                                                                                                                                                                           exposure.scaling.factor = 1,
                                                                                                                                                                           male.fitness.cost = 0)*10)
})
