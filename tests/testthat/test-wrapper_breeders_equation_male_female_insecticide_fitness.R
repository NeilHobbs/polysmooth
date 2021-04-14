test_that("resistane is not heritable", {
  expect_equal(wrapper_breeders_equation_male_female_insecticide_fitness(trait.mean = 900,
                                                                         female.fitness.cost = 1,
                                                                         male.fitness.cost = 0,
                                                                         female.insecticide.exposure = 1,
                                                                         male.insecticide.exposure = 1,
                                                                         standard.deviation = 50,
                                                                         vector.length = 10000,
                                                                         maximum.bioassay.survival.proportion = 1,
                                                                         michaelis.menten.slope = 1,
                                                                         half.population.bioassay.survival.resistance = 900,
                                                                         regression.coefficient = 0.48,
                                                                         regression.intercept = 0.15,
                                                                         current.insecticide.efficacy = 1,
                                                                         exposure.scaling.factor = 10,
                                                                         heritability = 0), 0)
})

test_that("no female.insecticide.exposure gives only fitness costs", {
  expect_equal(wrapper_breeders_equation_male_female_insecticide_fitness(trait.mean = 900,
                                                                         female.fitness.cost = 1,
                                                                         male.fitness.cost = 1,
                                                                         female.insecticide.exposure = 0,
                                                                         male.insecticide.exposure = 1,
                                                                         standard.deviation = 50,
                                                                         vector.length = 10000,
                                                                         maximum.bioassay.survival.proportion = 1,
                                                                         michaelis.menten.slope = 1,
                                                                         half.population.bioassay.survival.resistance = 900,
                                                                         regression.coefficient = 1,
                                                                         regression.intercept = 0.15,
                                                                         current.insecticide.efficacy = 1,
                                                                         exposure.scaling.factor = 10,
                                                                         heritability = 0.5), -0.5)
})

