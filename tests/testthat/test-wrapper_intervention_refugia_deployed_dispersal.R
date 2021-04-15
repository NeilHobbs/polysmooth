test_that("exchange to the intervention site", {
  expect_equal(wrapper_intervention_refugia_deployed_dispersal(insecticide.population.suppression = 0,
                                                               intervention.before.selection = 0,
                                                               female.fitness.cost = 0,
                                                               male.fitness.cost = 0,
                                                               female.insecticide.exposure = 1,
                                                               male.insecticide.exposure = 1,
                                                               standard.deviation = 30,
                                                               vector.length = 10000,
                                                               maximum.bioassay.survival.proportion = 1,
                                                               michaelis.menten.slope = 1,
                                                               half.population.bioassay.survival.resistance = 900,
                                                               regression.coefficient = 0.48,
                                                               regression.intercept = 0.15,
                                                               current.insecticide.efficacy = 0,
                                                               exposure.scaling.factor = 10,
                                                               heritability = 1,
                                                               refugia.before.selection = 10,
                                                               dispersal.rate = 0.5,
                                                               intervention.coverage = 0.5)[[1]], 2.5)
})

test_that("exchange to the refugia site", {
  expect_equal(wrapper_intervention_refugia_deployed_dispersal(insecticide.population.suppression = 0,
                                                               intervention.before.selection = 10,
                                                               female.fitness.cost = 0,
                                                               male.fitness.cost = 0,
                                                               female.insecticide.exposure = 1,
                                                               male.insecticide.exposure = 1,
                                                               standard.deviation = 30,
                                                               vector.length = 10000,
                                                               maximum.bioassay.survival.proportion = 1,
                                                               michaelis.menten.slope = 1,
                                                               half.population.bioassay.survival.resistance = 900,
                                                               regression.coefficient = 0.48,
                                                               regression.intercept = 0.15,
                                                               current.insecticide.efficacy = 0,
                                                               exposure.scaling.factor = 10,
                                                               heritability = 1,
                                                               refugia.before.selection = 0,
                                                               dispersal.rate = 0.5,
                                                               intervention.coverage = 0.5)[[2]], 2.5)
})

test_that("population suppression works", {
  expect_equal(wrapper_intervention_refugia_deployed_dispersal(insecticide.population.suppression = 1,
                                                               intervention.before.selection = 1000,
                                                               female.fitness.cost = 0,
                                                               male.fitness.cost = 0,
                                                               female.insecticide.exposure = 1,
                                                               male.insecticide.exposure = 1,
                                                               standard.deviation = 30,
                                                               vector.length = 10000,
                                                               maximum.bioassay.survival.proportion = 1,
                                                               michaelis.menten.slope = 1,
                                                               half.population.bioassay.survival.resistance = 900,
                                                               regression.coefficient = 0.48,
                                                               regression.intercept = 0.15,
                                                               current.insecticide.efficacy = 0,
                                                               exposure.scaling.factor = 10,
                                                               heritability = 1,
                                                               refugia.before.selection = 0,
                                                               dispersal.rate = 0.5,
                                                               intervention.coverage = 0.5)[[2]], 0)
})




