test_that("no exposure, only fitness costs", {
  expect_equal(wrapper_intervention_site_after_selection_deployed(intervention.before.selection = 10,
                                                                  female.fitness.cost = 1,
                                                                  male.fitness.cost = 1,
                                                                  female.insecticide.exposure = 0,
                                                                  male.insecticide.exposure = 0,
                                                                  standard.deviation = 30,
                                                                  vector.length = 10000,
                                                                  maximum.bioassay.survival.proportion = 1,
                                                                  michaelis.menten.slope = 1,
                                                                  half.population.bioassay.survival.resistance = 900,
                                                                  regression.coefficient = 0.48,
                                                                  regression.intercept = 0.15,
                                                                  current.insecticide.efficacy = 0.7,
                                                                  exposure.scaling.factor = 1,
                                                                  heritability = 1)
               , 9)
})

test_that("no exposure, only fitness costs. Cannot fall below zero", {
  expect_equal(wrapper_intervention_site_after_selection_deployed(intervention.before.selection = 10,
                                                                  female.fitness.cost = 10.1,
                                                                  male.fitness.cost = 10.1,
                                                                  female.insecticide.exposure = 0,
                                                                  male.insecticide.exposure = 0,
                                                                  standard.deviation = 30,
                                                                  vector.length = 10000,
                                                                  maximum.bioassay.survival.proportion = 1,
                                                                  michaelis.menten.slope = 1,
                                                                  half.population.bioassay.survival.resistance = 900,
                                                                  regression.coefficient = 0.48,
                                                                  regression.intercept = 0.15,
                                                                  current.insecticide.efficacy = 0.7,
                                                                  exposure.scaling.factor = 1,
                                                                  heritability = 1)
               , 0)
})
