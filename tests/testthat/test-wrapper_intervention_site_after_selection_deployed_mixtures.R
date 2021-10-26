test_that("population mean cannot fall below zero", {
  expect_equal(wrapper_intervention_site_after_selection_deployed_mixtures(intervention.before.selection = 10,
                                                                           female.fitness.cost =1000000,
                                                                           male.fitness.cost = 1000000,
                                                                           female.insecticide.exposure = 1,
                                                                           male.insecticide.exposure = 1,
                                                                           standard.deviation = 30,
                                                                           vector.length = 1000,
                                                                           maximum.bioassay.survival.proportion = 1,
                                                                           michaelis.menten.slope = 1,
                                                                           half.population.bioassay.survival.resistance = 900,
                                                                           regression.coefficient = 0.48,
                                                                           regression.intercept = 0.15,
                                                                           current.insecticide.efficacy = 0,
                                                                           exposure.scaling.factor = 10,
                                                                           heritability = 1,
                                                                           survival.to.other.insecticide = 1) , 0)
})


test_that("all die to other insecticide, no selection", {
  expect_equal(wrapper_intervention_site_after_selection_deployed_mixtures(intervention.before.selection = 10,
                                                                           female.fitness.cost =0,
                                                                           male.fitness.cost = 0,
                                                                           female.insecticide.exposure = 1,
                                                                           male.insecticide.exposure = 1,
                                                                           standard.deviation = 30,
                                                                           vector.length = 1000,
                                                                           maximum.bioassay.survival.proportion = 1,
                                                                           michaelis.menten.slope = 1,
                                                                           half.population.bioassay.survival.resistance = 900,
                                                                           regression.coefficient = 0.48,
                                                                           regression.intercept = 0.15,
                                                                           current.insecticide.efficacy = 0,
                                                                           exposure.scaling.factor = 10,
                                                                           heritability = 1,
                                                                           survival.to.other.insecticide = 0) , 10)
})
