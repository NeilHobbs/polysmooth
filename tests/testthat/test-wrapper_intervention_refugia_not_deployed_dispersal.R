test_that("no selection no change", {
  expect_equal(wrapper_intervention_refugia_not_deployed_dispersal(insecticide.population.suppression = 0,
                                                                   intervention.before.selection = 0,
                                                                   female.fitness.cost = 0,
                                                                   male.fitness.cost = 0,
                                                                   heritability = 1,
                                                                   refugia.before.selection = 0,
                                                                   dispersal.rate = 0.1,
                                                                   intervention.coverage = 0.1,
                                                                   cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                                        min.cross.selection = 0,
                                                                                                                        max.cross.selection = 0),
                                                                   currently.deployed.insecticide = 1,
                                                                   currently.tracked.insecticide = 2,
                                                                   female.insecticide.exposure = 0,
                                                                   male.insecticide.exposure = 0,
                                                                   standard.deviation = 20,
                                                                   vector.length = 1000,
                                                                   maximum.bioassay.survival.proportion = 1,
                                                                   michaelis.menten.slope = 1,
                                                                   half.population.bioassay.survival.resistance = 900,
                                                                   regression.coefficient = 0.48,
                                                                   regression.intercept = 0.15,
                                                                   current.insecticide.efficacy = 1,
                                                                   exposure.scaling.factor = 10,
                                                                   intervention.before.selection.other = 0)[[1]], 0)

  expect_equal(wrapper_intervention_refugia_not_deployed_dispersal(insecticide.population.suppression = 0,
                                                                   intervention.before.selection = 10,
                                                                   female.fitness.cost = 0,
                                                                   male.fitness.cost = 0,
                                                                   heritability = 1,
                                                                   refugia.before.selection = 0,
                                                                   dispersal.rate = 0,
                                                                   intervention.coverage = 0.1,
                                                                   cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = 2,
                                                                                                                        min.cross.selection = 0,
                                                                                                                        max.cross.selection = 0),
                                                                   currently.deployed.insecticide = 1,
                                                                   currently.tracked.insecticide = 2,
                                                                   female.insecticide.exposure = 0,
                                                                   male.insecticide.exposure = 0,
                                                                   standard.deviation = 20,
                                                                   vector.length = 1000,
                                                                   maximum.bioassay.survival.proportion = 1,
                                                                   michaelis.menten.slope = 1,
                                                                   half.population.bioassay.survival.resistance = 900,
                                                                   regression.coefficient = 0.48,
                                                                   regression.intercept = 0.15,
                                                                   current.insecticide.efficacy = 1,
                                                                   exposure.scaling.factor = 10,
                                                                   intervention.before.selection.other = 0)[[1]], 10)
})







