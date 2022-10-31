test.1 = multiple_gonotrophic_cycles_singles_dispersal(intervention.trait.mean.i = 0,
                                                             refugia.trait.mean.i = 0,
                                                             standard.deviation = 50,
                                                             vector.length = 10000,
                                                             female.exposure = 0.5,
                                                             exposure.scaling.factor = 10,
                                                             coverage = 1,
                                                             dispersal.rate = 0,
                                                             male.differential.intervention.i = 0,
                                                             male.differential.refugia.i = 0,
                                                             female.fitness.cost.i = 0,
                                                             heritability.i = 1,
                                                             n.cycles = 10,
                                                             half.population.bioassay.survival.resistance = 900,
                                                             michaelis.menten.slope = 1,
                                                             maximum.bioassay.survival.proportion = 1,
                                                             regression.coefficient = 0.5,
                                                             regression.intercept = 0.15,
                                                             current.insecticide.efficacy.i = 0)


test_that("no insecticide no selection", {
  expect_equal(test.1[[1]], 0)
  expect_equal(test.1[[2]], 0)
})

test.2 = multiple_gonotrophic_cycles_singles_dispersal(intervention.trait.mean.i = 0,
                                                             refugia.trait.mean.i = 0,
                                                             standard.deviation = 50,
                                                             vector.length = 10000,
                                                             female.exposure = 0.5,
                                                             exposure.scaling.factor = 10,
                                                             coverage = 1,
                                                             dispersal.rate = 0,
                                                             male.differential.intervention.i = -1000,
                                                             male.differential.refugia.i = -1000,
                                                             female.fitness.cost.i = 0,
                                                             heritability.i = 1,
                                                             n.cycles = 10,
                                                             half.population.bioassay.survival.resistance = 900,
                                                             michaelis.menten.slope = 1,
                                                             maximum.bioassay.survival.proportion = 1,
                                                             regression.coefficient = 0.5,
                                                             regression.intercept = 0.15,
                                                             current.insecticide.efficacy.i = 1)

test_that("mean cannot fall below zero", {
  expect_equal(test.2[[1]], 0)
  expect_equal(test.2[[2]], 0)
})


test.3 = multiple_gonotrophic_cycles_singles_dispersal(intervention.trait.mean.i = 0,
                                                       refugia.trait.mean.i = 0,
                                                       standard.deviation = 50,
                                                       vector.length = 10000,
                                                       female.exposure = 0.5,
                                                       exposure.scaling.factor = 10,
                                                       coverage = 1,
                                                       dispersal.rate = 0,
                                                       male.differential.intervention.i = 1,
                                                       male.differential.refugia.i = 1,
                                                       female.fitness.cost.i = 0,
                                                       heritability.i = 1,
                                                       n.cycles = 10,
                                                       half.population.bioassay.survival.resistance = 900,
                                                       michaelis.menten.slope = 1,
                                                       maximum.bioassay.survival.proportion = 1,
                                                       regression.coefficient = 0.5,
                                                       regression.intercept = 0.15,
                                                       current.insecticide.efficacy.i = 1)

test_that("selection does occur", {
  expect_gt(test.3[[1]], 0)
})



test.4 = multiple_gonotrophic_cycles_singles_dispersal(intervention.trait.mean.i = 0,
                                                       refugia.trait.mean.i = 0,
                                                       standard.deviation = 50,
                                                       vector.length = 10000,
                                                       female.exposure = 0.5,
                                                       exposure.scaling.factor = 10,
                                                       coverage = 0.5,
                                                       dispersal.rate = 0.3,
                                                       male.differential.intervention.i = 1,
                                                       male.differential.refugia.i = 1,
                                                       female.fitness.cost.i = 0,
                                                       heritability.i = 1,
                                                       n.cycles = 10,
                                                       half.population.bioassay.survival.resistance = 900,
                                                       michaelis.menten.slope = 1,
                                                       maximum.bioassay.survival.proportion = 1,
                                                       regression.coefficient = 0.5,
                                                       regression.intercept = 0.15,
                                                       current.insecticide.efficacy.i = 1)

test_that("selection and dispersal does occur", {
  expect_gt(test.4[[1]], 0)
  expect_gt(test.4[[2]], 0)
})

