#For mixtures, if efficacy of partner AI is 0, then should be a monotherapy
#For micromosaics, if ci = 1 and cj = 0, then should be a monotherapy
#For combinations if probaility if only encoutnering i then a monotherapy

actual.monotherapy = multiple_gonotrophic_cycles_singles_dispersal(intervention.trait.mean.i = 10,
                                                                   refugia.trait.mean.i = 10,
                                                                   standard.deviation = 50,
                                                                   vector.length = 1000,
                                                                   female.exposure = 0.7,
                                                                   exposure.scaling.factor = 10,
                                                                   coverage = 0.9,
                                                                   dispersal.rate = 0.1,
                                                                   male.differential.intervention.i = 0,
                                                                   male.differential.refugia.i = 0,
                                                                   female.fitness.cost.i = 0,
                                                                   heritability.i = 0.3,
                                                                   n.cycles = 1,
                                                                   half.population.bioassay.survival.resistance = 1,
                                                                   michaelis.menten.slope = 1,
                                                                   maximum.bioassay.survival.proportion = 1,
                                                                   regression.coefficient = 0.48,
                                                                   regression.intercept = 0.15,
                                                                   current.insecticide.efficacy.i = 1,
                                                                   between.gonotrophic.survival = 1)

mixture.as.monotherapy = multiple_gonotrophic_cycles_mixture_dispersal(intervention.trait.mean.i = 10,
                                                                       refugia.trait.mean.i = 10,
                                                                       standard.deviation = 50,
                                                                       vector.length = 1000,
                                                                       female.exposure = 0.7,
                                                                       exposure.scaling.factor = 10,
                                                                       coverage = 0.9,
                                                                       dispersal.rate = 0.1,
                                                                       male.differential.intervention.i = 0,
                                                                       male.differential.refugia.i = 0,
                                                                       female.fitness.cost.i = 0,
                                                                       heritability.i = 0.3,
                                                                       n.cycles = 1,
                                                                       half.population.bioassay.survival.resistance = 1,
                                                                       michaelis.menten.slope = 1,
                                                                       maximum.bioassay.survival.proportion = 1,
                                                                       regression.coefficient = 0.48,
                                                                       regression.intercept = 0.15,
                                                                       current.insecticide.efficacy.i = 1,
                                                                       between.gonotrophic.survival = 1,
                                                                       intervention.trait.mean.j = 10,
                                                                       refugia.trait.mean.j = 10,
                                                                       male.differential.intervention.j = 0,
                                                                       male.differential.refugia.j = 0,
                                                                       female.fitness.cost.j = 0,
                                                                       heritability.j = 0.3,
                                                                       current.insecticide.efficacy.j = 0,
                                                                       cross.selection.i.j = 0,
                                                                       cross.selection.j.i = 0)[1:2]

micromosaic.as.monotherapy = multiple_gonotrophic_cycles_micromosaic_dispersal(intervention.trait.mean.i = 10,
                                                                               refugia.trait.mean.i = 10,
                                                                               standard.deviation = 50,
                                                                               vector.length = 1000,
                                                                               female.exposure = 0.7,
                                                                               exposure.scaling.factor = 10,
                                                                               coverage = 0.9,
                                                                               dispersal.rate = 0.1,
                                                                               male.differential.intervention.i = 0,
                                                                               male.differential.refugia.i = 0,
                                                                               female.fitness.cost.i = 0,
                                                                               heritability.i = 0.3,
                                                                               n.cycles = 1,
                                                                               half.population.bioassay.survival.resistance = 1,
                                                                               michaelis.menten.slope = 1,
                                                                               maximum.bioassay.survival.proportion = 1,
                                                                               regression.coefficient = 0.48,
                                                                               regression.intercept = 0.15,
                                                                               current.insecticide.efficacy.i = 1,
                                                                               between.gonotrophic.survival = 1,
                                                                               intervention.trait.mean.j = 10,
                                                                               refugia.trait.mean.j = 10,
                                                                               male.differential.intervention.j = 0,
                                                                               male.differential.refugia.j = 0,
                                                                               female.fitness.cost.j = 0,
                                                                               heritability.j = 0.3,
                                                                               current.insecticide.efficacy.j = 0,
                                                                               coverage.i = 1,
                                                                               coverage.j = 0,
                                                                               cross.selection.i.j = 0,
                                                                               cross.selection.j.i = 0)[1:2]

combination.as.monotherapy = multiple_gonotrophic_cycles_combination_dispersal(intervention.trait.mean.i = 10,
                                                                               refugia.trait.mean.i = 10,
                                                                               standard.deviation = 50,
                                                                               vector.length = 1000,
                                                                               female.exposure = 0.7,
                                                                               exposure.scaling.factor = 10,
                                                                               coverage = 0.9,
                                                                               dispersal.rate = 0.1,
                                                                               male.differential.intervention.i = 0,
                                                                               male.differential.refugia.i = 0,
                                                                               female.fitness.cost.i = 0,
                                                                               heritability.i = 0.3,
                                                                               n.cycles = 1,
                                                                               half.population.bioassay.survival.resistance = 1,
                                                                               michaelis.menten.slope = 1,
                                                                               maximum.bioassay.survival.proportion = 1,
                                                                               regression.coefficient = 0.48,
                                                                               regression.intercept = 0.15,
                                                                               current.insecticide.efficacy.i = 1,
                                                                               between.gonotrophic.survival = 1,
                                                                               intervention.trait.mean.j = 10,
                                                                               refugia.trait.mean.j = 10,
                                                                               male.differential.intervention.j = 0,
                                                                               male.differential.refugia.j = 0,
                                                                               female.fitness.cost.j = 0,
                                                                               heritability.j = 0.3,
                                                                               current.insecticide.efficacy.j = 0,
                                                                               coverage.i = 1,
                                                                               coverage.j = 0,
                                                                               coverage.ij = 0,
                                                                               probability.only.i = 1,
                                                                               probability.only.j = 0,
                                                                               probability.both.i.j = 0,
                                                                               cross.selection.i.j = 0,
                                                                               cross.selection.j.i = 0)[1:2]


test_that("mixture as monotherapy", {
  expect_identical(actual.monotherapy, mixture.as.monotherapy)
})

test_that("micromosaic as monotherapy", {
  expect_identical(actual.monotherapy, micromosaic.as.monotherapy)
})

test_that("combiantion as monotherapy", {
  expect_identical(actual.monotherapy, combination.as.monotherapy)
})
