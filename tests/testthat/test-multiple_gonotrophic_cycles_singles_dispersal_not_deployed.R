test.1 = multiple_gonotrophic_cycles_singles_dispersal_not_deployed(intervention.trait.mean.i = 0,
                                                           intervention.trait.mean.j = 0,
                                                           refugia.trait.mean.i = 0,
                                                           refugia.trait.mean.j = 0,
                                                           standard.deviation = 50,
                                                           vector.length = 10000,
                                                           female.exposure = 0.5,
                                                           exposure.scaling.factor = 10,
                                                           coverage = 1,
                                                           dispersal.rate = 0,
                                                           male.differential.intervention.i = 0,
                                                           male.differential.intervention.j = 0,
                                                           male.differential.refugia.i = 0,
                                                           male.differential.refugia.j = 0,
                                                           female.fitness.cost.i = 0,
                                                           female.fitness.cost.j = 0,
                                                           heritability.i = 1,
                                                           heritability.j = 1,
                                                           n.cycles = 5,
                                                           half.population.bioassay.survival.resistance = 900,
                                                           michaelis.menten.slope = 1,
                                                           maximum.bioassay.survival.proportion = 1,
                                                           regression.coefficient = 0.5,
                                                           regression.intercept = 0.15,
                                                           current.insecticide.efficacy.j = 1,
                                                           cross.selection.j.i = 0)

test_that("no selection", {
  expect_equal(test.1[[1]], 0)
  expect_equal(test.1[[2]], 0)
})

test.2 = multiple_gonotrophic_cycles_singles_dispersal_not_deployed(intervention.trait.mean.i = 0,
                                                                    intervention.trait.mean.j = 0,
                                                                    refugia.trait.mean.i = 0,
                                                                    refugia.trait.mean.j = 0,
                                                                    standard.deviation = 50,
                                                                    vector.length = 10000,
                                                                    female.exposure = 0.5,
                                                                    exposure.scaling.factor = 10,
                                                                    coverage = 1,
                                                                    dispersal.rate = 0,
                                                                    male.differential.intervention.i = 0,
                                                                    male.differential.intervention.j = 0,
                                                                    male.differential.refugia.i = 0,
                                                                    male.differential.refugia.j = 0,
                                                                    female.fitness.cost.i = 0,
                                                                    female.fitness.cost.j = 0,
                                                                    heritability.i = 1,
                                                                    heritability.j = 1,
                                                                    n.cycles = 5,
                                                                    half.population.bioassay.survival.resistance = 900,
                                                                    michaelis.menten.slope = 1,
                                                                    maximum.bioassay.survival.proportion = 1,
                                                                    regression.coefficient = 0.5,
                                                                    regression.intercept = 0.15,
                                                                    current.insecticide.efficacy.j = 1,
                                                                    cross.selection.j.i = 0.5)

test_that("cross selection", {
  expect_gt(test.2[[1]], 0)
})


test.3.a = multiple_gonotrophic_cycles_singles_dispersal_not_deployed(intervention.trait.mean.i = 0,
                                                                    intervention.trait.mean.j = 0,
                                                                    refugia.trait.mean.i = 0,
                                                                    refugia.trait.mean.j = 0,
                                                                    standard.deviation = 50,
                                                                    vector.length = 10000,
                                                                    female.exposure = 0.5,
                                                                    exposure.scaling.factor = 10,
                                                                    coverage = 1,
                                                                    dispersal.rate = 0,
                                                                    male.differential.intervention.i = -1000,
                                                                    male.differential.intervention.j = 0,
                                                                    male.differential.refugia.i = -1000,
                                                                    male.differential.refugia.j = 0,
                                                                    female.fitness.cost.i = 1,
                                                                    female.fitness.cost.j = 0,
                                                                    heritability.i = 1,
                                                                    heritability.j = 1,
                                                                    n.cycles = 10,
                                                                    half.population.bioassay.survival.resistance = 900,
                                                                    michaelis.menten.slope = 1,
                                                                    maximum.bioassay.survival.proportion = 1,
                                                                    regression.coefficient = 0.5,
                                                                    regression.intercept = 0.15,
                                                                    current.insecticide.efficacy.j = 1,
                                                                    cross.selection.j.i = 0)

test.3.b = multiple_gonotrophic_cycles_singles_dispersal_not_deployed(intervention.trait.mean.i = 0,
                                                                      intervention.trait.mean.j = 0,
                                                                      refugia.trait.mean.i = 0,
                                                                      refugia.trait.mean.j = 0,
                                                                      standard.deviation = 50,
                                                                      vector.length = 10000,
                                                                      female.exposure = 0.5,
                                                                      exposure.scaling.factor = 10,
                                                                      coverage = 0.7,
                                                                      dispersal.rate = 0.3,
                                                                      male.differential.intervention.i = -1000,
                                                                      male.differential.intervention.j = 0,
                                                                      male.differential.refugia.i = -1000,
                                                                      male.differential.refugia.j = 0,
                                                                      female.fitness.cost.i = 1,
                                                                      female.fitness.cost.j = 0,
                                                                      heritability.i = 1,
                                                                      heritability.j = 1,
                                                                      n.cycles = 10,
                                                                      half.population.bioassay.survival.resistance = 900,
                                                                      michaelis.menten.slope = 1,
                                                                      maximum.bioassay.survival.proportion = 1,
                                                                      regression.coefficient = 0.5,
                                                                      regression.intercept = 0.15,
                                                                      current.insecticide.efficacy.j = 1,
                                                                      cross.selection.j.i = 0)

test_that("resistance cannot fall below zero", {
  expect_equal(test.3.a[[1]], 0)
  expect_equal(test.3.b[[1]], 0)
  expect_equal(test.3.b[[2]], 0)

})
