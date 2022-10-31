test.1.a = multiple_gonotrophic_cycles_singles_dispersal_sd_scaled(intervention.trait.mean.i = 0,
                                                                 refugia.trait.mean.i = 0,
                                                                 vector.length = 10000,
                                                                 female.exposure = 0.5,
                                                                 exposure.scaling.factor = 10,
                                                                 coverage = 1,
                                                                 dispersal.rate = 0,
                                                                 male.differential.intervention.i = 0,
                                                                 male.differential.refugia.i= 0,
                                                                 female.fitness.cost.i= 0,
                                                                 heritability.i = 1,
                                                                 n.cycles = 10,
                                                                 half.population.bioassay.survival.resistance = 900,
                                                                 michaelis.menten.slope = 1,
                                                                 maximum.bioassay.survival.proportion = 1,
                                                                 regression.coefficient = 0.5,
                                                                 regression.intercept = 0.15,
                                                                 current.insecticide.efficacy.i = 1,
                                                                 z.sd.intercept = 25,
                                                                 z.sd.coefficient = 0.4)

test.1.b = multiple_gonotrophic_cycles_singles_dispersal_sd_scaled(intervention.trait.mean.i = 0,
                                                                 refugia.trait.mean.i = 0,
                                                                 vector.length = 10000,
                                                                 female.exposure = 0.5,
                                                                 exposure.scaling.factor = 10,
                                                                 coverage = 0.7,
                                                                 dispersal.rate = 0.3,
                                                                 male.differential.intervention.i = 0,
                                                                 male.differential.refugia.i= 0,
                                                                 female.fitness.cost.i= 0,
                                                                 heritability.i = 1,
                                                                 n.cycles = 10,
                                                                 half.population.bioassay.survival.resistance = 900,
                                                                 michaelis.menten.slope = 1,
                                                                 maximum.bioassay.survival.proportion = 1,
                                                                 regression.coefficient = 0.5,
                                                                 regression.intercept = 0.15,
                                                                 current.insecticide.efficacy.i = 1,
                                                                 z.sd.intercept = 25,
                                                                 z.sd.coefficient = 0.4)

test_that("resistance takes off", {
  expect_gt(test.1.a[[1]], 0)
  expect_gt(test.1.b[[1]], 0)
  expect_gt(test.1.b[[2]], 0)
})

test.2.a = multiple_gonotrophic_cycles_singles_dispersal_sd_scaled(intervention.trait.mean.i = 0,
                                                                 refugia.trait.mean.i = 0,
                                                                 vector.length = 10000,
                                                                 female.exposure = 0.5,
                                                                 exposure.scaling.factor = 10,
                                                                 coverage = 1,
                                                                 dispersal.rate = 0,
                                                                 male.differential.intervention.i = -1000,
                                                                 male.differential.refugia.i= -1000,
                                                                 female.fitness.cost.i= 1,
                                                                 heritability.i = 1,
                                                                 n.cycles = 10,
                                                                 half.population.bioassay.survival.resistance = 900,
                                                                 michaelis.menten.slope = 1,
                                                                 maximum.bioassay.survival.proportion = 1,
                                                                 regression.coefficient = 0.5,
                                                                 regression.intercept = 0.15,
                                                                 current.insecticide.efficacy.i = 1,
                                                                 z.sd.intercept = 25,
                                                                 z.sd.coefficient = 0.4)


test.2.b = multiple_gonotrophic_cycles_singles_dispersal_sd_scaled(intervention.trait.mean.i = 0,
                                                                   refugia.trait.mean.i = 0,
                                                                   vector.length = 10000,
                                                                   female.exposure = 0.5,
                                                                   exposure.scaling.factor = 10,
                                                                   coverage = 0.7,
                                                                   dispersal.rate = 0.3,
                                                                   male.differential.intervention.i = -1000,
                                                                   male.differential.refugia.i= -1000,
                                                                   female.fitness.cost.i= 1,
                                                                   heritability.i = 1,
                                                                   n.cycles = 10,
                                                                   half.population.bioassay.survival.resistance = 900,
                                                                   michaelis.menten.slope = 1,
                                                                   maximum.bioassay.survival.proportion = 1,
                                                                   regression.coefficient = 0.5,
                                                                   regression.intercept = 0.15,
                                                                   current.insecticide.efficacy.i = 1,
                                                                   z.sd.intercept = 25,
                                                                   z.sd.coefficient = 0.4)

test_that("resistance cannot fall below 0", {
  expect_equal(test.2.a[[1]], 0)
  expect_equal(test.2.b[[1]], 0)
  expect_equal(test.2.b[[2]], 0)

})
