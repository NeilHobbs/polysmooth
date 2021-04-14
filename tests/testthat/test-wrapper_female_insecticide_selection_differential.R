test_that("no selection when no insecticide efficacy", {
  expect_equal(wrapper_female_insecticide_selection_differential(intervention.before.selection = 10,
                                                                 female.insecticide.exposure = 1,
                                                                 standard.deviation = 10,
                                                                 vector.length = 1000,
                                                                 maximum.bioassay.survival.proportion = 1,
                                                                 michaelis.menten.slope = 1,
                                                                 half.population.bioassay.survival.resistance = 900,
                                                                 regression.coefficient = 0.48,
                                                                 regression.intercept = 0.15,
                                                                 current.insecticide.efficacy = 0), 0)
})


test_that("no selection when no exposure", {
  expect_equal(wrapper_female_insecticide_selection_differential(intervention.before.selection = 10,
                                                                 female.insecticide.exposure = 0,
                                                                 standard.deviation = 10,
                                                                 vector.length = 1000,
                                                                 maximum.bioassay.survival.proportion = 1,
                                                                 michaelis.menten.slope = 1,
                                                                 half.population.bioassay.survival.resistance = 900,
                                                                 regression.coefficient = 0.48,
                                                                 regression.intercept = 0.15,
                                                                 current.insecticide.efficacy = 1), 0)
})

