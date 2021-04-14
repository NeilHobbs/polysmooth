test_that("unchanged as no mortality", {
  expect_equal(calculate_density_after_selection(insecticide.exposure = 1,
                                                 vector.length = 1000,
                                                 trait.mean = 10,
                                                 standard.deviation = 10,
                                                 maximum.bioassay.survival.proportion = 1,
                                                 michaelis.menten.slope = 1,
                                                 half.population.bioassay.survival.resistance = 900,
                                                 regression.coefficient = 1,
                                                 regression.intercept = 0,
                                                 current.insecticide.efficacy = 0),
               calculate_density_of_trait_values(vector.length = 1000,
                                                 trait.mean = 10,
                                                 standard.deviation = 10))
})


test_that("insecticide.exposure too low", {

  expect_error(calculate_density_after_selection(insecticide.exposure = -0.1,
                vector.length = 1000,
                trait.mean = 10,
                standard.deviation = 10,
                maximum.bioassay.survival.proportion = 1,
                michaelis.menten.slope = 1,
                half.population.bioassay.survival.resistance = 900,
                regression.coefficient = 1,
                regression.intercept = 0,
                current.insecticide.efficacy = 0), "insecticide.exposure must be between 0 and 1")

})


test_that("insecticide.exposure too high", {

  expect_error(calculate_density_after_selection(insecticide.exposure = 1.1,
                                                 vector.length = 1000,
                                                 trait.mean = 10,
                                                 standard.deviation = 10,
                                                 maximum.bioassay.survival.proportion = 1,
                                                 michaelis.menten.slope = 1,
                                                 half.population.bioassay.survival.resistance = 900,
                                                 regression.coefficient = 1,
                                                 regression.intercept = 0,
                                                 current.insecticide.efficacy = 0), "insecticide.exposure must be between 0 and 1")

})


