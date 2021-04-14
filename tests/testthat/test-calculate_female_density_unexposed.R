test_that("error when insecticide.exposure too low", {
  expect_error(calculate_female_density_unexposed(vector.length = 1000,
                                                  trait.mean = 10,
                                                  standard.deviation - 10,
                                                  female.insecticide.exposure = -0.1), "female.insecticide.exposure must be between 0 and 1")
})

test_that("error when insecticide.exposure too high", {
  expect_error(calculate_female_density_unexposed(vector.length = 1000,
                                                  trait.mean = 10,
                                                  standard.deviation = 10,
                                                  female.insecticide.exposure = 1.1), "female.insecticide.exposure must be between 0 and 1")
})
