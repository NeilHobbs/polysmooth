test_that("error when female.insecticide.exposure too low", {
  expect_error(calculate_male_density_unexposed(vector.length = 1000,
                                                  trait.mean = 10,
                                                  standard.deviation - 10,
                                                  female.insecticide.exposure = -0.1,
                                                male.insecticide.exposure = 0.5), "female.insecticide.exposure must be between 0 and 1")
})

test_that("error when female.insecticide.exposure too high", {
  expect_error(calculate_male_density_unexposed(vector.length = 1000,
                                                  trait.mean = 10,
                                                  standard.deviation = 10,
                                                  female.insecticide.exposure = 1.1,
                                                male.insecticide.exposure = 0.5), "female.insecticide.exposure must be between 0 and 1")
})

test_that("error when male.insecticide.exposure too low", {
  expect_error(calculate_male_density_unexposed(vector.length = 1000,
                                                  trait.mean = 10,
                                                  standard.deviation - 10,
                                                  female.insecticide.exposure = 0.5,
                                                male.insecticide.exposure = -0.1), "male.insecticide.exposure must be between 0 and 1")
})

test_that("error when male.insecticide.exposure too high", {
  expect_error(calculate_male_density_unexposed(vector.length = 1000,
                                                  trait.mean = 10,
                                                  standard.deviation = 10,
                                                  female.insecticide.exposure = 0.5,
                                                  male.insecticide.exposure = 1.1), "male.insecticide.exposure must be between 0 and 1")
})
