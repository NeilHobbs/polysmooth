test_that("division works", {
  expect_equal(calculate_initial_efficacy_from_insecticide_concentration_mixtures(mixture.dose = 50,
                                                                                  recommended.insecticide.dose = 100), 0.5)
})
