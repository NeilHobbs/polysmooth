test_that("division works", {
  expect_equal(calculate_initial_efficacy_from_insecticide_concentration(applied.insecticide.dose = 80,
                                                                                    recommended.insecticide.dose = 100), 0.8)
})
