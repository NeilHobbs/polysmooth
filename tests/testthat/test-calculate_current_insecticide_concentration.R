test_that("multiplication works", {
  expect_equal(calculate_current_insecticide_concentration(applied.concentration,
                                                           instantaneous.decay.rate,
                                                           generations.since.deployment), 4)
})
