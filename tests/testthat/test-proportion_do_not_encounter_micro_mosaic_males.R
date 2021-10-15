test_that("all encounter insecticide", {
  expect_equal(proportion_do_not_encounter_micro_mosaic_males(insecticide.coverage.1 = 0.5,
                                               insecticide.coverage.2 = 0.5,
                                               female.exposure = 1,
                                               male.exposure = 1), 0)
})


test_that("none encounter encounter insecticide", {
  expect_equal(proportion_do_not_encounter_micro_mosaic_males(insecticide.coverage.1 = 0.5,
                                                              insecticide.coverage.2 = 0.5,
                                                              female.exposure = 0,
                                                              male.exposure = 0), 1)
})


test_that("none encounter encounter insecticide", {
  expect_error(proportion_do_not_encounter_micro_mosaic_males(insecticide.coverage.1 = 0.47,
                                                              insecticide.coverage.2 = 0.13,
                                                              female.exposure = 0,
                                                              male.exposure = 0), "insecticide.coverage.1 and insecticide.coverage.2 should sum to 1")
})
