test_that("coverages must sum to 1", {
  expect_error(proportion_do_not_encounter_micro_mosaic(insecticide.coverage.1 = 0.1,
                                                        insecticide.coverage.2 = 0.5,
                                                        female.exposure = 1), "insecticide.coverage.1 and insecticide.coverage.2 should sum to 1")
})


test_that("calculates correctly", {
  expect_equal(proportion_do_not_encounter_micro_mosaic(insecticide.coverage.1 = 0.5,
                                                        insecticide.coverage.2 = 0.5,
                                                        female.exposure = 1), 0)})
