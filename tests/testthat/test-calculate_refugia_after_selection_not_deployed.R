test_that("resistance cannot fall below zero", {
  expect_equal(calculate_refugia_after_selection_not_deployed(refugia.before.selection = 5,
                                                              response.fitness = -10), 0)
})
