test_that("resistance cannot fall below zero", {
  expect_equal(calculate_intervention_site_after_selection_deployed(intervention.before.selection = 5,
                                                                    response.insecticide.fitness = -10), 0)
})
