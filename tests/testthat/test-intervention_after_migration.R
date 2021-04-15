test_that("complete exchange", {
  expect_equal(intervention_after_migration(intervention.after.selection = 20,
                                            staying.in.intervention = 0,
                                            insecticide.population.suppression = 0,
                                            joining.from.refugia = 1,
                                            refugia.after.selection = 10), 10)
})


test_that("none leaving, none joining", {
  expect_equal(intervention_after_migration(intervention.after.selection = 20,
                                            staying.in.intervention = 1,
                                            insecticide.population.suppression = 0,
                                            joining.from.refugia = 0,
                                            refugia.after.selection = 10), 20)
})

test_that("population suppression works", {
  expect_equal(intervention_after_migration(intervention.after.selection = 20,
                                            staying.in.intervention = 1,
                                            insecticide.population.suppression = 0.5,
                                            joining.from.refugia = 0.5,
                                            refugia.after.selection = 10), 15)
})

test_that("NA becomes zero", {
  expect_equal(intervention_after_migration(intervention.after.selection = 20,
                                            staying.in.intervention = 0,
                                            insecticide.population.suppression = 0.5,
                                            joining.from.refugia = 0,
                                            refugia.after.selection = 10), 0)
})

