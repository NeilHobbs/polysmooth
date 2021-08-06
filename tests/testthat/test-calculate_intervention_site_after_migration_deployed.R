test_that("cannot fall below zero", {
  expect_equal(calculate_intervention_site_after_migration_deployed(intervention.after.selection = -100,
                                                                    refugia.after.selection = -100,
                                                                    migration.intervention.to.refugia = 0.5,
                                                                    migration.refugia.to.intervention = 0.5,
                                                                    insecticide.population.suppression = 0), 0)
})

test_that("there is complete exchange", {
  expect_equal(calculate_intervention_site_after_migration_deployed(intervention.after.selection = 500,
                                                                    refugia.after.selection = 400,
                                                                    migration.intervention.to.refugia = 1,
                                                                    migration.refugia.to.intervention = 1,
                                                                    insecticide.population.suppression = 0), 400)
})

test_that("there is no exchange", {
  expect_equal(calculate_intervention_site_after_migration_deployed(intervention.after.selection = 500,
                                                                    refugia.after.selection = 400,
                                                                    migration.intervention.to.refugia = 0,
                                                                    migration.refugia.to.intervention = 0,
                                                                    insecticide.population.suppression = 0), 500)
})



test_that("there is  exchange and total population suppression", {
  expect_equal(calculate_intervention_site_after_migration_deployed(intervention.after.selection = 200,
                                                                    refugia.after.selection = 100,
                                                                    migration.intervention.to.refugia = 0.1,
                                                                    migration.refugia.to.intervention = 0.1,
                                                                    insecticide.population.suppression = 1), 100)
})
