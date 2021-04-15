test_that("complete exchange", {
  expect_equal(refugia_after_migration(intervention.after.selection = 10,
                                       joining.from.intervetion = 1,
                                       insecticide.population.suppression = 0,
                                       refugia.after.selection = 20,
                                       staying.in.refugia = 0), 10)
})


test_that("none leaving, none joining", {
  expect_equal(refugia_after_migration(intervention.after.selection = 10,
                                       joining.from.intervetion = 0,
                                       insecticide.population.suppression = 0,
                                       refugia.after.selection = 20,
                                       staying.in.refugia = 1), 20)
})

test_that("population suppression works", {
  expect_equal(refugia_after_migration(intervention.after.selection = 200,
                                       joining.from.intervetion = 1,
                                       insecticide.population.suppression = 1,
                                       refugia.after.selection = 30,
                                       staying.in.refugia = 1), 30)
})

test_that("NA becomes zero", {
  expect_equal(refugia_after_migration(intervention.after.selection = 10,
                                       joining.from.intervetion = 0,
                                       insecticide.population.suppression = 0,
                                       refugia.after.selection = 20,
                                       staying.in.refugia = 0), 0)
})

