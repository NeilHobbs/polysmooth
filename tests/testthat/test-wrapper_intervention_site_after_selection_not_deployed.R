test_that("resistance cannot fall below zero", {
  expect_equal(wrapper_intervention_site_after_selection_not_deployed(heritability = 1,
                                                                      intervention.before.selection = 10,
                                                                      female.fitness.cost = 20,
                                                                      male.fitness.cost = 20), 0)
})


test_that("resistance goes down", {
  expect_equal(wrapper_intervention_site_after_selection_not_deployed(heritability = 1,
                                                                      intervention.before.selection = 10,
                                                                      female.fitness.cost = 5,
                                                                      male.fitness.cost = 5), 5)
})


test_that("resistance goes up", {
  expect_equal(wrapper_intervention_site_after_selection_not_deployed(heritability = 1,
                                                                      intervention.before.selection = 10,
                                                                      female.fitness.cost = -5,
                                                                      male.fitness.cost = -5), 15)
})

test_that("resistance not heritable", {
  expect_equal(wrapper_intervention_site_after_selection_not_deployed(heritability = 0,
                                                                      intervention.before.selection = 10,
                                                                      female.fitness.cost = 5,
                                                                      male.fitness.cost = 5), 10)
})
