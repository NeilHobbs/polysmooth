test_that("exchange to the intervention site", {
  expect_equal(wrapper_intervention_refugia_not_deployed_dispersal(insecticide.population.suppression = 0,
                                                               intervention.before.selection = 0,
                                                               female.fitness.cost = 0,
                                                               male.fitness.cost = 0,
                                                               heritability = 1,
                                                               refugia.before.selection = 10,
                                                               dispersal.rate = 0.5,
                                                               intervention.coverage = 0.5)[[1]], 2.5)
})

test_that("exchange to the refugia site", {
  expect_equal(wrapper_intervention_refugia_not_deployed_dispersal(insecticide.population.suppression = 0,
                                                               intervention.before.selection = 10,
                                                               female.fitness.cost = 0,
                                                               male.fitness.cost = 0,
                                                               heritability = 1,
                                                               refugia.before.selection = 0,
                                                               dispersal.rate = 0.5,
                                                               intervention.coverage = 0.5)[[2]], 2.5)
})

test_that("population suppression works", {
  expect_equal(wrapper_intervention_refugia_not_deployed_dispersal(insecticide.population.suppression = 1,
                                                               intervention.before.selection = 1000,
                                                               female.fitness.cost = 0,
                                                               male.fitness.cost = 0,
                                                               heritability = 1,
                                                               refugia.before.selection = 0,
                                                               dispersal.rate = 0.5,
                                                               intervention.coverage = 0.5)[[2]], 0)
})




