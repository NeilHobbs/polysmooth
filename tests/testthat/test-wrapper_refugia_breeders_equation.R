test_that("can go down", {
  expect_equal(wrapper_refugia_breeders_equation(refugia.before.selection = 10,
                                                 heritability = 1,
                                                 female.fitness.cost = 1,
                                                 male.fitness.cost = 1), 9)
})

test_that("can go up", {
  expect_equal(wrapper_refugia_breeders_equation(refugia.before.selection = 10,
                                                 heritability = 1,
                                                 female.fitness.cost = -1,
                                                 male.fitness.cost = -1), 11)
})

test_that("cannot go below zero", {
  expect_equal(wrapper_refugia_breeders_equation(refugia.before.selection = 10,
                                                 heritability = 1,
                                                 female.fitness.cost = 100,
                                                 male.fitness.cost = 100), 0)
})
