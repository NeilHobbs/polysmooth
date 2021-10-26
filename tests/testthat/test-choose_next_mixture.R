test_that("goes to next mixture works", {
  expect_equal(choose_next_mixture(previous.mixture = 3,
                                   total.mixtures = 4,
                                   available.mixtures = c(1,2,4)), 4)
})



test_that("goes to first mixture", {
  expect_equal(choose_next_mixture(previous.mixture = 3,
                                   total.mixtures = 4,
                                   available.mixtures = c(1,2)), 1)
})


test_that("goes to first available mixture", {
  expect_equal(choose_next_mixture(previous.mixture = 4,
                                   total.mixtures = 4,
                                   available.mixtures = c(2, 3)), 2)
})
