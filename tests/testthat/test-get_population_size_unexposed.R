test_that("addition works removing NAs", {
  expect_equal(get_population_size_unexposed(relative.contributions.unexposed = c(NA, NA, NA, 5, 5, 5, 5)), 20)
})
