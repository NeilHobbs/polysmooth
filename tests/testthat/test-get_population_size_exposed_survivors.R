temp.vector = c(NA, NA, NA, 5, 5, 5, 5)

test_that("addition works removing NAs", {
  expect_equal(get_population_size_exposed_survivors(relative.contributions.after.selection = temp.vector), 20)
})
