temp.vector = c(NA, NA, 5, 5, 5, 5)

test_that("addtion works removing NAs", {
  expect_equal(calculate_male_population_size_exposed_survivors(relative.male.contributions.after.selection = temp.vector), 20)
})
