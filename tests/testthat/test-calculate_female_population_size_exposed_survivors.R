##Create a vector:
temp.vector = c(NA, NA, 5, 5, 5, 5)

test_that("addition works when NA present", {
  expect_equal(calculate_female_population_size_exposed_survivors(relative.female.contributions.after.selection = temp.vector), 20)
})
