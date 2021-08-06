test_that("subtraction works", {
  expect_equal(calculate_male_insecticide_selection_differential(male.trait.mean.after.selection = 14,
                                                                            male.trait.mean = 10), 4)
})
