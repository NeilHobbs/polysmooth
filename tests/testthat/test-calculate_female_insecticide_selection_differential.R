test_that("subtraction works", {
  expect_equal(calculate_female_insecticide_selection_differential(female.trait.mean.after.selection = 12,
                                                                   female.trait.mean = 10), 2)
})
