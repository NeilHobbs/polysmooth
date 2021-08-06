test_that("subtraction works", {
  expect_equal(calculate_insecticide_selection_differential(mean.post.selection = 15,
                                                            trait.mean = 11), 4)
})
