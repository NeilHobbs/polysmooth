test_that("subtraction works", {
  expect_equal(calculate_selection_differential(trait.mean = 10,
                                                trait.value.parents = 14), 4)
})
