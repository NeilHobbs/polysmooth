test_that("heritability error message", {
  expect_error(breeders_equation(selection.differential = 10,
                                 heritability = -0.1),
               "heritability must be between 0 and 1")
})

test_that("heritability error message", {
  expect_error(breeders_equation(selection.differential = 10,
                                 heritability = 1.1),
               "heritability must be between 0 and 1")
})
