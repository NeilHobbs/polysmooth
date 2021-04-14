test_that("heritability error message", {
  expect_error(breeders_equation_male_female(heritability = -0.1,
                                             female.selection.differential = 10,
                                             male.selection.differential = 5), "heritability must be between 0 and 1")
})

test_that("heritability error message", {
  expect_error(breeders_equation_male_female(heritability = 1.1,
                                             female.selection.differential = 10,
                                             male.selection.differential = 5), "heritability must be between 0 and 1")
})
