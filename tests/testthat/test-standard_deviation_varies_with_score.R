
test_that("equation works", {
  expect_equal(standard_deviation_varies_with_score(standard.deviation.at.zero = 10,
                                                               current.trait.mean = 0), 10)

  expect_equal(standard_deviation_varies_with_score(standard.deviation.at.zero = 10,
                                                    current.trait.mean = 10), 11)

})
