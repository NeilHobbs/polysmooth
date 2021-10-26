
mix.df = data.frame(mixture.id = c(1, 2, 3 , 4),
                    mixture.part.1 = c(1, 2, 3, 4),
                    mixture.part.2 = c(2, 3, 4, 1))


test_that("makes mixtures properly", {
  expect_equal(make_mixture_sequential_continous(number.of.insecticides = 4), mix.df)
})
