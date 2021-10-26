
df1 = data.frame(mixture.id = c(1,2),
                mixture.part.1=c(1,3),
                mixture.part.2 = c(2,4))

test_that("makes discrete mixtures", {
  expect_equal(select_mixing_stategy(mixture.strategy = "mix.sequential.discrete",
                                     number.of.insecticides = 4), df1)
})



df2 = data.frame(mixture.id = c(1,2, 3, 4),
                 mixture.part.1=c(1,2,3, 4),
                 mixture.part.2 = c(2,3,4, 1))

test_that("makes continous mixtures", {
  expect_equal(select_mixing_stategy(mixture.strategy = "mix.sequential.continous",
                                     number.of.insecticides = 4), df2)
})



df3 = data.frame(mixture.id = c(1,2,3),
                 mixture.part.1=c(1,1,1),
                 mixture.part.2 = c(2,3,4))

test_that("makes continous mixtures", {
  expect_equal(select_mixing_stategy(mixture.strategy = "pyrethroid.plus",
                                     number.of.insecticides = 4), df3)
})
