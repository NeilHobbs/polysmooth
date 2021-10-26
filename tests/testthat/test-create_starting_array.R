

the.array = create_starting_array(n.insecticides = 12, maximum.generations = 10)

test_that("correct sized array", {
  expect_equal(length(the.array), 240)
})


test_that("initially all filled as NA", {
  expect_equal(sum(is.na(the.array)), 240)
})
