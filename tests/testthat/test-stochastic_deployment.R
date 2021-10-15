deployment.vector = c()
for(i in 1:10000){
deployment.vector[i] = stochastic_deployment(desired.deployment.interval = 0,
                      deployment.error = 12)
}

check.deployment = deployment.vector>0

test_that("deployment can never be less than zero", {
  expect_equal(check.deployment, rep(TRUE, 10000))
})

test_that("spans range, to min", {
  expect_equal(min(deployment.vector), 1)
})

test_that("spans range, to max", {
  expect_equal(max(deployment.vector), 12)
})
