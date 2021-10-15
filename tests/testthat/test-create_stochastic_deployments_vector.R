deployments.vector.10 = create_stochastic_deployments_vector(desired.deployment.interval = 5,
                                                deployment.error = 12,
                                                max.generations = 10)
deployments.vector.100 = create_stochastic_deployments_vector(desired.deployment.interval = 5,
                                                          deployment.error = 12,
                                                          max.generations = 100)
deployments.vector.200 = create_stochastic_deployments_vector(desired.deployment.interval = 5,
                                                          deployment.error = 12,
                                                          max.generations = 200)
deployments.vector.500 = create_stochastic_deployments_vector(desired.deployment.interval = 5,
                                                          deployment.error = 12,
                                                          max.generations = 500)

test_that("total deployment is max generations", {

  expect_equal(sum(deployments.vector.10), 10)
  expect_equal(sum(deployments.vector.100), 100)
  expect_equal(sum(deployments.vector.200), 200)
  expect_equal(sum(deployments.vector.500), 500)


})

