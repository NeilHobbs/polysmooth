test_that("population size unaffected by trait.mean", {
  expect_equal(get_total_population_size(relative.contributions.before.selection = calculate_density_of_trait_values(vector.length = 1000,
                                                                                                                     trait.mean = 100,
                                                                                                                     standard.deviation = 10)),
               get_total_population_size(relative.contributions.before.selection = calculate_density_of_trait_values(vector.length = 1000,
                                                                                                                     trait.mean = 100000000,
                                                                                                                     standard.deviation = 10)))
})

