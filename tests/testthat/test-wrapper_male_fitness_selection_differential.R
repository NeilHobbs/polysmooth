trait.values = c(1, 10, 1000, 100000)

for(i in 1:length(trait.values))
test_that("turns fixed fitness cost negative regardless of trait.mean", {
  expect_equal(wrapper_male_fitness_selection_differential(male.trait.mean = trait.values[i],
                                                           male.fitness.cost = 1), -1)
})
