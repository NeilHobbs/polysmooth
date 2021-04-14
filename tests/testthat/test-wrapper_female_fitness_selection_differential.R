trait.values = c(1, 10, 100, 1000, 10000)

for(i in 1:length(trait.values)){
test_that("turns fixed value negative regardless of trait.mean", {
  expect_equal(wrapper_female_fitness_selection_differential(female.trait.mean = trait.values[i],
                                                             female.fitness.cost = 1), -1)
})
}
