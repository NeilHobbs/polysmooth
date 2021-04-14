trait.mean.values = c(0, 10, 20, 40, 10000)

for(i in 1:5){
  test_that("returns the trait mean when no insecticide", {
    expect_equal(calculate_male_trait_mean_exposed_survivors(vector.length = 10000,
                                                             trait.mean = trait.mean.values[i],
                                                             standard.deviation = 40,
                                                             maximum.bioassay.survival.proportion = 1,
                                                             michaelis.menten.slope = 1,
                                                             half.population.bioassay.survival.resistance = 900,
                                                             regression.coefficient = 1,
                                                             regression.intercept = 0,
                                                             current.insecticide.efficacy = 0,
                                                             female.insecticide.exposure = 1,
                                                             male.insecticide.exposure = 1)
                 , trait.mean.values[i])
  })}
