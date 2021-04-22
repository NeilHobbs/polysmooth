sim.array1 = create_starting_array(n.insecticides = 3,
                                   maximum.generations = 200)

sim.array1["intervention", 1, 100] = 200
sim.array1["intervention", 2, 100] = 200
sim.array1["intervention", 3, 100] = 20

exposure.values = seq(0, 1, by = 0.1)

for(i in 1:length(exposure.values)){

  test_that("no insecticide efficacy, no population suppression", {
    expect_equal(wrapper_calculate_population_suppresion(current.insecticide.efficacy = 0,
                                                         currently.deployed.insecticide = 2,
                                                         vector.length = 10000,
                                                         current.generation = 101,
                                                         standard.deviation = 30,
                                                         female.insecticide.exposure = exposure.values[i],
                                                         maximum.bioassay.survival.proportion = 1,
                                                         michaelis.menten.slope = 1,
                                                         half.population.bioassay.survival.resistance = 900,
                                                         regression.coefficient = 0.48,
                                                         regression.intercept = 0.15,
                                                         sim.array = sim.array1), 0)
  })
}



