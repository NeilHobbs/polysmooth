temp.array = create_starting_array(n.insecticides = 2,
                                   maximum.generations = 10)

temp.array["intervention", 1, 1] = 100
temp.array["refugia", 1, 1] = 100
temp.array["intervention", 2, 1] = 100
temp.array["refugia", 2, 1] = 100

deployed.mixture.df = data.frame(insecticide.efficacy.vector.part.1 = rep(1, 10),
                                 insecticide.efficacy.vector.part.2 = rep(1, 10))


test_that("no selection no change", {
  expect_equal(wrapper_intervention_refugia_deployed_dispersal_mixtures(insecticide.population.suppression = TRUE,
                                                         intervention.before.selection = 100,
                                                         female.fitness.cost = 0,
                                                         male.fitness.cost = 0,
                                                         female.insecticide.exposure = 0,
                                                         male.insecticide.exposure = 0,
                                                         standard.deviation = 30,
                                                         vector.length =1000,
                                                         maximum.bioassay.survival.proportion = 1,
                                                         michaelis.menten.slope = 1,
                                                         half.population.bioassay.survival.resistance = 900,
                                                         regression.coefficient = 0.48,
                                                         regression.intercept = 0.15,
                                                         currently.tracked.insecticide =1,
                                                         deployed.mixture = deployed.mixture.df,
                                                         exposure.scaling.factor = 10,
                                                         heritability  =0.3,
                                                         refugia.before.selection = 100,
                                                         dispersal.rate = 0.2,
                                                         intervention.coverage =0.5,
                                                         other.mixture.part = 2,
                                                         sim.array = temp.array,
                                                         current.generation = 2)
, list(100, 100))
})
