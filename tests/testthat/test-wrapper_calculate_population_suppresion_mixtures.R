temp.array = create_starting_array(n.insecticides = 2,
                                   maximum.generations = 10)

deployed.mixture.df = data.frame(
  mixture.part.1 = rep(1, 10),
  mixture.part.2 = rep(1, 10),
  insecticide.efficacy.vector.part.1 = rep(2, 10),
  insecticide.efficacy.vector.part.2 = rep(2, 10))

temp.array["intervention", 1, 1] = 1000
temp.array["intervention", 2, 1] = 1000

test_that("False works", {
  expect_equal(wrapper_calculate_population_suppresion_mixtures(vector.length = 1000,
                                                 current.generation = 2,
                                                 standard.deviation = 30,
                                                 female.insecticide.exposure = 0.5,
                                                 maximum.bioassay.survival.proportion = 1,
                                                 michaelis.menten.slop = 1,
                                                 half.population.bioassay.survival.resistance = 900,
                                                 regression.coefficient = 0.5,
                                                 regression.intercept = 0.15,
                                                 sim.array = temp.array,
                                                 population.suppression = FALSE,
                                                 deployed.mixture = deployed.mixture.df), 0)
})



test_that("no suppression all survive", {
  expect_equal(wrapper_calculate_population_suppresion_mixtures(vector.length = 1000,
                                                                current.generation = 2,
                                                                standard.deviation = 30,
                                                                female.insecticide.exposure = 0.5,
                                                                maximum.bioassay.survival.proportion = 1,
                                                                michaelis.menten.slop = 1,
                                                                half.population.bioassay.survival.resistance = 900,
                                                                regression.coefficient = 1,
                                                                regression.intercept = 1,
                                                                sim.array = temp.array,
                                                                population.suppression = TRUE,
                                                                deployed.mixture = deployed.mixture.df), 0)
})
