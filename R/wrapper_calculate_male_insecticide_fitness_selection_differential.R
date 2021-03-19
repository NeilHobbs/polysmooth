#'@title A wrapper function to calculate the male insecticide and fitness cost selection differential
#'
#'@param male.trait.mean =
#'@param female.insecticide.exposure =
#'@param male.insecticide.exposure =
#'@param standard.deviation =
#'@param vector.length =
#'@param maximum.bioassay.survival.proportion =
#'@param michaelis.menten.slope =
#'@param half.population.bioassay.survival.resistance =
#'@param regression.coefficient =
#'@param regression.intercept =
#'@param current.insecticide.efficacy =
#'@param exposure.scaling.factor =



wrapper_calculate_male_insecticide_fitness_selection_differential = function(male.trait.mean,
                                                                             female.insecticide.exposure,
                                                                             male.insecticide.exposure,
                                                                             standard.deviation,
                                                                             vector.length,
                                                                             maximum.bioassay.survival.proportion,
                                                                             michaelis.menten.slope,
                                                                             half.population.bioassay.survival.resistance,
                                                                             regression.coefficient,
                                                                             regression.intercept,
                                                                             current.insecticide.efficacy,
                                                                             exposure.scaling.factor){


  male.fitness.selection.differential = wrapper_male_fitness_selection_differential(male.trait.mean = male.trait.mean,
                                                                                    male.fitness.cost = male.fitness.cost)

  male.insecticide.selection.differential = wrapper_male_insecticide_selection_differential(intervention.before.selection = male.trait.mean,
                                                                                            female.insecticide.exposure = female.insecticide.exposure,
                                                                                            male.insecticide.exposure = male.insecticide.exposure,
                                                                                            standard.deviation = standard.deviation,
                                                                                            vector.length = vector.length,
                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                            regression.coefficient = regression.coefficient,
                                                                                            regression.intercept = regression.intercept,
                                                                                            current.insecticide.efficacy = current.insecticide.efficacy)

  male.insecticide.fitness.selection.differential = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.selection.differential,
                                                                                                              exposure.scaling.factor = exposure.scaling.factor,
                                                                                                              male.fitness.selection.differential = male.fitness.selection.differential)


  return(male.insecticide.fitness.selection.differential)

}
