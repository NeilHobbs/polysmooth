#'@title A wrapper function to calculate the female insecticide and fitness cost selection differential
#'
#'@param female.trait.mean =
#'@param female.insecticide.exposure =
#'@param standard.deviation =
#'@param vector.length =
#'@param maximum.bioassay.survival.proportion =
#'@param michaelis.menten.slope =
#'@param half.population.bioassay.survival.resistance =
#'@param regression.coefficient =
#'@param regression.intercept =
#'@param current.insecticide.efficacy =
#'@param exposure.scaling.factor =



wrapper_calculate_female_insecticide_fitness_selection_differential = function(female.trait.mean,
                                                                               female.insecticide.exposure,
                                                                               standard.deviation,
                                                                               vector.length,
                                                                               maximum.bioassay.survival.proportion,
                                                                               michaelis.menten.slope,
                                                                               half.population.bioassay.survival.resistance,
                                                                               regression.coefficient,
                                                                               regression.intercept,
                                                                               current.insecticide.efficacy,
                                                                               exposure.scaling.factor,
                                                                               female.fitness.cost){


  female.fitness.selection.differential = wrapper_female_fitness_selection_differential(female.trait.mean = female.trait.mean,
                                                                                        female.fitness.cost = female.fitness.cost)

  female.insecticide.selection.differential = wrapper_female_insecticide_selection_differential(intervention.before.selection = female.trait.mean,
                                                                                                female.insecticide.exposure = female.insecticide.exposure,
                                                                                                standard.deviation = standard.deviation,
                                                                                                vector.length = vector.length,
                                                                                                maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                michaelis.menten.slope = michaelis.menten.slope,
                                                                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                regression.coefficient = regression.coefficient,
                                                                                                regression.intercept = regression.intercept,
                                                                                                current.insecticide.efficacy = current.insecticide.efficacy)

  female.insecticide.fitness.selection.differential = calculate_female_insecticide_fitness_selection_differential(female.insecticide.selection.differential = female.insecticide.selection.differential,
                                                                                                                  exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                  female.fitness.selection.differential = female.fitness.selection.differential)


  return(female.insecticide.fitness.selection.differential)

}
