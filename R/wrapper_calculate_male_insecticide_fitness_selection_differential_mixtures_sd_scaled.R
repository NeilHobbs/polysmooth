#'@title A wrapper function to calculate the male insecticide and fitness cost selection differential
#'
#'@param male.trait.mean = The mean Polygenic Resistance Score to the insecticide for male mosquitoes before selection has occurred.
#'@param female.insecticide.exposure =Proportion of female mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide.
#'@param male.insecticide.exposure = Proportion of male mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide as a proportion of the exposure of female mosquitoes
#'@param standard.deviation = The standard deviation of the trait mean in the population.
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param maximum.bioassay.survival.proportion = The maximum proportion of mosquitoes that can survive in the bioassay.
#'@param michaelis.menten.slope = The slope in the Michaelis-Menten equation.
#'@param half.population.bioassay.survival.resistance = The Polygenic Resistance Score which gives a 50% survival probability in a WHO cylinder bioassay.
#'@param regression.coefficient = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param regression.intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param current.insecticide.efficacy = The current efficacy of the insecticide, defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.
#'@param exposure.scaling.factor = A factor which converts the insecticide exposure to the selection differential.



wrapper_calculate_male_insecticide_fitness_selection_differential_mixtures_sd_scaled = function(male.trait.mean,
                                                                                                female.insecticide.exposure,
                                                                                                male.insecticide.exposure,
                                                                                                z.sd.intercept,
                                                                                                z.sd.coefficient,
                                                                                                male.fitness.cost,
                                                                                                vector.length,
                                                                                                maximum.bioassay.survival.proportion,
                                                                                                michaelis.menten.slope,
                                                                                                half.population.bioassay.survival.resistance,
                                                                                                regression.coefficient,
                                                                                                regression.intercept,
                                                                                                current.insecticide.efficacy,
                                                                                                exposure.scaling.factor,
                                                                                                survival.to.other.insecticide){


  male.fitness.selection.differential = sd_changes_with_z(current.z = male.trait.mean,
                                                          z.sd.intercept = z.sd.intercept,
                                                          z.sd.coefficient = z.sd.coefficient) * male.fitness.cost



  male.insecticide.selection.differential = wrapper_male_insecticide_selection_differential_mixtures(intervention.before.selection = male.trait.mean,
                                                                                                     female.insecticide.exposure = female.insecticide.exposure,
                                                                                                     male.insecticide.exposure = male.insecticide.exposure,
                                                                                                     standard.deviation = sd_changes_with_z(current.z = male.trait.mean,
                                                                                                                                            z.sd.intercept = z.sd.intercept,
                                                                                                                                            z.sd.coefficient = z.sd.coefficient),
                                                                                                     vector.length = vector.length,
                                                                                                     maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                     michaelis.menten.slope = michaelis.menten.slope,
                                                                                                     half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                     regression.coefficient = regression.coefficient,
                                                                                                     regression.intercept = regression.intercept,
                                                                                                     current.insecticide.efficacy = current.insecticide.efficacy,
                                                                                                     survival.to.other.insecticide = survival.to.other.insecticide)

  male.insecticide.fitness.selection.differential = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.selection.differential,
                                                                                                              exposure.scaling.factor = exposure.scaling.factor,
                                                                                                              male.fitness.selection.differential = male.fitness.selection.differential)


  return(male.insecticide.fitness.selection.differential)

}
