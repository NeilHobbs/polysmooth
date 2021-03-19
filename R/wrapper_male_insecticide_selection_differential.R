#'@title A wrapper function to calculate the insecticide selection differential for male mosquitoes
#'
#'@param intervention.before.selection = The mean Polygenic Resistance Score of the male mosquitoes prior to insecticide selection this generation.
#'@param female.insecticide.exposure = Proportion of female mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide.
#'@param male.insecticide.exposure = Proportion of male mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide as a proportion of the exposure of female mosquitoes
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param trait.mean = The mean value of a polygenic trait in a population.
#'@param standard.deviation = The standard deviation of the trait mean in the population.
#'@param maximum.bioassay.survival.proportion = The maximum proportion of mosquitoes that can survive in the bioassay.
#'@param michaelis.menten.slope = The slope in the Michaelis-Menten equation.
#'@param half.population.bioassay.survival.resistance = The Polygenic Resistance Score which gives a 50% survival probability in a WHO cylinder bioassay.
#'@param regression.coefficient = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param regression.intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param current.insecticide.efficacy = The current efficacy of the insecticide, defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.


wrapper_male_insecticide_selection_differential = function(intervention.before.selection,
                                                           male.insecticide.exposure,
                                                           female.insecticide.exposure,
                                                           standard.deviation,
                                                           vector.length,
                                                           maximum.bioassay.survival.proportion = 1,
                                                           michaelis.menten.slope = 1,
                                                           half.population.bioassay.survival.resistance = 900,
                                                           regression.coefficient = 0.48,
                                                           regression.intercept = 0.15,
                                                           current.insecticide.efficacy){

  total.male.population.size = get_male_total_population_size(total.population.size = wrapper_total_population_size(standard.deviation = standard.deviation,
                                                                                                                    vector.length = vector.length))

  male.population.size.unexposed =  calculate_male_population_size_unexposed(total.male.population.size = total.male.population.size,
                                                                             male.insecticide.exposure = male.insecticide.exposure,
                                                                             female.insecticide.exposure = female.insecticide.exposure)


  relative.male.contribution.after.selection =  calculate_male_density_after_selection(female.insecticide.exposure = female.insecticide.exposure,
                                                                                       male.insecticide.exposure = male.insecticide.exposure,
                                                                                       vector.length = vector.length,
                                                                                       trait.mean = intervention.before.selection,
                                                                                       standard.deviation = standard.deviation,
                                                                                       maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                       michaelis.menten.slope = michaelis.menten.slope,
                                                                                       half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                       regression.coefficient = regression.coefficient,
                                                                                       regression.intercept = regression.intercept,
                                                                                       current.insecticide.efficacy = current.insecticide.efficacy)


  #calculate the mean trait value of the exposed survivors
  male.trait.mean.exposed.survivors = calculate_male_trait_mean_exposed_survivors(relative.male.contribution.after.selection = relative.male.contribution.after.selection,
                                                                                  trait.mean = intervention.before.selection,
                                                                                  vector.length = vector.length,
                                                                                  standard.deviation = standard.deviation)

  #Calculate the relative population size of male exposed survivors
  male.population.size.exposed.survivors = calculate_male_population_size_exposed_survivors(relative.male.contributions.after.selection = relative.male.contribution.after.selection)


  #Calculate the overall relative population size after insecticide selection
  male.population.size.after.selection = calculate_male_population_size_after_selection(male.population.size.exposed.survivors = male.population.size.exposed.survivors,
                                                                                        male.population.size.unexposed = male.population.size.unexposed)

  #Calculate the trait mean for males after insecticide selection
  male.trait.mean.after.selection = calculate_male_trait_mean_post_selection(male.population.size.exposed.survivors = male.population.size.exposed.survivors,
                                                                             male.trait.mean.exposed.survivors = male.trait.mean.exposed.survivors,
                                                                             male.population.size.unexposed = male.population.size.unexposed,
                                                                             male.trait.mean = intervention.before.selection,
                                                                             male.population.size.after.selection = male.population.size.after.selection)


  #Finally get the insecticide selection differential
  male.insecticide.selection.differential = calculate_male_insecticide_selection_differential(male.trait.mean.after.selection = male.trait.mean.after.selection,
                                                                                              male.trait.mean = intervention.before.selection)


  #Return the male insecticide selection differential
  return(male.insecticide.selection.differential)
}

#
# wrapper_male_insecticide_selection_differential(intervention.before.selection = 0,
#                                                   male.insecticide.exposure = 0.4,
#                                                   female.insecticide.exposure = 0.9,
#                                                   standard.deviation = 10,
#                                                   vector.length = 1000,
#                                                   maximum.bioassay.survival.proportion = 1,
#                                                   michaelis.menten.slope = 1,
#                                                   half.population.bioassay.survival.resistance = 900,
#                                                   regression.coefficient = 0.48,
#                                                   regression.intercept = 0.15,
#                                                   current.insecticide.efficacy = 1)

