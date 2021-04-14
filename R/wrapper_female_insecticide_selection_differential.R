#'@title A wrapper function to calculate the insecticide selection differential for female mosquitoes
#'
#'@param intervention.before.selection = The mean Polygenic Resistance Score of the female mosquitoes prior to insecticide selection this generation.
#'@param female.insecticide.exposure = Proportion of female mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide.
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param trait.mean = The mean value of a polygenic trait in a population.
#'@param standard.deviation = The standard deviation of the trait mean in the population.
#'@param maximum.bioassay.survival.proportion = The maximum proportion of mosquitoes that can survive in the bioassay.
#'@param michaelis.menten.slope = The slope in the Michaelis-Menten equation.
#'@param half.population.bioassay.survival.resistance = The Polygenic Resistance Score which gives a 50% survival probability in a WHO cylinder bioassay.
#'@param regression.coefficient = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param regression.intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param current.insecticide.efficacy = The current efficacy of the insecticide, defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.

#'

wrapper_female_insecticide_selection_differential = function(intervention.before.selection,
                                                             female.insecticide.exposure,
                                                             standard.deviation,
                                                             vector.length,
                                                             maximum.bioassay.survival.proportion = 1,
                                                             michaelis.menten.slope = 1,
                                                             half.population.bioassay.survival.resistance = 900,
                                                             regression.coefficient = 0.48,
                                                             regression.intercept = 0.15,
                                                             current.insecticide.efficacy){

  #1. Get the total relative population size of the female mosquitoes prior to any insecticide based selection.
  total.female.population.size = get_female_total_population_size(total.population.size = wrapper_total_population_size(standard.deviation = standard.deviation,
                                                                                                                        vector.length = vector.length))

  #A proportion of the females are not exposed to the insecticide
  female.population.size.unexposed =  calculate_female_population_size_unexposed(total.female.population.size = total.female.population.size,
                                                                                 female.insecticide.exposure = female.insecticide.exposure)

  #of the female mosquitoes that encounter the insecticide, most will likely die.
  relative.female.contributions.after.selection =  calculate_female_density_after_selection(female.insecticide.exposure = female.insecticide.exposure,
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
  female.trait.mean.exposed.survivors = calculate_female_trait_mean_exposed_survivors(vector.length = vector.length,
                                                                                      trait.mean = intervention.before.selection,
                                                                                      standard.deviation = standard.deviation,
                                                                                      maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                      michaelis.menten.slope = michaelis.menten.slope,
                                                                                      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                      regression.coefficient = regression.coefficient,
                                                                                      regression.intercept = regression.intercept,
                                                                                      current.insecticide.efficacy = current.insecticide.efficacy,
                                                                                      female.insecticide.exposure = female.insecticide.exposure)

  #Calculate the relative population size of female exposed survivors
  female.population.size.exposed.survivors = calculate_female_population_size_exposed_survivors(relative.female.contributions.after.selection = relative.female.contributions.after.selection)

  #Calculate the overall relative population size after insecticide selection
  female.population.size.after.selection = calculate_female_population_size_after_selection(female.population.size.exposed.survivors = female.population.size.exposed.survivors,
                                                                                            female.population.size.unexposed = female.population.size.unexposed)

  #Calculate the trait mean for females after insecticide selection
  female.trait.mean.after.selection = calculate_female_trait_mean_post_selection(female.population.size.exposed.survivors = female.population.size.exposed.survivors,
                                                                                 female.trait.mean.exposed.survivors = female.trait.mean.exposed.survivors,
                                                                                 female.population.size.unexposed = female.population.size.unexposed,
                                                                                 female.trait.mean = intervention.before.selection,
                                                                                 female.population.size.after.selection = female.population.size.after.selection)


  #Finally get the insecticide selection differential
  female.insecticide.selection.differential = calculate_female_insecticide_selection_differential(female.trait.mean.after.selection = female.trait.mean.after.selection,
                                                                                                  female.trait.mean = intervention.before.selection)



  #Prevent NA occurring
  female.insecticide.selection.differential = ifelse(is.na(female.insecticide.selection.differential),
                                                     yes = 0,
                                                     no = female.insecticide.selection.differential)

  #Return the female insecticide selection differential
  return(female.insecticide.selection.differential)
}

#
# wrapper_female_insecticide_selection_differential(intervention.before.selection = 0,
#                                                   female.insecticide.exposure = 0.9,
#                                                   standard.deviation = 10,
#                                                   vector.length = 1000,
#                                                   maximum.bioassay.survival.proportion = 1,
#                                                   michaelis.menten.slope = 1,
#                                                   half.population.bioassay.survival.resistance = 900,
#                                                   regression.coefficient = 0.48,
#                                                   regression.intercept = 0.15,
#                                                   current.insecticide.efficacy = 1)

