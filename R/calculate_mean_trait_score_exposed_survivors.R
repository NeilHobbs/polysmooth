#'@title Calculate the mean Polygenic Resistance Score of the mosquitoes who encounter the insecticide and survive.
#'
#'@param insecticide.exposure = The proportion of the mosquito population in the intervention site being exposed to an insecticide.
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param trait.mean = The mean value of a polygenic trait in a population.
#'@param standard.deviation = The standard deviation of the trait mean in the population.
#'@param maximum.bioassay.survival.proportion = The maximum proportion of mosquitoes that can survive in the bioassay.
#'@param michaelis.menten.slope = The slope in the Michaelis-Menten equation.
#'@param half.population.bioassay.survival.resistance = The Polygenic Resistance Score which gives a 50% survival probability in a WHO cylinder bioassay.
#'@param regression.coefficient = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param regression.intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param current.insecticide.efficacy = The current efficacy of the insecticide, defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.

calculate_mean_trait_score_exposed_survivors = function(vector.length,
                                                        trait.mean,
                                                        standard.deviation,
                                                        maximum.bioassay.survival.proportion = 1,
                                                        michaelis.menten.slope = 1,
                                                        half.population.bioassay.survival.resistance = 900,
                                                        regression.coefficient = 0.48,
                                                        regression.intercept = 0.15,
                                                        current.insecticide.efficacy,
                                                        insecticide.exposure){

  #step 1:create the normal.distribution of the trait

  normal.distribution = create_normal_distribution(vector.length = vector.length,
                                                   trait.mean = trait.mean,
                                                   standard.deviation = standard.deviation)


  #step 2: calculate the density after insecticide selection
  relative.contribution.after.selection=  calculate_density_after_selection(insecticide.exposure = insecticide.exposure,
                                                                            vector.length = vector.length,
                                                                            trait.mean = trait.mean,
                                                                            standard.deviation = standard.deviation,
                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                            regression.coefficient = regression.coefficient,
                                                                            regression.intercept = regression.intercept,
                                                                            current.insecticide.efficacy = current.insecticide.efficacy)

  #step 3: calculate the mean score of the survivors
  mean.trait.score.exposed.survivors = sum(relative.contribution.after.selection * normal.distribution)

  return(mean.trait.score.exposed.survivors)
}
