
#'@title Calculate the relative population contribution to the population of female mosquitoes with a Polygenic Resistance Score  who do not encounter the insecticide.
#'
#'@param female.insecticide.exposure = Proportion of female mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide.
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param trait.mean = The mean value of a polygenic trait in a population.
#'@param standard.deviation = The standard deviation of the trait mean in the population.

calculate_female_density_unexposed = function(vector.length,
                                              trait.mean,
                                              standard.deviation,
                                              female.insecticide.exposure){

  #step 1:create the Normal Distribution of Polygenic Resistance Values
  normal.distribution = create_normal_distribution(vector.length = vector.length,
                                                   trait.mean = trait.mean,
                                                   standard.deviation = standard.deviation)

  #step 2:get the probability densities
  relative.female.contribution.before.selection =  calculate_density_of_trait_values(vector.length = vector.length,
                                                                              trait.mean = trait.mean,
                                                                              standard.deviation = standard.deviation)/2


  #step 3:calculate the change in probability densities

  relative.female.contribution.unexposed = relative.female.contribution.before.selection * (1-female.insecticide.exposure)

  return(relative.female.contribution.unexposed)
}


