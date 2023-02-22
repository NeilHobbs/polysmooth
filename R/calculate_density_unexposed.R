#'@title Calculate the relative population contribution to the population of mosquitoes with a Polygenic Resistance Score that did not encounter the insecticide in the intervention site.
#'
#'@param insecticide.exposure = The proportion of the mosquito population in the intervention site being exposed to an insecticide.
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param trait.mean = The mean value of a polygenic trait in a population.
#'@param standard.deviation = The standard deviation of the trait mean in the population.

calculate_density_unexposed = function(vector.length,
                                        trait.mean,
                                        standard.deviation,
                                        insecticide.exposure){

 # if(insecticide.exposure > 1| insecticide.exposure<0){stop("insecticide.exposure must be between 0 and 1")}

  #step 1:create the Normal Distribution of Polygenic Resistance Values
  normal.distribution = create_normal_distribution(vector.length = vector.length,
                                                   trait.mean = trait.mean,
                                                   standard.deviation = standard.deviation)

  #step 2:get the probability densities
  relative.contribution.before.selection =  calculate_density_of_trait_values(vector.length = vector.length,
                                                                              trait.mean = trait.mean,
                                                                              standard.deviation = standard.deviation)


  #step 3:calculate the change in probability densities

  relative.contribution.unexposed = relative.contribution.before.selection * (1-insecticide.exposure)

  return(relative.contribution.unexposed)
}
