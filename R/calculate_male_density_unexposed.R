
#'@title Calculate the relative population contribution to the population of male mosquitoes with a Polygenic Resistance Score  who do not encounter the insecticide.
#'
#'@param female.insecticide.exposure = Proportion of female mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide.
#'@param male.insecticide.exposure = Proportion of male mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide as a proportion of the exposure of female mosquitoes
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param trait.mean = The mean value of a polygenic trait in a population.
#'@param standard.deviation = The standard deviation of the trait mean in the population.

calculate_male_density_unexposed = function(vector.length,
                                              trait.mean,
                                              standard.deviation,
                                              female.insecticide.exposure,
                                              male.insecticide.exposure){

if(female.insecticide.exposure > 1 | female.insecticide.exposure < 0){stop("female.insecticide.exposure must be between 0 and 1")}
if(male.insecticide.exposure > 1 | male.insecticide.exposure < 0){stop("male.insecticide.exposure must be between 0 and 1")}

  #step 1:create the Normal Distribution of Polygenic Resistance Values
  normal.distribution = create_normal_distribution(vector.length = vector.length,
                                                   trait.mean = trait.mean,
                                                   standard.deviation = standard.deviation)

  #step 2:get the probability densities
  relative.male.contribution.before.selection =  calculate_density_of_trait_values(vector.length = vector.length,
                                                                                     trait.mean = trait.mean,
                                                                                     standard.deviation = standard.deviation)/2


  #step 3:calculate the change in probability densities

  relative.male.contribution.unexposed = relative.male.contribution.before.selection * (1-(female.insecticide.exposure * male.insecticide.exposure))

  return(relative.male.contribution.unexposed)
}


