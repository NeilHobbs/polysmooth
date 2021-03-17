#'@title Calculate the mean Polygenic Resistance Score of the male mosquitoes exposed to and surviving the insecticide encounter.
#'
#'@param relative.male.contribution.after.selection = The relative population contribution to the population of male mosquitoes with a Polygenic Resistance Score  after the insecticide encounter in the intervention site.
#'@param trait.mean = The mean value of a polygenic trait in a population.

calculate_male_trait_mean_exposed_survivors = function(relative.male.contribution.after.selection,
                                                         trait.mean
){

  male.trait.mean.exposed.survivors = sum(relative.male.contribution.after.selection * trait.mean)

  return(male.trait.mean.exposed.survivors)
}
