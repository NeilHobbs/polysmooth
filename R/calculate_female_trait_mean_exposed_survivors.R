#'@title Calculate the mean Polygenic Resistance Score of the female mosquitoes exposed to and surviving the insecticide encounter.
#'
#'@param relative.female.contribution.after.selection = The relative population contribution to the population of female mosquitoes with a Polygenic Resistance Score  after the insecticide encounter in the intervention site.
#'@param trait.mean = The mean value of a polygenic trait in a population.

calculate_female_trait_mean_exposed_survivors = function(relative.female.contribution.after.selection,
                                                         trait.mean
                                                         ){

  female.trait.mean.exposed.survivors = sum(relative.female.contribution.after.selection * trait.mean)

  return(female.trait.mean.exposed.survivors)
}
