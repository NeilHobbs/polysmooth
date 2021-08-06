#'@title Calculate the relative population size of female mosquitoes that have encountered the insecticide and survived.
#'
#'@param relative.female.contribution.after.selection = A vector of the relative population contributions to the population of female mosquitoes for all values of the Polygenic Resistance Score after the insecticide encounter in the intervention site.

calculate_female_population_size_exposed_survivors = function(relative.female.contributions.after.selection){

  female.population.size.exposed.survivors = sum(relative.female.contributions.after.selection, na.rm = TRUE)

  return(female.population.size.exposed.survivors)
}
