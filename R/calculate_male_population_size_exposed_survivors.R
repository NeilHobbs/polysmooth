#'@title Calculate the relative population size of male mosquitoes that have encountered the insecticide and survived.
#'
#'@param relative.male.contribution.after.selection = A vector of the relative population contributions to the population of male mosquitoes for all values of the Polygenic Resistance Score after the insecticide encounter in the intervention site.

calculate_male_population_size_exposed_survivors = function(relative.male.contributions.after.selection){

  male.population.size.exposed.survivors = sum(relative.male.contributions.after.selection, na.rm = TRUE)

  return(male.population.size.exposed.survivors)
}
