#'@title Calculate the relative population size of the mosquitoes in the intervention site who encountered the insecticide and survived
#'
#'@param relative.contributions.after.selection = A vector containing the values of the relative population contributions to the population of mosquitoes for all Polygenic Resistance Scores in population after the insecticide encounter in the intervention site.

get_population_size_exposed_survivors = function(relative.contributions.after.selection){

  population.size.exposed.survivors = sum(relative.contributions.after.selection)

  return(population.size.exposed.survivors)
}
