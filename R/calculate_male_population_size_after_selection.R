#'@title Calculate the relative population size of all the male mosquitoes in the intervention site after insecticide selection.
#'
#'@param male.population.size.exposed.survivors = The relative population size of male mosquitoes that have encountered the insecticide and survived.
#'@param male.population.size.unexposed = The relative population size of the unexposed male mosquitoes in the intervention site.



calculate_male_population_size_after_selection = function(male.population.size.exposed.survivors,
                                                          male.population.size.unexposed){

    male.population.size.after.selection = male.population.size.exposed.survivors + male.population.size.unexposed

  return(male.population.size.after.selection)

}
