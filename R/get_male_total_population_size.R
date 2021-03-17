#'@title Get the  total relative population size of male mosquitoes in the intervention site before insecticide selection.
#'
#'@param total.population.size = The relative population size of the intervention site prior to selection.

get_male_total_population_size = function(total.population.size){

  total.male.population.size = total.population.size / 2

  return(total.male.population.size)

}
