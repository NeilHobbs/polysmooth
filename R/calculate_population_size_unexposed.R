#'@title Calculate the relative population size of the mosquitoes in the intervention site who did not encounter the insecticide.
#'
#'@param total.population.size = The relative population size of the intervention site prior to selection.
#'@param insecticide.exposure = The proportion of the mosquito population in the intervention site being exposed to an insecticide.


calculate_population_size_unexposed = function(total.population.size,
                                               insecticide.exposure){

    population.size.unexposed = total.population.size * (1 - insecticide.exposure)

  return(population.size.unexposed)
}
