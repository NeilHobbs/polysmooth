#'@title Calculate the relative population size of the mosquitoes in the intervention site who did not encounter the insecticide
#'
#'@param relative.contributions.unexposed = A vector containing the relative population contributions to the population of mosquitoes with a Polygenic Resistance Scores that did not encounter the insecticide in the intervention site.


get_population_size_unexposed = function(relative.contributions.unexposed){

  population.size.unexposed = sum(relative.contribution.unexposed)

  return(population.size.unexposed)
}
