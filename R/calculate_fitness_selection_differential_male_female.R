#'@title Calculate the fitness selection differential from separate male and female selection differential inputs
#'
#'@param male.fitness.selection.differential =The selection differential for males following from fitness costs.
#'@param female.fitness.selection.differential = The selection differential for female mosquitoes from fitness costs

calculate_fitness_selection_differential_male_female = function(male.fitness.selection.differential,
                                                                female.fitness.selection.differential){

  fitness.selection.differential = (male.fitness.selection.differential + female.fitness.selection.differential)/2

  return(fitness.selection.differential)

}
