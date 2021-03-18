#'@title Calculate the selection differential as a result of fitness costs.
#'
#'@param trait.mean.after.fitness = The mean Polygenic Resistance Score after fitness costs are applied to the population.
#'@param trait.mean = The mean value of a polygenic trait in a population.

calculate_fitness_selection_differential = function(trait.mean.after.fitness,
                                                    trait.mean){

  fitness.selection.differential = trait.mean.after.fitness - trait.mean

  return(fitness.selection.differential)
}
