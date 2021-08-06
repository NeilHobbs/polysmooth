#' @title Calculate the relative population size of the intervention site prior to selection.
#'
#'@param relative.contributions.before.selection = a vector of probability densities calculated from calculate_density_of_trait_values

get_total_population_size = function(relative.contributions.before.selection){

  total.population.size = sum(relative.contributions.before.selection, na.rm = TRUE)

  return(total.population.size)
}


