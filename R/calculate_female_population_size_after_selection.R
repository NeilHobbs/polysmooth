#'@title Calculate the relative population size of all the female mosquitoes in the intervention site after insecticide selection.
#'
#'@param female.population.size.unexposed = The relative population size of the unexposed female mosquitoes in the intervention site.
#'@param female.population.size.exposed.survivors = The relative population size of female mosquitoes that have encountered the insecticide and survived.


calculate_female_population_size_after_selection = function(female.population.size.unexposed,
                                                 female.population.size.exposed.survivors){


  female.population.size.unexposed = ifelse(is.na(female.population.size.unexposed),
                                            yes = 0,
                                            no = female.population.size.unexposed)


  female.population.size.exposed.survivors = ifelse(is.na(female.population.size.exposed.survivors),
                                                    yes = 0,
                                                    no = female.population.size.exposed.survivors)


  female.population.size.after.selection = female.population.size.unexposed + female.population.size.exposed.survivors

  return(female.population.size.after.selection)
}
