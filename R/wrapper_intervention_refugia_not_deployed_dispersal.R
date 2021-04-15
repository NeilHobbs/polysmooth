#'@title Wrapper to allow mosquito dispersal tracking in the intervention site when the insecticide is not deployed
#'
#'@param heritabililty = The heritability of a polygenic trait.
#'@param intervention.before.selection = The mean Polygenic Resistance Score of the mosquito population in the intervention before selection has occurred that generation.
#'@param female.fitness.cost = The fixed fitness cost associated with polygenic resistance for females.
#'@param male.fitness.cost = The fixed fitness costs associated with polygenic resistance for males
#'@param refugia.before.selection = The mean Polygenic Resistance Score of the mosquito population in the refugia  before selection has occurred that generation.
#'@param dispersal.rate = The rate of mosquito exchange between the intervention site and the refugia.
#'@param intervention.coverage = The proportion of the total mosquito population that is covered by the intervention site.



wrapper_intervention_refugia_not_deployed_dispersal = function(insecticide.population.suppression,
                                                               intervention.before.selection,
                                                               female.fitness.cost,
                                                               male.fitness.cost,
                                                               heritability,
                                                               refugia.before.selection,
                                                               dispersal.rate,
                                                               intervention.coverage){


  intervention.after.selection = wrapper_intervention_site_after_selection_not_deployed(heritability = heritability,
                                                                                        intervention.before.selection = intervention.before.selection,
                                                                                        female.fitness.cost = female.fitness.cost,
                                                                                        male.fitness.cost = male.fitness.cost)

  refugia.after.selection = wrapper_refugia_breeders_equation(refugia.before.selection = refugia.before.selection,
                                                              heritability = heritability,
                                                              female.fitness.cost = female.fitness.cost,
                                                              male.fitness.cost = male.fitness.cost)

  staying.in.intervention = 1 - number_migrating_intervention_to_refugia(dispersal.rate = dispersal.rate,
                                                                         intervention.coverage = intervention.coverage)

  joining.from.intervetion = number_migrating_intervention_to_refugia(dispersal.rate = dispersal.rate,
                                                                      intervention.coverage = intervention.coverage)

  joining.from.refugia =  number_migrating_refugia_to_intervention(dispersal.rate = dispersal.rate,
                                                                   intervention.coverage = intervention.coverage)

  staying.in.refugia = 1 - number_migrating_refugia_to_intervention(dispersal.rate = dispersal.rate,
                                                                    intervention.coverage = intervention.coverage)


  intervention.after.migration = intervention_after_migration(intervention.after.selection = intervention.after.selection,
                                                              staying.in.intervention = staying.in.intervention,
                                                              insecticide.population.suppression = insecticide.population.suppression,
                                                              joining.from.refugia = joining.from.refugia,
                                                              refugia.after.selection = refugia.after.selection)


  refugia.after.migration = refugia_after_migration(intervention.after.selection = intervention.after.selection,
                                                    joining.from.intervetion = joining.from.intervetion,
                                                    insecticide.population.suppression = insecticide.population.suppression,
                                                    refugia.after.selection = refugia.after.selection,
                                                    staying.in.refugia = staying.in.refugia)


  #prevent from going below zero
  refugia.after.migration = ifelse(refugia.after.migration < 0,
                                   yes = 0,
                                   no = refugia.after.migration)

  intervention.after.migration = ifelse(intervention.after.migration < 0,
                                        yes = 0,
                                        no = intervention.after.migration)


  return(list(intervention.after.migration, refugia.after.migration))
}
