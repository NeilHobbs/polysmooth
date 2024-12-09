#'@title Calculate the mean Polygenic Resistance Score of the mosquito population in the refugia site after mosquito migration.
#'
#'@param intervention.after.selection = The mean Polygenic Resistance Score of the mosquito population in the intervention site after insecticide and fitness cost based selection.
#'@param refugia.after.selection = The mean Polygenic Resistance Score of the mosquito population in the refugia site after  fitness cost based selection.
#'@param migration.intervention.to.refugia = The relative number of mosquitoes dispersing from the intervention site to the refugia.
#'@param migration.refugia.to.intervention = The relative number of mosquitoes dispersing from the refugia to the intervention site.
#'@param insecticide.population.suppression = The impact of insecticides on the relative population size of female mosquitoes the intervention site, who are now ready to lay eggs.


calculate_refugia_after_migration_not_deployed = function(intervention.after.selection,
                                                          refugia.after.selection,
                                                          migration.intervention.to.refugia,
                                                          migration.refugia.to.intervention,
                                                          insecticide.population.suppression){

  contribution.joining.from.intervention = intervention.after.selection *migration.intervention.to.refugia*(1-insecticide.population.suppression)
  contribution.stay.in.refugia = refugia.after.selection * (1 - migration.refugia.to.intervention)
  population.weighting = (1 - migration.refugia.to.intervention) + ((1-insecticide.population.suppression)*migration.intervention.to.refugia)



  refugia.after.migration = (contribution.joining.from.intervention + contribution.stay.in.refugia) / population.weighting

  #prevent NaN
  refugia.after.migration = ifelse(is.na(refugia.after.migration),
                                   yes = 0,
                                   no = refugia.after.migration)
  #prevent falling below zero
  refugia.after.migration = ifelse(refugia.after.migration < 0,
                                   yes = 0,
                                   no = refugia.after.migration)


  return(refugia.after.migration)


}
