#'@title Calculate the mean Polygenic Resistance Score of the mosquito population in the intervention site after mosquito migration.
#'
#'@param intervention.after.selection = The mean Polygenic Resistance Score of the mosquito population in the intervention site after insecticide and fitness cost based selection.
#'@param refugia.after.selection = The mean Polygenic Resistance Score of the mosquito population in the refugia site after  fitness cost based selection.
#'@param migration.intervention.to.refugia = The relative number of mosquitoes dispersing from the intervention site to the refugia.
#'@param migration.refugia.to.intervention = The relative number of mosquitoes dispersing from the refugia to the intervention site.
#'@param insecticide.population.suppression = The impact of insecticides on the relative population size of female mosquitoes the intervention site, who are now ready to lay eggs.

calculate_intervention_site_after_migration_deployed = function(intervention.after.selection,
                                                                refugia.after.selection,
                                                                migration.intervention.to.refugia,
                                                                migration.refugia.to.intervention,
                                                                insecticide.population.suppression){

  contribution.stay.in.intervention = intervention.after.selection *(1 - migration.intervention.to.refugia)*(1-insecticide.population.suppression)

  contribution.joining.from.refugia = refugia.after.selection * migration.refugia.to.intervention

  population.weighting = ((1 - migration.intervention.to.refugia)*(1-insecticide.population.suppression)) + migration.refugia.to.intervention


  intervention.after.migration = (contribution.stay.in.intervention + contribution.joining.from.refugia) / population.weighting

  #Prevent NaN or NA
  intervention.after.migration = ifelse(is.na(intervention.after.migration),
                                        yes = 0,
                                        no = intervention.after.migration)

  #Prevent below zero
  intervention.after.migration = ifelse(intervention.after.migration < 0,
                                        yes = 0,
                                        no = intervention.after.migration)

 return(intervention.after.migration)
}
