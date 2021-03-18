#'@title Calculate the relative number of mosquitoes dispersing from the intervention site to the refugia.
#'
#'@param dispersal.rate = The rate of mosquito exchange between the intervention site and the refugia.
#'@param intervention.coverage = The proportion of the total mosquito population that is covered by the intervention site.

number_migrating_intervention_to_refugia = function(dispersal.rate,
                                                    intervention.coverage){

  migration.intervention.to.refugia = dispersal.rate * intervention.coverage

return(migration.intervention.to.refugia)

  }
