#'@title Calculate the relative number of mosquitoes dispersing from the refugia to the intervention site.
#'
#'@param dispersal.rate = The rate of mosquito exchange between the intervention site and the refugia.
#'@param intervention.coverage = The proportion of the total mosquito population that is covered by the intervention site.

number_migrating_refugia_to_intervention = function(dispersal.rate,
                                                    intervention.coverage){

  migration.refugia.to.intervention =  dispersal.rate * (1 - intervention.coverage)

  return(migration.refugia.to.intervention)
}
