#'@title Calculate Natural Survival Betweeen Gonotrophic Cycles
#'
#'@description Calculate the proportion proportion of female mosquitoes
#' surviving to the next gonotrophic cycle (having successfully completed the
#' previous gonotrophic cycle). This will be dependent on the natural daily survival
#' which is not impacted by insecticides, and the gonotrophic cycle lenght.
#'
#'@param gonotrophic.cycle.length = The length of gonotrophic cycle in days.
#'@param natural.daily.survival = The daily survival probability excluding insecticide induced mortality
#'
#'@return proportion.surviving = the proportion of female mosquitoes at the end of one gonotrophic cycle starting the next gonotrophic cycle
between_gonotrophic_cycle_survival = function(gonotrophic.cycle.length,
                                              natural.daily.survival){


  proportion.surviving = natural.daily.survival ^ gonotrophic.cycle.length


  return(proportion.surviving)

}
