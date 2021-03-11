#'@title Calculate the selection differential for female mosquitoes following insecticide selection.
#'
#'@param female.trait.mean.after.selection = The mean Polygenic Resistance Score to the insecticide of the female mosquitoes after insecticide selection.
#'@param female.trait.mean = The mean Polygenic Resistance Score to the insecticide of female mosquitoes before selection has occurred.


calculate_female_insecticide_selection_differential = function(female.trait.mean.after.selection,
                                                               female.trait.mean){

female.insecticide.selection.differential = female.trait.mean.after.selection - female.trait.mean

  return(female.insecticide.selection.differential)

}
