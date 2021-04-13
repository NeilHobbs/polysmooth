#'@title Calculate the mean Polygenic Resistance Score of the mosquito population in the refugia site after  fitness cost based selection.
#'
#'@param refugia.before.selection = The mean Polygenic Resistance Score of the mosquito population in the refugia  before selection has occurred that generation.
#'@param response.fitness = The response to selection from fitness costs only, such as would occur when the insecticide is either not deployed in the intervention site or the selection that occurs in the refugia.


calculate_refugia_after_selection_not_deployed = function(refugia.before.selection,
                                                          response.fitness){


  refugia.after.selection =  refugia.before.selection + response.fitness

  refugia.after.selection = ifelse(refugia.after.selection < 0,
                                   yes = 0,
                                   no = refugia.after.selection)

  return(refugia.after.selection)
}
