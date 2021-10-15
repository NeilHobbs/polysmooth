#'@title Calculate the proportion of female mosquitoes who do not encounter either insecticide each gonotrophic cycle

#'@param insecticide.coverage.1 = The proportion of the total intervention coverage that is by the first insecticide
#'@param insecticide.coverage.2 = The proportion of the total intervention coverage that is by the second insecticide
#'@param female.exposure = The probability of a mosquito encountering an insecticide.


proportion_do_not_encounter_micro_mosaic = function(insecticide.coverage.1,
                                                    insecticide.coverage.2,
                                                    female.exposure){


  if(insecticide.coverage.1 + insecticide.coverage.2 != 1){
    stop("insecticide.coverage.1 and insecticide.coverage.2 should sum to 1")
  }

  proportion.do.not.encounter = 1 - ((insecticide.coverage.1 + insecticide.coverage.2) * female.exposure)

  return(proportion.do.not.encounter)

}
