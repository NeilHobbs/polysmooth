#'@title Calculate the initial insecticidal efficacy from the insecticide concentrations
#'
#'@param applied.insecticide.dose = The actual applied dosage of the insecticide deployed.
#'@param recommended.insecticide.dose = The manufacturer’s recommended dosage for the insecticide when deployed as a single insecticide formulation.


calculate_initial_efficacy_from_insecticide_concentration = function(applied.insecticide.dose,
                                                                     recommended.insecticide.dose){

  initial.insecticide.efficacy = applied.insecticide.dose / recommended.insecticide.dose

  return(initial.insecticide.efficacy)

}
