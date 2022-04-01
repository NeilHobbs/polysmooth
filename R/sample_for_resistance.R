#'@title
#'
#'@description In the the field mosquito bioassay survival is unlikely to be conducted perfectly as the model requires.
#' Therefore we need to account for how operational sampling/surveillance which is designed to inform IRM may impact
#' the operational decision making incorrectly due to imprecise estimates of the true population resistance level.
#'
#' @param number.mosquitoes = The number of mosquitoes used in the bioassays
#' @param trait.mean = The trait mean of the insecticide
#' @param standard.deviation = The standard deviation of the resistance trait.

sample_for_resistance = function(number.mosquitoes,
                                 trait.mean,
                                 standard.deviation,
                                 vector.length){



norm.dist = create_normal_distribution(vector.length = vector.length,
                                       trait.mean = trait.mean,
                                       standard.deviation = standard.deviation)

rel.freq = calculate_density_of_trait_values(vector.length = vector.length,
                                             trait.mean = trait.mean,
                                             standard.deviation = standard.deviation)

#convert to sampling probabilities
sample.probability = rel.freq / (sum(rel.freq))

#sample for mosquitoes with the given weighted probabiities.
mosquitoes.sampled.1 = sample(norm.dist, number.mosquitoes, replace = TRUE, prob = sample.probability)

survival.probability.1 = convert_resistance_score_to_bioassay_survival(trait.mean = mosquitoes.sampled.1,
                                                                       half.population.bioassay.survival.resistance = 900,
                                                                       michaelis.menten.slope = 1,
                                                                       maximum.bioassay.survival.proportion = 1)

#need to convert the survival probabilities to true/false in a living/dying sense
  #Therefore roll some dice to get some probabiltiies
dice.rolls = runif(number.mosquitoes, 0, 1)

  #Then if the survival probability is greater the dice roll the mosquito survives; otherwise it dies
survive = ifelse(dice.rolls <= survival.probability.1,
                 yes = 1, #live
                 no = 0)#die


measured.bioassay.survival = (sum(survive))/number.mosquitoes

return(measured.bioassay.survival)

}



sample_for_resistance(number.mosquitoes = 100,
                      trait.mean = 100,
                      standard.deviation = 50)










