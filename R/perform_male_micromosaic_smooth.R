#'@title Calculate the selection differentials for male mosquitoes when two insecticides are deployed as a micro-mosaic.
#'
#'@description This function calculates the selection differentials for male mosquitoes when there
#'are two insecticides deployed as a micromosaic. As such there is selection and population suppression
#' by two insecticides which may or may not have equal coverage. Selection differentials are
#' calculated using smooth selection.
#'
#' @param insecticide.coverage.1 = The proportion of the total coverage by insecticide 1. Note insecticide.coverage.1 and insecticide.coverage.2 must sum to 1.
#' @param insecticide.coverage.2 = The proportion of the total coverage by insecticide 2. Note insecticide.coverage.1 and insecticide.coverage.2 must sum to 1.
#' @param trait.mean.1 = The mean polygenic resistance score to the first insecticide, corresponding to insecticide.coverage.1 and current.insecticde.efficacy.1
#' @param trait.mean.2 = The mean polygenic resistance score to the second insecticide, corresponding to insecticide.coverage.2 and current.insecticde.efficacy.2
#' @param standard.deviation = The standard deviation of the polygenic resistance scores
#' @param vector.length = The lenght of the vector used to calcualte the Normal distribution.
#' @param female.exposure = THe proportion of female mosquitoes encountering the insecticides
#' @param male.exposure = The proportion of male mosqutioes encountering the insecticides as a proportion of the female exposure.
#' @param current.insecticide.efficacy.1 = The current efficacy of the first insecticide, corresponds to insecticide.coverage.1 and trait.mean.1
#' @param current.insecticide.efficacy.1 = The current efficacy of the second insecticide, corresponds to insecticide.coverage.2 and trait.mean.2
#' @param regression.coefficient = The regression coefficient between bioassay survival and field survival
#' @param regression.intercept = The regression intercept betweeen bioassay survival and field survival
#' @param half.population.bioassay.survival.resistance = THe polygenic resistance score that gives 50% bioassay survival
#' @param michaelis.menten.slope = THe slope of the michaelis menten equation; should be set at 1.
#' @param maximum.bioassay.survival.proportion = The maximum survival proportion in a mosquito bioassay, should be set at 1.

perform_male_micromosaic_smooth = function(insecticide.coverage.1,
                                           insecticide.coverage.2,
                                           trait.mean.1,
                                           trait.mean.2,
                                           standard.deviation,
                                           vector.length,
                                           female.exposure,
                                           male.exposure,
                                           current.insecticide.efficacy.1,
                                           current.insecticide.efficacy.2,
                                           regression.coefficient,
                                           regression.intercept,
                                           half.population.bioassay.survival.resistance,
                                           michaelis.menten.slope,
                                           maximum.bioassay.survival.proportion){

  #create the starting conditions for the first gonotrophic cycle
  #Values of the Normal Distrition of Trait 1 (insecticide 1)
  normal.distribution.trait.1 = create_normal_distribution(vector.length = vector.length,
                                                           trait.mean = trait.mean.1,
                                                           standard.deviation = standard.deviation)

  #Values of the Normal distribution of Trait 2 (insecticide 2)
  normal.distribution.trait.2 = create_normal_distribution(vector.length = vector.length,
                                                           trait.mean = trait.mean.2,
                                                           standard.deviation = standard.deviation)

  #Relative Frequency of each of Trait 1 of the Normal Distribution
  relative.frequency.trait.1 = calculate_density_of_trait_values(vector.length = vector.length,
                                                                 trait.mean = trait.mean.1,
                                                                 standard.deviation = standard.deviation)


  #Relative Frequency of each of Trait 2 of the Normal Distribution
  relative.frequency.trait.2 = calculate_density_of_trait_values(vector.length = vector.length,
                                                                 trait.mean = trait.mean.2,
                                                                 standard.deviation = standard.deviation)


  survival.probability.1 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.1,
                                                                                                                                         half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                         michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                         maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                       regression.coefficient = regression.coefficient,
                                                                       regression.intercept = regression.intercept,
                                                                       current.insecticide.efficacy = current.insecticide.efficacy.1)

  survival.probability.2 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.2,
                                                                                                                                         half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                         michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                         maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                       regression.coefficient = regression.coefficient,
                                                                       regression.intercept = regression.intercept,
                                                                       current.insecticide.efficacy = current.insecticide.efficacy.2)


  do.not.encounter = proportion_do_not_encounter_micro_mosaic_males(insecticide.coverage.1 = insecticide.coverage.1,
                                                                    insecticide.coverage.2 = insecticide.coverage.2,
                                                                    female.exposure = female.exposure,
                                                                    male.exposure = male.exposure)



  temp.vec.1 = ((relative.frequency.trait.1*do.not.encounter) +
                  (relative.frequency.trait.1*female.exposure*male.exposure*insecticide.coverage.2*mean(survival.probability.2)) +
                  (relative.frequency.trait.1*female.exposure*male.exposure*insecticide.coverage.1*survival.probability.1))

  temp.vec.2 = ((relative.frequency.trait.2*do.not.encounter) +
                  (relative.frequency.trait.2*female.exposure*male.exposure*insecticide.coverage.2*survival.probability.2) +
                  (relative.frequency.trait.2*female.exposure*male.exposure*insecticide.coverage.1*mean(survival.probability.1)))



    ##Tracking Trait 1
    update.density.1 = temp.vec.1

    pop.size.1 = sum(update.density.1)

    update.mean.z.1 = (sum(normal.distribution.trait.1 * update.density.1))/ pop.size.1

    selection.diff.1 = update.mean.z.1 - trait.mean.1



    #Tracking Trait 2
    update.density.2 = temp.vec.2

    pop.size.2 = sum(update.density.2)

    update.mean.z.2= (sum(normal.distribution.trait.2 * update.density.2))/ pop.size.2

    selection.diff.2= update.mean.z.2 - trait.mean.2


  return(list(selection.diff.1, selection.diff.2))
}

# perform_micromosaic_smooth(max.cycles = 6,
#                     insecticide.coverage.1 = 0,
#                     insecticide.coverage.2 = 1,
#                     trait.mean.1 = 0,
#                     trait.mean.2 = 0,
#                     standard.deviation = 30,
#                     vector.length = 1000000,
#                     female.exposure = 1,
#                     male.selection.diff.1 = 0,
#                     male.selection.diff.2 = 0,
#                     current.insecticide.efficacy.1 = 0,
#                     current.insecticide.efficacy.2 = 1,
#                     regression.coefficient = 0.48,
#                     regression.intercept = 0.15,
#                     heritability = 0.3,
#                     exposure.scaling.factor = 20,
#                     half.population.bioassay.survival.resistance = 900,
#                     michaelis.menten.slope = 1,
#                     maximum.bioassay.survival.proportion = 1)
#
#
#
# n.cycles = seq(1, 20, 1)
# response.val = c()
# for(g in 1:20){
#   response.val[g] = perform_micromosaic_smooth(max.cycles = n.cycles[g],
#                                     insecticide.coverage.1 = 0.5,
#                                     insecticide.coverage.2 = 0.5,
#                                     trait.mean.1 = 0,
#                                     trait.mean.2 = 900,
#                                     standard.deviation = 30,
#                                     vector.length = 10000,
#                                     female.exposure = 1,
#                                     male.selection.diff.1 = 0,
#                                     male.selection.diff.2 = 0,
#                                     current.insecticide.efficacy.1 = 1,
#                                     current.insecticide.efficacy.2 = 1,
#                                     regression.coefficient = 0.48,
#                                     regression.intercept = 0.15,
#                                     heritability = 0.3,
#                                     half.population.bioassay.survival.resistance = 900,
#                                     michaelis.menten.slope = 1,
#                                     maximum.bioassay.survival.proportion = 1,
#                                     exposure.scaling.factor = 20)[[1]]
# }
#
# plot(n.cycles, response.val)
