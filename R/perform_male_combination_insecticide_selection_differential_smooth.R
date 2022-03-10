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

perform_male_combination_insecticide_selection_differential_smooth = function(coverage,
                                                                              coverage.i,
                                                                              coverage.j,
                                                                              coverage.ij,
                                                                              probability.only.i,
                                                                              probability.only.j,
                                                                              probability.both.i.j,
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
  intervention.normal.distribution.i = create_normal_distribution(vector.length = vector.length,
                                                           trait.mean = trait.mean.1,
                                                           standard.deviation = standard.deviation)

  #Values of the Normal distribution of Trait 2 (insecticide 2)
  intervention.normal.distribution.j = create_normal_distribution(vector.length = vector.length,
                                                           trait.mean = trait.mean.2,
                                                           standard.deviation = standard.deviation)

  #Relative Frequency of each of Trait 1 of the Normal Distribution
  initial.intervention.densities.i = calculate_density_of_trait_values(vector.length = vector.length,
                                                                 trait.mean = trait.mean.1,
                                                                 standard.deviation = standard.deviation)


  #Relative Frequency of each of Trait 2 of the Normal Distribution
  initial.intervention.densities.j = calculate_density_of_trait_values(vector.length = vector.length,
                                                                 trait.mean = trait.mean.2,
                                                                 standard.deviation = standard.deviation)


  #Create a vector of the field survival to the first insecticide dependent on the insecticide efficacy
  survival.probability.int.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.normal.distribution.i,
                                                                                                                                         half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                         michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                         maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                       regression.coefficient = regression.coefficient,
                                                                       regression.intercept = regression.intercept,
                                                                       current.insecticide.efficacy = current.insecticide.efficacy.1)

  #Create a vector of the field survival to the second insecticide dependent on the insecticide efficact
  survival.probability.int.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.normal.distribution.j,
                                                                                                                                         half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                         michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                         maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                       regression.coefficient = regression.coefficient,
                                                                       regression.intercept = regression.intercept,
                                                                       current.insecticide.efficacy = current.insecticide.efficacy.2)

  mean.survival.int.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = trait.mean.1,
                                                                                                                                      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                      michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                      maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                    regression.coefficient = regression.coefficient,
                                                                    regression.intercept = regression.intercept,
                                                                    current.insecticide.efficacy = current.insecticide.efficacy.1)

  mean.survival.int.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = trait.mean.2,
                                                                                                                                      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                      michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                      maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                    regression.coefficient = regression.coefficient,
                                                                    regression.intercept = regression.intercept,
                                                                    current.insecticide.efficacy = current.insecticide.efficacy.2)




  update.frequencies.i  = (initial.intervention.densities.i * female.exposure * survival.probability.int.i * coverage.i * coverage) +
    (initial.intervention.densities.i * (female.exposure*male.exposure) * coverage.j * mean.survival.int.j * coverage) +
    (initial.intervention.densities.i * (female.exposure*male.exposure) * coverage.ij  * probability.only.i * survival.probability.int.i * coverage) + #i only
    (initial.intervention.densities.i * (female.exposure*male.exposure) * coverage.ij  * probability.only.j * mean.survival.int.j * coverage) + #j only
    (initial.intervention.densities.i * (female.exposure*male.exposure) * coverage.ij  * probability.both.i.j * mean.survival.int.j * survival.probability.int.i * coverage) + #both ij
    (initial.intervention.densities.i * (1-(female.exposure*male.exposure)) * coverage)



  total.males.surviving.i  = sum(update.frequencies.i)


  male.differential.i  = ((sum(update.frequencies.i *intervention.normal.distribution.i) / total.males.surviving.i ) - trait.mean.1)



  update.frequencies.j  = (initial.intervention.densities.j * (female.exposure*male.exposure) * survival.probability.int.j* coverage.j * coverage) +
    (initial.intervention.densities.j * (female.exposure*male.exposure) * coverage.i * mean.survival.int.i * coverage) +
    (initial.intervention.densities.j * (female.exposure*male.exposure) * coverage.ij  * probability.only.j * survival.probability.int.j * coverage) + #i only
    (initial.intervention.densities.j * (female.exposure*male.exposure) * coverage.ij  * probability.only.i * mean.survival.int.i * coverage) + #j only
    (initial.intervention.densities.j * (female.exposure*male.exposure) * coverage.ij  * probability.both.i.j * mean.survival.int.i * survival.probability.int.j * coverage) + #both ij
    (initial.intervention.densities.j * (1-(female.exposure*male.exposure)) * coverage)



  total.males.surviving.j  = sum(update.frequencies.j)


  male.differential.j  = ((sum(update.frequencies.j *intervention.normal.distribution.j) / total.males.surviving.j ) - trait.mean.2)

  return(list(male.differential.i, male.differential.j))
}

