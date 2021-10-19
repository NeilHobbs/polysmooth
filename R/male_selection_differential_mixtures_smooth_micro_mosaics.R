male_selection_differential_mixtures_smooth_micro_mosaics = function(trait.mean.1,
                                                                     trait.mean.2,
                                                                     standard.deviation,
                                                                     vector.length,
                                                                     female.exposure,
                                                                     male.exposure,
                                                                     insecticide.coverage,
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
                                                                 trait.mean = trait.mean.1, #value does not technically matter
                                                                 standard.deviation = standard.deviation)


  #Relative Frequency of each of Trait 2 of the Normal Distribution
  relative.frequency.trait.2 = calculate_density_of_trait_values(vector.length = vector.length,
                                                                 trait.mean = trait.mean.2, #value does not technically matter
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

  do.not.encounter = 1 - ((insecticide.coverage) * female.exposure*male.exposure)


  #Need to figure out a way to put in dispersal in each gonotrophic cycle???



temp.vec.1 = ((relative.frequency.trait.1*do.not.encounter) +
                               (relative.frequency.trait.1*female.exposure*male.exposure*insecticide.coverage*survival.probability.1*mean(survival.probability.2)))

temp.vec.2 = ((relative.frequency.trait.2*do.not.encounter) +
                    (relative.frequency.trait.2*female.exposure*male.exposure*insecticide.coverage*survival.probability.2*mean(survival.probability.1)))


    ##Tracking Trait 1
    update.density.1 = temp.vec.1

    pop.size.1= sum(update.density.1)

    update.mean.z.1 = (sum(normal.distribution.trait.1 * update.density.1))/ pop.size.1

    selection.diff.1 = update.mean.z.1 - trait.mean.1


    #Tracking Trait 2
    update.density.2 = temp.vec.2

    pop.size.2 = sum(update.density.2)

    update.mean.z.2 = (sum(normal.distribution.trait.2 * update.density.2))/ pop.size.2

    selection.diff.2 = update.mean.z.2 - trait.mean.2




  return(list(selection.diff.1, selection.diff.2))
}
