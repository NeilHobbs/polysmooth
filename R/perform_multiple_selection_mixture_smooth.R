#'@title Allow multiple rounds of selection for females.

perform_multiple_selection_mixture_smooth = function(max.cycles,
                                                     trait.mean.1,
                                                     trait.mean.2,
                                                     standard.deviation,
                                                     insecticide.coverage,
                                                     vector.length,
                                                     female.exposure,
                                                     male.selection.diff.1,
                                                     male.selection.diff.2,
                                                     current.insecticide.efficacy.1,
                                                     current.insecticide.efficacy.2,
                                                     regression.coefficient,
                                                     regression.intercept,
                                                     heritability.1,
                                                     heritability.2,
                                                     exposure.scaling.factor,
                                                     half.population.bioassay.survival.resistance,
                                                     michaelis.menten.slope,
                                                     maximum.bioassay.survival.proportion,
                                                     cross.selection){

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



  #Create vectors of the field survival probability for each of the trait values (first insecticide) given the current insecticide efficacy
  survival.probability.1 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.1,
                                                                                                                                         half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                         michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                         maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                       regression.coefficient = regression.coefficient,
                                                                       regression.intercept = regression.intercept,
                                                                       current.insecticide.efficacy = current.insecticide.efficacy.1)

  #Create vectors of the field survival probability for each of the trait values (second insecticide) given the current insecticide efficacy
  survival.probability.2 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.2,
                                                                                                                                         half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                         michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                         maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                       regression.coefficient = regression.coefficient,
                                                                       regression.intercept = regression.intercept,
                                                                       current.insecticide.efficacy = current.insecticide.efficacy.2)

  #Calculate the proportion of females who escape insecticide exposure each gonotrophic cycle
  do.not.encounter = 1 - ((insecticide.coverage) * female.exposure)

  #Create empty lists to hold the vectors obtained during each gonotrophic cycle:
    #First Insecticide:::
  update.density.1 = list()
  update.mean.z.1 = list()
  pop.size.1 = list()
  selection.diff.1 = list()
  response.1 = list()

    #Second Insecticide:::
  update.density.2 = list()
  update.mean.z.2 = list()
  selection.diff.2 = list()
  response.2 = list()
  pop.size.2 = list()

  #Need to figure out a way to put in dispersal in each gonotrophic cycle??? - avoid doing; painfully complicated...

  #Loop over the number of gonotrophic cycles
  for(i in 1:max.cycles){

    #First gonotrophic cycle is different
    if(i == 1){temp.vec.1 = ((relative.frequency.trait.1*do.not.encounter) +
                               (relative.frequency.trait.1*female.exposure*insecticide.coverage*survival.probability.1*mean(survival.probability.2)))


    temp.vec.2 = ((relative.frequency.trait.2*do.not.encounter) +
                    (relative.frequency.trait.2*female.exposure*insecticide.coverage*survival.probability.2*mean(survival.probability.1)))
    }

    if(i != 1){temp.vec.1 = ((update.density.1[[i-1]]*do.not.encounter) +
                               (update.density.1[[i-1]]*insecticide.coverage*female.exposure*survival.probability.1*mean(survival.probability.2)))


    temp.vec.2 = ((update.density.2[[i-1]]*do.not.encounter)+
                    (update.density.2[[i-1]]*female.exposure*insecticide.coverage*survival.probability.2*mean(survival.probability.1)))

    }

    ##Tracking Trait 1
    update.density.1[[i]] = temp.vec.1

    pop.size.1[[i]] = sum(update.density.1[[i]])

    update.mean.z.1[[i]] = (sum(normal.distribution.trait.1 * update.density.1[[i]]))/ pop.size.1[[i]]

    selection.diff.1[[i]] = update.mean.z.1[[i]] - trait.mean.1



    #Tracking Trait 2
    update.density.2[[i]] = temp.vec.2

    pop.size.2[[i]] = sum(update.density.2[[i]])

    update.mean.z.2[[i]] = (sum(normal.distribution.trait.2 * update.density.2[[i]]))/ pop.size.2[[i]]

    selection.diff.2[[i]] = update.mean.z.2[[i]] - trait.mean.2


    #Track Responses
    response.1[[i]] = heritability.1 * exposure.scaling.factor * ((selection.diff.1[[i]] + male.selection.diff.1) / 2) +
            (cross.selection * (heritability.2 * exposure.scaling.factor * ((selection.diff.2[[i]] + male.selection.diff.2) / 2)))

    response.2[[i]] = heritability.2 * exposure.scaling.factor * ((selection.diff.2[[i]] + male.selection.diff.2) / 2) +
      (cross.selection * (heritability.1 * exposure.scaling.factor * ((selection.diff.1[[i]] + male.selection.diff.1) / 2)))
  }

  total.oviposition.1 = sum(unlist(pop.size.1))
  overall.response.1 = sum(unlist(response.1)*(unlist(pop.size.1)/total.oviposition.1))

  total.oviposition.2 = sum(unlist(pop.size.2))
  overall.response.2 = sum(unlist(response.2)*(unlist(pop.size.2)/total.oviposition.2))


  return(list(overall.response.1, overall.response.2))
}
