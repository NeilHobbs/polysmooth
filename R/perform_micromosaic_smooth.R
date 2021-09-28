perform_micromosaic_smooth = function(max.cycles,
                                      insecticide.coverage.1,
                                      insecticide.coverage.2,
                                      trait.mean.1,
                                      trait.mean.2,
                                      standard.deviation,
                                      vector.length,
                                      female.exposure,
                                      male.selection.diff.1,
                                      male.selection.diff.2,
                                      current.insecticide.efficacy.1,
                                      current.insecticide.efficacy.2,
                                      regression.coefficient,
                                      regression.intercept,
                                      heritability,
                                      exposure.scaling.factor){

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


  survival.probability.1 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.1),
                                                                       regression.coefficient = regression.coefficient,
                                                                       regression.intercept = regression.intercept,
                                                                       current.insecticide.efficacy = current.insecticide.efficacy.1)

  survival.probability.2 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.2),
                                                                       regression.coefficient = regression.coefficient,
                                                                       regression.intercept = regression.intercept,
                                                                       current.insecticide.efficacy = current.insecticide.efficacy.2)

  do.not.encounter = 1 - ((insecticide.coverage.1 + insecticide.coverage.2) * female.exposure)

  update.density.1 = list()
  update.mean.z.1 = list()
  pop.size.1 = list()
  selection.diff.1 = list()
  response.1 = list()
  update.density.2 = list()
  update.mean.z.2 = list()
  selection.diff.2 = list()
  response.2 = list()
  pop.size.2 = list()

  #Need to figure out a way to put in dispersal in each gonotrophic cycle???

  for(i in 1:max.cycles){

    if(i == 1){temp.vec.1 = ((relative.frequency.trait.1*do.not.encounter) +
                               (relative.frequency.trait.1*female.exposure*insecticide.coverage.2*mean(survival.probability.2)) +
                               (relative.frequency.trait.1*female.exposure*insecticide.coverage.1*survival.probability.1))


    temp.vec.2 = ((relative.frequency.trait.2*do.not.encounter) +
                    (relative.frequency.trait.2*female.exposure*insecticide.coverage.2*survival.probability.2) +
                    (relative.frequency.trait.2*female.exposure*insecticide.coverage.1*mean(survival.probability.1)))
    }

    if(i != 1){temp.vec.1 = ((update.density.1[[i-1]]*do.not.encounter) +
                               (update.density.1[[i-1]]*female.exposure*insecticide.coverage.2*mean(survival.probability.2)) +
                               (update.density.1[[i-1]]*insecticide.coverage.1*female.exposure*survival.probability.1))


    temp.vec.2 = ((update.density.2[[i-1]]*do.not.encounter)+
                    (update.density.2[[i-1]]*insecticide.coverage.1*female.exposure*mean(survival.probability.1))+
                    (update.density.2[[i-1]]*female.exposure*insecticide.coverage.2*survival.probability.2))

    }

    ##Tracking Trait 1
    update.density.1[[i]] = temp.vec.1

    pop.size.1[[i]] = sum(update.density.1[[i]])

    update.mean.z.1[[i]] = (sum(normal.distribution.trait.1 * update.density.1[[i]]))/ pop.size.1[[i]]

    selection.diff.1[[i]] = update.mean.z.1[[i]] - trait.mean.1

    response.1[[i]] = heritability * exposure.scaling.factor * ((selection.diff.1[[i]] + male.selection.diff.1) / 2)

    #Tracking Trait 2
    update.density.2[[i]] = temp.vec.2

    pop.size.2[[i]] = sum(update.density.2[[i]])

    update.mean.z.2[[i]] = (sum(normal.distribution.trait.2 * update.density.2[[i]]))/ pop.size.2[[i]]

    selection.diff.2[[i]] = update.mean.z.2[[i]] - trait.mean.2

    response.2[[i]] = heritability * exposure.scaling.factor * ((selection.diff.2[[i]] + male.selection.diff.2) / 2)

  }

  total.oviposition.1 = sum(unlist(pop.size.1))
  overall.response.1 = sum(unlist(response.1)*(unlist(pop.size.1)/total.oviposition.1))

  total.oviposition.2 = sum(unlist(pop.size.2))
  overall.response.2 = sum(unlist(response.2)*(unlist(pop.size.2)/total.oviposition.2))


  return(list(overall.response.1, overall.response.2))
}

perform_micromosaic_smooth(max.cycles = 6,
                    insecticide.coverage.1 = 0,
                    insecticide.coverage.2 = 1,
                    trait.mean.1 = 0,
                    trait.mean.2 = 0,
                    standard.deviation = 20,
                    vector.length = 1000000,
                    female.exposure = 1,
                    male.selection.diff.1 = 0,
                    male.selection.diff.2 = 0,
                    current.insecticide.efficacy.1 = 0,
                    current.insecticide.efficacy.2 = 1,
                    regression.coefficient = 0.48,
                    regression.intercept = 0.15,
                    heritability = 0.3,
                    exposure.scaling.factor = 10)



n.cycles = seq(1, 20, 1)
response.val = c()
for(g in 1:20){
  response.val[g] = perform_micromosaic_smooth(max.cycles = n.cycles[g],
                                    insecticide.coverage.1 = 0.5,
                                    insecticide.coverage.2 = 0.5,
                                    trait.mean.1 = 0,
                                    trait.mean.2 = 900,
                                    standard.deviation = 20,
                                    vector.length = 10000,
                                    female.exposure = 1,
                                    male.selection.diff.1 = 0,
                                    male.selection.diff.2 = 0,
                                    current.insecticide.efficacy.1 = 1,
                                    current.insecticide.efficacy.2 = 1,
                                    regression.coefficient = 0.48,
                                    regression.intercept = 0.15,
                                    heritability = 0.3,
                                    exposure.scaling.factor = 10)[[1]]
}

plot(n.cycles, response.val)
