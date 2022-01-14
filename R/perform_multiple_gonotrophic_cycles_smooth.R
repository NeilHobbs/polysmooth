#This is a chlunky piece of code.

#Need to break it down into some component modules.
#Plus add in error/warning messages.


perform_multiple_gonotrophic_cycles_smooth = function(max.cycles,
                                                      trait.mean.1,
                                                      standard.deviation,
                                                      vector.length,
                                                      female.exposure,
                                                      male.selection.diff.1,
                                                      current.insecticide.efficacy.1,
                                                      regression.coefficient,
                                                      regression.intercept,
                                                      heritability.trait.1,
                                                      exposure.scaling.factor,
                                                      half.population.bioassay.survival.resistance,
                                                      michaelis.menten.slope,
                                                      maximum.bioassay.survival.proportion,
                                                      female.natural.survival.probability){

  #create the starting conditions for the first gonotrophic cycle
  #Values of the Normal Distrition of Trait 1 (insecticide 1)
  normal.distribution.trait.1 = create_normal_distribution(vector.length = vector.length,
                                                           trait.mean = trait.mean.1,
                                                           standard.deviation = standard.deviation)



  #Relative Frequency of each of Trait 1 of the Normal Distribution
  relative.frequency.trait.1 = calculate_density_of_trait_values(vector.length = vector.length,
                                                                 trait.mean = trait.mean.1, #value does not technically matter
                                                                 standard.deviation = standard.deviation)



  survival.probability.1 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.1,
                                                                                                                                         half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                         michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                         maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                       regression.coefficient = regression.coefficient,
                                                                       regression.intercept = regression.intercept,
                                                                       current.insecticide.efficacy = current.insecticide.efficacy.1)



  do.not.encounter = 1-female.exposure

  update.density.1 = list()
  update.mean.z.1 = list()
  pop.size.1 = list()
  selection.diff.1 = list()
  response.1 = list()


  #Need to figure out a way to put in dispersal in each gonotrophic cycle???

  for(i in 1:max.cycles){

    if(i == 1){temp.vec.1 = ((relative.frequency.trait.1*do.not.encounter * female.natural.survival.probability) +
                               (relative.frequency.trait.1*female.exposure* ifelse(survival.probability.1>female.natural.survival.probability,
                                                                                   yes = female.natural.survival.probability,
                                                                                   no = survival.probability.1)))





    }

    if(i != 1){temp.vec.1 = ((update.density.1[[i-1]]*do.not.encounter * female.natural.survival.probability) +
                               (update.density.1[[i-1]]*female.exposure*ifelse(survival.probability.1>female.natural.survival.probability,
                                                                               yes = female.natural.survival.probability,
                                                                               no = survival.probability.1)))



    }

    ##Tracking Trait 1
    update.density.1[[i]] = temp.vec.1

    pop.size.1[[i]] = sum(update.density.1[[i]])

    update.mean.z.1[[i]] = (sum(normal.distribution.trait.1 * update.density.1[[i]]))/ pop.size.1[[i]]

    selection.diff.1[[i]] = update.mean.z.1[[i]] - trait.mean.1



    #Track Responses
    response.1[[i]] = heritability.trait.1 * exposure.scaling.factor * ((selection.diff.1[[i]] + male.selection.diff.1) / 2)


  }


  #Create separate module function.
  total.oviposition.1 = sum(unlist(pop.size.1))
  overall.response.1 = sum(unlist(response.1)*(unlist(pop.size.1)/total.oviposition.1))



  return(overall.response.1)
}
