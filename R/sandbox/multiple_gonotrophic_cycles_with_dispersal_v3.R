#Extending to have 2 insecticides deployed at the same time in a micromosaic and allowing for dispersal with a refugia.

multiple_gonotrophic_cycles_with_dispersal_v3 = function(coverage = 0.8,
                                                         insecticide.coverage.1 = 0.5,
                                                         insecticide.coverage.2 = 0.5,
                                                         natural.survival = 0.8,
                                                         dispersal.rate = 0.2,
                                                         female.exposure = 0.7,
                                                         refugia.trait.mean.1 = 0,
                                                         intervention.trait.mean.1 = 10,
                                                         refugia.trait.mean.2 = 0,
                                                         intervention.trait.mean.2 = 0,
                                                         heritability = 0.3,
                                                         n.cycles = 5,
                                                         male.selection.differential.intervention.1 = 0,
                                                         male.selection.differential.refugia.1 = 0,
                                                         male.selection.differential.intervention.2 = 0,
                                                         male.selection.differential.refugia.2 = 0,
                                                         exposure.scaling.factor = 10,
                                                         vector.length = 1000,
                                                         standard.deviation = 30,
                                                         half.population.bioassay.survival.resistance = 900,
                                                         michaelis.menten.slope = 1,
                                                         maximum.bioassay.survival.proportion = 1,
                                                         regression.coefficient = 0.48,
                                                         regression.intercept = 0.15,
                                                         current.insecticide.efficacy.1 = 1,
                                                         current.insecticide.efficacy.2 = 1){


  normal.distribution.trait.1.intervention = create_normal_distribution(vector.length = vector.length,
                                                                        trait.mean = intervention.trait.mean.1,
                                                                        standard.deviation = standard.deviation)

  normal.distribution.trait.1.refugia = create_normal_distribution(vector.length = vector.length,
                                                                   trait.mean = refugia.trait.mean.1,
                                                                   standard.deviation = standard.deviation)



  #Create vectors of start relative frequencies of the traits. Weighted by the relative
  #size of the intervention site and refugia.

  relative.frequency.trait.1.intervention = calculate_density_of_trait_values(vector.length = vector.length,
                                                                              trait.mean = intervention.trait.mean.1,
                                                                              standard.deviation = standard.deviation)* (coverage)

  relative.frequency.trait.1.refugia = calculate_density_of_trait_values(vector.length = vector.length,
                                                                         trait.mean = refugia.trait.mean.1,
                                                                         standard.deviation = standard.deviation)* (1-coverage)


  #Create vectors of survival probabilities
  survival.probability.1.a = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.1.intervention,
                                                                                                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                           michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                           maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                         regression.coefficient = regression.coefficient,
                                                                         regression.intercept = regression.intercept,
                                                                         current.insecticide.efficacy = current.insecticide.efficacy.1)
  #if traits from refugia.
  survival.probability.1.b = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.1.refugia,
                                                                                                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                           michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                           maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                         regression.coefficient = regression.coefficient,
                                                                         regression.intercept = regression.intercept,
                                                                         current.insecticide.efficacy = current.insecticide.efficacy.1)





  #Create vectors of start relative frequencies of the traits. Weighted by the relative
  #size of the intervention site and refugia.
  normal.distribution.trait.2.intervention = create_normal_distribution(vector.length = vector.length,
                                                                        trait.mean = intervention.trait.mean.2,
                                                                        standard.deviation = standard.deviation)

  normal.distribution.trait.2.refugia = create_normal_distribution(vector.length = vector.length,
                                                                   trait.mean = refugia.trait.mean.2,
                                                                   standard.deviation = standard.deviation)


  #Initial relative population frequencies
  relative.frequency.trait.2.intervention = calculate_density_of_trait_values(vector.length = vector.length,
                                                                              trait.mean = intervention.trait.mean.2,
                                                                              standard.deviation = standard.deviation)* (coverage)

  relative.frequency.trait.2.refugia = calculate_density_of_trait_values(vector.length = vector.length,
                                                                         trait.mean = refugia.trait.mean.2,
                                                                         standard.deviation = standard.deviation)* (1-coverage)


  #Create vectors of survival probabilities
  survival.probability.2.a = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.2.intervention,
                                                                                                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                           michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                           maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                         regression.coefficient = regression.coefficient,
                                                                         regression.intercept = regression.intercept,
                                                                         current.insecticide.efficacy = current.insecticide.efficacy.2)
  #if traits from refugia.
  survival.probability.2.b = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.2.refugia,
                                                                                                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                           michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                           maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                         regression.coefficient = regression.coefficient,
                                                                         regression.intercept = regression.intercept,
                                                                         current.insecticide.efficacy = current.insecticide.efficacy.2)

  #Create the empty lists to hold the values

  #Trait 1:::
  intervention.staying.1.a = list() #in intervention, staying ; traits from intervention distribution
  intervention.staying.1.b = list() #in intervention, staying ; traits from refugia distribution
  refugia.leaving.1.a = list() #moving from refugia to intervention ; traits from intervention distribution
  refugia.leaving.1.b = list() #moving from refugia to intervention ; traits from refugia distribution

  intervention.leaving.1.a = list() #moving from intervention to refugia ; traits from intervention distribution
  intervention.leaving.1.b = list() #moving from intervention to refugia ; traits from refugia distribution
  refugia.staying.1.a = list() #in refugia, staying ; traits from intervention distribution
  refugia.staying.1.b = list() # in refugia, staying ; traits from refugia distribution


  refugia.update.mean.1.a = list()
  intervention.update.mean.1.a = list()


  #egg laying then occurs::

  next.gen.intervention.1.a.staying = list()
  next.gen.intervention.1.a.joining = list()
  next.gen.intervention.1.b.staying = list()
  next.gen.intervention.1.b.joining = list()

  next.gen.refugia.1.a.staying = list()
  next.gen.refugia.1.a.joining = list()
  next.gen.refugia.1.b.staying = list()
  next.gen.refugia.1.b.joining = list()


  number.intervention.staying.1.a = list()
  number.intervention.staying.1.b = list()
  number.refugia.leaving.1.a = list()
  number.refugia.leaving.1.b = list()

  number.intervention.leaving.1.a = list()
  number.intervention.leaving.1.b = list()
  number.refugia.staying.1.a = list()
  number.refugia.staying.1.b = list()

  total.laying.refugia.1 = list()
  total.laying.intervention.1 = list()

  #And for trait 2:::
  #Create the empty lists to hold the values
  intervention.staying.2.a = list() #in intervention, staying ; traits from intervention distribution
  intervention.staying.2.b = list() #in intervention, staying ; traits from refugia distribution
  refugia.leaving.2.a = list() #moving from refugia to intervention ; traits from intervention distribution
  refugia.leaving.2.b = list() #moving from refugia to intervention ; traits from refugia distribution

  intervention.leaving.2.a = list() #moving from intervention to refugia ; traits from intervention distribution
  intervention.leaving.2.b = list() #moving from intervention to refugia ; traits from refugia distribution
  refugia.staying.2.a = list() #in refugia, staying ; traits from intervention distribution
  refugia.staying.2.b = list() # in refugia, staying ; traits from refugia distribution


  refugia.update.mean.2 = list()
  intervention.update.mean.2 = list()


  #egg laying then occurs::

  next.gen.intervention.2.a.staying = list()
  next.gen.intervention.2.a.joining = list()
  next.gen.intervention.2.b.staying = list()
  next.gen.intervention.2.b.joining = list()

  next.gen.refugia.2.a.staying = list()
  next.gen.refugia.2.a.joining = list()
  next.gen.refugia.2.b.staying = list()
  next.gen.refugia.2.b.joining = list()


  number.intervention.staying.2.a = list()
  number.intervention.staying.2.b = list()
  number.refugia.leaving.2.a = list()
  number.refugia.leaving.2.b = list()

  number.intervention.leaving.2.a = list()
  number.intervention.leaving.2.b = list()
  number.refugia.staying.2.a = list()
  number.refugia.staying.2.b = list()

  total.laying.refugia.2 = list()
  total.laying.intervention.2 = list()

  for(gonotrophic in 1:n.cycles){

    if(gonotrophic == 1){
      #Do first gonotrophic cycle::
      #Selection in intervention:
      intervention.selection.1.a = (relative.frequency.trait.1.intervention * insecticide.coverage.1* female.exposure* survival.probability.1.a) +
        (relative.frequency.trait.1.intervention * natural.survival * (1-female.exposure)) +
        (relative.frequency.trait.1.intervention * female.exposure * mean(survival.probability.2.a))

      intervention.selection.2.a = (relative.frequency.trait.2.intervention * insecticide.coverage.2* female.exposure* survival.probability.2.a) +
        (relative.frequency.trait.2.intervention * natural.survival * (1-female.exposure)) +
        (relative.frequency.trait.2.intervention * female.exposure * mean(survival.probability.1.a))


      #Selection in refugia
      refugia.selection.1.b = relative.frequency.trait.1.refugia * natural.survival
      refugia.selection.2.b = relative.frequency.trait.2.refugia * natural.survival


      #calculate the new mean after selection:::
      #intervention site::
      new.intervention.mean.1.a = sum(intervention.selection.1.a * normal.distribution.trait.1.intervention)/(sum(intervention.selection.1.a))
      new.intervention.mean.2.a = sum(intervention.selection.2.a * normal.distribution.trait.2.intervention)/(sum(intervention.selection.2.a))

      #refugia::
      new.refugia.mean.1.b = sum(refugia.selection.1.b * normal.distribution.trait.1.refugia)/(sum(refugia.selection.1.b))
      new.refugia.mean.2.b = sum(refugia.selection.2.b * normal.distribution.trait.2.refugia)/(sum(refugia.selection.2.b))

      #Calculate the selection differentials::
      intervention.selection.differential.1.a = new.intervention.mean.1.a - intervention.trait.mean.1
      intervention.selection.differential.2.a = new.intervention.mean.2.a - intervention.trait.mean.2

      refugia.selection.differential.1.b = new.refugia.mean.1.b - refugia.trait.mean.1
      refugia.selection.differential.2.b = new.refugia.mean.2.b - refugia.trait.mean.2


      #calculate the responses::
      response.intervention.1.a = (exposure.scaling.factor * heritability * ((intervention.selection.differential.1.a + male.selection.differential.intervention.1) / 2))
      response.refugia.1.b = (exposure.scaling.factor * heritability * ((refugia.selection.differential.1.b + male.selection.differential.refugia.1)/2))

      response.intervention.2.a = (exposure.scaling.factor * heritability * ((intervention.selection.differential.2.a + male.selection.differential.intervention.2) / 2))
      response.refugia.2.b = (exposure.scaling.factor * heritability * ((refugia.selection.differential.2.b + male.selection.differential.refugia.2)/2))


      #Who then disperse or stay to lay the eggs. Trait 1::
      intervention.staying.1.a[[gonotrophic]] = intervention.selection.1.a * (1-dispersal.rate)
      refugia.leaving.1.b[[gonotrophic]] = refugia.selection.1.b * dispersal.rate

      intervention.leaving.1.a[[gonotrophic]] = intervention.selection.1.a * dispersal.rate
      refugia.staying.1.b[[gonotrophic]] = refugia.selection.1.b * (1-dispersal.rate)

      #Who then disperse or stay to lay the eggs. Trait 2::
      intervention.staying.2.a[[gonotrophic]] = intervention.selection.2.a * (1-dispersal.rate)
      refugia.leaving.2.b[[gonotrophic]] = refugia.selection.2.b * dispersal.rate

      intervention.leaving.2.a[[gonotrophic]] = intervention.selection.2.a * dispersal.rate
      refugia.staying.2.b[[gonotrophic]] = refugia.selection.2.b * (1-dispersal.rate)


      #egg laying then occurs::
      #Trait 1::
      next.gen.intervention.1.a.staying[[gonotrophic]] = intervention.trait.mean.1 + response.intervention.1.a
      next.gen.intervention.1.b.joining[[gonotrophic]] = refugia.trait.mean.1 + response.refugia.1.b

      next.gen.refugia.1.a.staying[[gonotrophic]] = intervention.trait.mean.1 + response.intervention.1.a
      next.gen.refugia.1.b.joining[[gonotrophic]] = refugia.trait.mean.1 + response.refugia.1.b

      #Trait 2::
      next.gen.intervention.2.a.staying[[gonotrophic]] = intervention.trait.mean.2 + response.intervention.2.a
      next.gen.intervention.2.b.joining[[gonotrophic]] = refugia.trait.mean.2 + response.refugia.2.b

      next.gen.refugia.2.a.staying[[gonotrophic]] = intervention.trait.mean.2 + response.intervention.2.a
      next.gen.refugia.2.b.joining[[gonotrophic]] = refugia.trait.mean.2 + response.refugia.2.b



      #weighting::
      #relative egg contributions
      #Trait 1::
      number.intervention.staying.1.a[[gonotrophic]] = sum(intervention.staying.1.a[[gonotrophic]])
      number.refugia.leaving.1.b[[gonotrophic]] = sum(refugia.leaving.1.b[[gonotrophic]])
      total.laying.intervention.1[[gonotrophic]] = sum(number.intervention.staying.1.a[[gonotrophic]], number.refugia.leaving.1.b[[gonotrophic]])

      number.intervention.leaving.1.a[[gonotrophic]] = sum(intervention.leaving.1.a[[gonotrophic]])
      number.refugia.staying.1.b[[gonotrophic]] = sum(refugia.staying.1.b[[gonotrophic]])
      total.laying.refugia.1[[gonotrophic]] = sum(number.intervention.leaving.1.a[[gonotrophic]], number.refugia.staying.1.b[[gonotrophic]])

      #Trait 2::
      number.intervention.staying.2.a[[gonotrophic]] = sum(intervention.staying.2.a[[gonotrophic]])
      number.refugia.leaving.2.b[[gonotrophic]] = sum(refugia.leaving.2.b[[gonotrophic]])
      total.laying.intervention.2[[gonotrophic]] = sum(number.intervention.staying.2.a[[gonotrophic]], number.refugia.leaving.2.b[[gonotrophic]])

      number.intervention.leaving.2.a[[gonotrophic]] = sum(intervention.leaving.2.a[[gonotrophic]])
      number.refugia.staying.2.b[[gonotrophic]] = sum(refugia.staying.2.b[[gonotrophic]])
      total.laying.refugia.2[[gonotrophic]] = sum(number.intervention.leaving.2.a[[gonotrophic]], number.refugia.staying.2.b[[gonotrophic]])

    }

    if(gonotrophic != 1){

      #calcuate mean survivals::
        #Trait 1::
    #  current.intervention.mean.1.a = sum(intervention.staying.1.a[[gonotrophic-1]] * normal.distribution.trait.1.intervention)/(sum((intervention.staying.1.a[[gonotrophic-1]])))
    #  mean.survival.probability.1.a = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = current.intervention.mean.1.a,
    #                                                                                                                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
    #                                                                                                                                                michaelis.menten.slope = michaelis.menten.slope,
    #                                                                                                                                                maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
    #                                                                              regression.coefficient = regression.coefficient,
    #                                                                              regression.intercept = regression.intercept,
    #                                                                              current.insecticide.efficacy = current.insecticide.efficacy.1)
    #
    #
    # current.intervention.mean.1.b = sum(intervention.staying.1.b[[gonotrophic-1]] * normal.distribution.trait.1.refugia)/(sum((intervention.staying.1.b[[gonotrophic-1]])))
    # mean.survival.probability.1.b = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = current.intervention.mean.1.b,
    #                                                                                                                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
    #                                                                                                                                               michaelis.menten.slope = michaelis.menten.slope,
    #                                                                                                                                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
    #                                                                             regression.coefficient = regression.coefficient,
    #                                                                             regression.intercept = regression.intercept,
    #                                                                             current.insecticide.efficacy = current.insecticide.efficacy.1)
    #
    #  #Trait 2::
    # current.intervention.mean.2.a = sum(intervention.staying.2.a[[gonotrophic-1]] * normal.distribution.trait.2.intervention)/(sum((intervention.staying.2.a[[gonotrophic-1]])))
    # mean.survival.probability.2.a = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = current.intervention.mean.2.a,
    #                                                                                                                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
    #                                                                                                                                               michaelis.menten.slope = michaelis.menten.slope,
    #                                                                                                                                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
    #                                                                             regression.coefficient = regression.coefficient,
    #                                                                             regression.intercept = regression.intercept,
    #                                                                             current.insecticide.efficacy = current.insecticide.efficacy.2)
    #
    #
    # current.intervention.mean.2.b = sum(intervention.staying.2.b[[gonotrophic-1]] * normal.distribution.trait.2.refugia)/(sum((intervention.staying.2.b[[gonotrophic-1]])))
    # mean.survival.probability.2.b = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = current.intervention.mean.2.b,
    #                                                                                                                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
    #                                                                                                                                               michaelis.menten.slope = michaelis.menten.slope,
    #                                                                                                                                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
    #                                                                             regression.coefficient = regression.coefficient,
    #                                                                             regression.intercept = regression.intercept,
    #                                                                             current.insecticide.efficacy = current.insecticide.efficacy.2)



      #First do the selection
      #Trait 1::
      intervention.selection.1.a = (intervention.staying.1.a[[gonotrophic-1]] * female.exposure * insecticide.coverage.1 * survival.probability.1.a) +
        (intervention.staying.1.a[[gonotrophic-1]] * (1-female.exposure)*natural.survival) +
        (intervention.staying.1.a[[gonotrophic-1]] * female.exposure * insecticide.coverage.2 * mean(survival.probability.2.a))

      intervention.selection.1.b = (refugia.leaving.1.b[[gonotrophic - 1]] * female.exposure * insecticide.coverage.1* survival.probability.1.b) +
        (refugia.leaving.1.b[[gonotrophic - 1]] * (1-female.exposure)*natural.survival) +
        (refugia.leaving.1.b[[gonotrophic - 1]] * female.exposure * insecticide.coverage.2* mean(survival.probability.2.b))

      refugia.selection.1.a = intervention.leaving.1.a[[gonotrophic - 1]] * natural.survival
      refugia.selection.1.b = refugia.staying.1.b[[gonotrophic - 1]] * natural.survival

      #Trait 2::
      intervention.selection.2.a = (intervention.staying.2.a[[gonotrophic-1]] * female.exposure * insecticide.coverage.2 * survival.probability.2.a) +
        (intervention.staying.2.a[[gonotrophic-1]] * (1-female.exposure)*natural.survival) +
        (intervention.staying.2.a[[gonotrophic-1]] * female.exposure * insecticide.coverage.1 * mean(survival.probability.1.a))

      intervention.selection.2.b = (refugia.leaving.2.b[[gonotrophic - 1]] * female.exposure * insecticide.coverage.2* survival.probability.2.b) +
        (refugia.leaving.2.b[[gonotrophic - 1]] * (1-female.exposure)*natural.survival) +
        (refugia.leaving.2.b[[gonotrophic - 1]] * female.exposure * insecticide.coverage.1* mean(survival.probability.1.b))

      refugia.selection.2.a = intervention.leaving.2.a[[gonotrophic - 1]] * natural.survival
      refugia.selection.2.b = refugia.staying.2.b[[gonotrophic - 1]] * natural.survival





      #Next, calculate the new means after selection:::
      #Trait 1::
      #intervention site::
      new.intervention.mean.1.a = sum(intervention.selection.1.a * normal.distribution.trait.1.intervention)/(sum(intervention.selection.1.a))
      new.intervention.mean.1.b = sum(intervention.selection.1.b * normal.distribution.trait.1.refugia)/(sum(intervention.selection.1.b))
      #refugia::
      new.refugia.mean.1.a = sum(refugia.selection.1.a * normal.distribution.trait.1.intervention)/(sum(refugia.selection.1.a))
      new.refugia.mean.1.b = sum(refugia.selection.1.b * normal.distribution.trait.1.refugia)/(sum(refugia.selection.1.b))



      #Trait 2::
      #intervention site::
      new.intervention.mean.2.a = sum(intervention.selection.2.a * normal.distribution.trait.2.intervention)/(sum(intervention.selection.2.a))
      new.intervention.mean.2.b = sum(intervention.selection.2.b * normal.distribution.trait.2.refugia)/(sum(intervention.selection.2.b))
      #refugia::
      new.refugia.mean.2.a = sum(refugia.selection.2.a * normal.distribution.trait.2.intervention)/(sum(refugia.selection.2.a))
      new.refugia.mean.2.b = sum(refugia.selection.2.b * normal.distribution.trait.2.refugia)/(sum(refugia.selection.2.b))


      #Calculate the selection differentials::
      #Trait 1::
      intervention.selection.differential.1.a = new.intervention.mean.1.a - intervention.trait.mean.1
      intervention.selection.differential.1.b = new.intervention.mean.1.b - refugia.trait.mean.1
      refugia.selection.differential.1.a = new.intervention.mean.1.a - intervention.trait.mean.1
      refugia.selection.differential.1.b = new.refugia.mean.1.b - refugia.trait.mean.1

      #Trait 2::
      intervention.selection.differential.2.a = new.intervention.mean.2.a - intervention.trait.mean.2
      intervention.selection.differential.2.b = new.intervention.mean.2.b - refugia.trait.mean.2
      refugia.selection.differential.2.a = new.intervention.mean.2.a - intervention.trait.mean.2
      refugia.selection.differential.2.b = new.refugia.mean.2.b - refugia.trait.mean.2


      #calculate the responses::
      #Trait 1::
      response.intervention.1.a = (exposure.scaling.factor * heritability * ((intervention.selection.differential.1.a + male.selection.differential.intervention.1) / 2))
      response.intervention.1.b = (exposure.scaling.factor * heritability * ((intervention.selection.differential.1.b + male.selection.differential.refugia.1) / 2))
      response.refugia.1.a = (exposure.scaling.factor * heritability * ((refugia.selection.differential.1.a + male.selection.differential.intervention.1)/2))
      response.refugia.1.b = (exposure.scaling.factor * heritability * ((refugia.selection.differential.1.b + male.selection.differential.refugia.1)/2))

      #Trait 2::
      response.intervention.2.a = (exposure.scaling.factor * heritability * ((intervention.selection.differential.2.a + male.selection.differential.intervention.2) / 2))
      response.intervention.2.b = (exposure.scaling.factor * heritability * ((intervention.selection.differential.2.b + male.selection.differential.refugia.2) / 2))
      response.refugia.2.a = (exposure.scaling.factor * heritability * ((refugia.selection.differential.2.a + male.selection.differential.intervention.2)/2))
      response.refugia.2.b = (exposure.scaling.factor * heritability * ((refugia.selection.differential.2.b + male.selection.differential.refugia.2)/2))


      #Who then disperse or stay to lay the eggs.

      #Trait 1:::
      #Dispersal (staying in and joining Intervention)
      intervention.staying.1.a[[gonotrophic]] = intervention.selection.1.a * (1-dispersal.rate)
      intervention.staying.1.b[[gonotrophic]] = intervention.selection.1.b * (1-dispersal.rate)
      refugia.leaving.1.a[[gonotrophic]] = refugia.selection.1.a * dispersal.rate
      refugia.leaving.1.b[[gonotrophic]] = refugia.selection.1.b * dispersal.rate

      #Dispersal (staying in and joining refugia)
      intervention.leaving.1.a[[gonotrophic]] = intervention.selection.1.a * dispersal.rate
      intervention.leaving.1.b[[gonotrophic]] = intervention.selection.1.b * dispersal.rate
      refugia.staying.1.a[[gonotrophic]] = refugia.selection.1.a * (1-dispersal.rate)
      refugia.staying.1.b[[gonotrophic]] = refugia.selection.1.b * (1-dispersal.rate)


      #Trait 2:::
      #Dispersal (staying in and joining Intervention)
      intervention.staying.2.a[[gonotrophic]] = intervention.selection.2.a * (1-dispersal.rate)
      intervention.staying.2.b[[gonotrophic]] = intervention.selection.2.b * (1-dispersal.rate)
      refugia.leaving.2.a[[gonotrophic]] = refugia.selection.2.a * dispersal.rate
      refugia.leaving.2.b[[gonotrophic]] = refugia.selection.2.b * dispersal.rate

      #Dispersal (staying in and joining refugia)
      intervention.leaving.2.a[[gonotrophic]] = intervention.selection.2.a * dispersal.rate
      intervention.leaving.2.b[[gonotrophic]] = intervention.selection.2.b * dispersal.rate
      refugia.staying.2.a[[gonotrophic]] = refugia.selection.2.a * (1-dispersal.rate)
      refugia.staying.2.b[[gonotrophic]] = refugia.selection.2.b * (1-dispersal.rate)



      #egg laying then occurs::
          #Trait 1:::
      next.gen.intervention.1.a.staying[[gonotrophic]] = intervention.trait.mean.1 + response.intervention.1.a
      next.gen.intervention.1.a.joining[[gonotrophic]] = intervention.trait.mean.1 + response.refugia.1.a
      next.gen.intervention.1.b.staying[[gonotrophic]] = refugia.trait.mean.1 + response.intervention.1.b
      next.gen.intervention.1.b.joining[[gonotrophic]] = refugia.trait.mean.1 + response.refugia.1.b

      next.gen.refugia.1.a.staying[[gonotrophic]] = intervention.trait.mean.1 + response.refugia.1.a
      next.gen.refugia.1.a.joining[[gonotrophic]] = intervention.trait.mean.1 + response.intervention.1.a
      next.gen.refugia.1.b.staying[[gonotrophic]] = refugia.trait.mean.1 + response.refugia.1.b
      next.gen.refugia.1.b.joining[[gonotrophic]] = refugia.trait.mean.1 + response.intervention.1.b

          #Trait 2:::
      next.gen.intervention.2.a.staying[[gonotrophic]] = intervention.trait.mean.2 + response.intervention.2.a
      next.gen.intervention.2.a.joining[[gonotrophic]] = intervention.trait.mean.2 + response.refugia.2.a
      next.gen.intervention.2.b.staying[[gonotrophic]] = refugia.trait.mean.2 + response.intervention.2.b
      next.gen.intervention.2.b.joining[[gonotrophic]] = refugia.trait.mean.2 + response.refugia.2.b

      next.gen.refugia.2.a.staying[[gonotrophic]] = intervention.trait.mean.2 + response.refugia.2.a
      next.gen.refugia.2.a.joining[[gonotrophic]] = intervention.trait.mean.2 + response.intervention.2.a
      next.gen.refugia.2.b.staying[[gonotrophic]] = refugia.trait.mean.2 + response.refugia.2.b
      next.gen.refugia.2.b.joining[[gonotrophic]] = refugia.trait.mean.2 + response.intervention.2.b


      #relative egg contributions
        #Trait 1:::
      number.intervention.staying.1.a[[gonotrophic]] = sum(intervention.staying.1.a[[gonotrophic]])
      number.intervention.staying.1.b[[gonotrophic]] = sum(intervention.staying.1.b[[gonotrophic]])
      number.refugia.leaving.1.a[[gonotrophic]] = sum(refugia.leaving.1.a[[gonotrophic]])
      number.refugia.leaving.1.b[[gonotrophic]] = sum(refugia.leaving.1.b[[gonotrophic]])


      total.laying.intervention.1[[gonotrophic]] = sum(number.intervention.staying.1.a[[gonotrophic]],
                                                     number.intervention.staying.1.b[[gonotrophic]],
                                                     number.refugia.leaving.1.a[[gonotrophic]],
                                                     number.refugia.leaving.1.b[[gonotrophic]])

      number.intervention.leaving.1.a[[gonotrophic]] = sum(intervention.leaving.1.a[[gonotrophic]])
      number.intervention.leaving.1.b[[gonotrophic]] = sum(intervention.leaving.1.b[[gonotrophic]])
      number.refugia.staying.1.a[[gonotrophic]] = sum(refugia.staying.1.a[[gonotrophic]])
      number.refugia.staying.1.b[[gonotrophic]] = sum(refugia.staying.1.b[[gonotrophic]])

      total.laying.refugia.1[[gonotrophic]] = sum(number.intervention.leaving.1.a[[gonotrophic]],
                                                number.intervention.leaving.1.b[[gonotrophic]],
                                                number.refugia.staying.1.a[[gonotrophic]],
                                                number.refugia.staying.1.b[[gonotrophic]])



      #Trait 2:::
      number.intervention.staying.2.a[[gonotrophic]] = sum(intervention.staying.2.a[[gonotrophic]])
      number.intervention.staying.2.b[[gonotrophic]] = sum(intervention.staying.2.b[[gonotrophic]])
      number.refugia.leaving.2.a[[gonotrophic]] = sum(refugia.leaving.2.a[[gonotrophic]])
      number.refugia.leaving.2.b[[gonotrophic]] = sum(refugia.leaving.2.b[[gonotrophic]])


      total.laying.intervention.2[[gonotrophic]] = sum(number.intervention.staying.2.a[[gonotrophic]],
                                                       number.intervention.staying.2.b[[gonotrophic]],
                                                       number.refugia.leaving.2.a[[gonotrophic]],
                                                       number.refugia.leaving.2.b[[gonotrophic]])

      number.intervention.leaving.2.a[[gonotrophic]] = sum(intervention.leaving.2.a[[gonotrophic]])
      number.intervention.leaving.2.b[[gonotrophic]] = sum(intervention.leaving.2.b[[gonotrophic]])
      number.refugia.staying.2.a[[gonotrophic]] = sum(refugia.staying.2.a[[gonotrophic]])
      number.refugia.staying.2.b[[gonotrophic]] = sum(refugia.staying.2.b[[gonotrophic]])

      total.laying.refugia.2[[gonotrophic]] = sum(number.intervention.leaving.2.a[[gonotrophic]],
                                                  number.intervention.leaving.2.b[[gonotrophic]],
                                                  number.refugia.staying.2.a[[gonotrophic]],
                                                  number.refugia.staying.2.b[[gonotrophic]])
    }
  }

  #weight the responses by the population sizes (number of females / number of eggs)
 #Trait 1::
  #Calculate the mean of the next generation for the refugia:::
  #all the eggs laid in total

  total.egg.output.refugia.1 = sum(unlist(total.laying.refugia.1))

  refugia.egg.batches.1 = c(unlist(next.gen.refugia.1.a.staying), unlist(next.gen.refugia.1.a.joining), unlist(next.gen.refugia.1.b.staying), unlist(next.gen.refugia.1.b.joining))
  refugia.batch.sizes.1 = c(unlist(number.refugia.staying.1.a), unlist(number.intervention.leaving.1.a), unlist(number.refugia.staying.1.b), unlist(number.intervention.leaving.1.b))

  update.refugia.mean.1 = sum((refugia.egg.batches.1 * (refugia.batch.sizes.1/total.egg.output.refugia.1)))
  update.refugia.mean.1 = ifelse(update.refugia.mean.1 < 0,
                               yes = 0,
                               no = update.refugia.mean.1)

  #and for intervention site:::
  total.egg.output.intervention.1 = sum(unlist(total.laying.intervention.1))

  intervention.egg.batches.1 = c(unlist(next.gen.intervention.1.a.staying), unlist(next.gen.intervention.1.a.joining), unlist(next.gen.intervention.1.b.staying), unlist(next.gen.intervention.1.b.joining))
  intervention.batch.sizes.1 = c(unlist(number.intervention.staying.1.a), unlist(number.refugia.leaving.1.a), unlist(number.intervention.staying.1.b), unlist(number.refugia.leaving.1.b))

  update.intervention.mean.1 = sum((intervention.egg.batches.1 * (intervention.batch.sizes.1/total.egg.output.intervention.1)))

  update.intervention.mean.1 = ifelse(update.intervention.mean.1 < 0,
                                    yes = 0,
                                    no = update.intervention.mean.1)


  #Trait 2::
  #Calculate the mean of the next generation for the refugia:::
  #all the eggs laid in total

  total.egg.output.refugia.2 = sum(unlist(total.laying.refugia.2))

  refugia.egg.batches.2 = c(unlist(next.gen.refugia.2.a.staying), unlist(next.gen.refugia.2.a.joining), unlist(next.gen.refugia.2.b.staying), unlist(next.gen.refugia.2.b.joining))
  refugia.batch.sizes.2 = c(unlist(number.refugia.staying.2.a), unlist(number.intervention.leaving.2.a), unlist(number.refugia.staying.2.b), unlist(number.intervention.leaving.2.b))

  update.refugia.mean.2 = sum((refugia.egg.batches.2 * (refugia.batch.sizes.2/total.egg.output.refugia.2)))
  update.refugia.mean.2 = ifelse(update.refugia.mean.2 < 0,
                                 yes = 0,
                                 no = update.refugia.mean.2)

  #and for intervention site:::
  total.egg.output.intervention.2 = sum(unlist(total.laying.intervention.2))

  intervention.egg.batches.2 = c(unlist(next.gen.intervention.2.a.staying), unlist(next.gen.intervention.2.a.joining), unlist(next.gen.intervention.2.b.staying), unlist(next.gen.intervention.2.b.joining))
  intervention.batch.sizes.2 = c(unlist(number.intervention.staying.2.a), unlist(number.refugia.leaving.2.a), unlist(number.intervention.staying.2.b), unlist(number.refugia.leaving.2.b))

  update.intervention.mean.2 = sum((intervention.egg.batches.2 * (intervention.batch.sizes.2/total.egg.output.intervention.2)))

  update.intervention.mean.2 = ifelse(update.intervention.mean.2 < 0,
                                      yes = 0,
                                      no = update.intervention.mean.2)





  return(list(update.intervention.mean.1, update.refugia.mean.1,
              update.intervention.mean.2, update.refugia.mean.2))
}
