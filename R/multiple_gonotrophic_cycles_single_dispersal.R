multiple_gonotrophic_cycles_single_dispersal = function(coverage = 0.8,
                                                        dispersal.rate = 0.2,
                                                        female.exposure = 0.7,
                                                        refugia.trait.mean = 0,
                                                        heritability = 0.3,
                                                        intervention.trait.mean = 10,
                                                        n.cycles = 5,
                                                        male.selection.differential.intervention = 0,
                                                        male.selection.differential.refugia = 0,
                                                        female.fitness.cost.intervention = 0,
                                                        female.fitness.cost.refugia = 0,
                                                        exposure.scaling.factor = 10,
                                                        vector.length = 1000,
                                                        standard.deviation = 30,
                                                        half.population.bioassay.survival.resistance = 900,
                                                        michaelis.menten.slope = 1,
                                                        maximum.bioassay.survival.proportion = 1,
                                                        regression.coefficient = 0.48,
                                                        regression.intercept = 0.15,
                                                        current.insecticide.efficacy = 1){


  normal.distribution.trait.1.intervention = create_normal_distribution(vector.length = vector.length,
                                                                        trait.mean = intervention.trait.mean,
                                                                        standard.deviation = standard.deviation)

  normal.distribution.trait.1.refugia = create_normal_distribution(vector.length = vector.length,
                                                                   trait.mean = refugia.trait.mean,
                                                                   standard.deviation = standard.deviation)



  #Create vectors of start relative frequencies of the traits. Weighted by the relative
  #size of the intervention site and refugia.

  relative.frequency.trait.1.intervention = calculate_density_of_trait_values(vector.length = vector.length,
                                                                              trait.mean = intervention.trait.mean,
                                                                              standard.deviation = standard.deviation)* (coverage)

  relative.frequency.trait.1.refugia = calculate_density_of_trait_values(vector.length = vector.length,
                                                                         trait.mean = refugia.trait.mean,
                                                                         standard.deviation = standard.deviation)* (1-coverage)


  #Create vectors of survival probabilities
  survival.probability.1.a = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.1.intervention,
                                                                                                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                           michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                           maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                         regression.coefficient = regression.coefficient,
                                                                         regression.intercept = regression.intercept,
                                                                         current.insecticide.efficacy = current.insecticide.efficacy)
  #if traits from refugia.
  survival.probability.1.b = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = normal.distribution.trait.1.refugia,
                                                                                                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                           michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                           maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                         regression.coefficient = regression.coefficient,
                                                                         regression.intercept = regression.intercept,
                                                                         current.insecticide.efficacy = current.insecticide.efficacy)


  #Create the empty lists to hold the values
  intervention.staying.a = list() #in intervention, staying ; traits from intervention distribution
  intervention.staying.b = list() #in intervention, staying ; traits from refugia distribution
  refugia.leaving.a = list() #moving from refugia to intervention ; traits from intervention distribution
  refugia.leaving.b = list() #moving from refugia to intervention ; traits from refugia distribution

  intervention.leaving.a = list() #moving from intervention to refugia ; traits from intervention distribution
  intervention.leaving.b = list() #moving from intervention to refugia ; traits from refugia distribution
  refugia.staying.a = list() #in refugia, staying ; traits from intervention distribution
  refugia.staying.b = list() # in refugia, staying ; traits from refugia distribution


  refugia.update.mean = list()
  intervention.update.mean = list()


  #egg laying then occurs::

  next.gen.intervention.a.staying = list()
  next.gen.intervention.a.joining = list()
  next.gen.intervention.b.staying = list()
  next.gen.intervention.b.joining = list()

  next.gen.refugia.a.staying = list()
  next.gen.refugia.a.joining = list()
  next.gen.refugia.b.staying = list()
  next.gen.refugia.b.joining = list()


  number.intervention.staying.a = list()
  number.intervention.staying.b = list()
  number.refugia.leaving.a = list()
  number.refugia.leaving.b = list()

  number.intervention.leaving.a = list()
  number.intervention.leaving.b = list()
  number.refugia.staying.a = list()
  number.refugia.staying.b = list()

  total.laying.refugia = list()
  total.laying.intervention = list()


  if(coverage < 1){

  for(gonotrophic in 1:n.cycles){

    if(gonotrophic == 1){
      #Do first gonotrophic cycle::
      #Selection in intervention:
      intervention.selection.a = (relative.frequency.trait.1.intervention * female.exposure* survival.probability.1.a) + (relative.frequency.trait.1.intervention * (1-female.exposure))

      #Selection in refugia
      refugia.selection.b = relative.frequency.trait.1.refugia

      #calculate the new mean after selection:::
      #intervention site::
      new.intervention.mean.a = sum(intervention.selection.a * normal.distribution.trait.1.intervention)/(sum(intervention.selection.a))

      #refugia::
      new.refugia.mean.b = sum(refugia.selection.b * normal.distribution.trait.1.refugia)/(sum(refugia.selection.b))

      #Calculate the selection differentials::
      intervention.selection.differential.a = (exposure.scaling.factor*(new.intervention.mean.a - intervention.trait.mean)) - female.fitness.cost.intervention

      refugia.selection.differential.b = (exposure.scaling.factor*(new.refugia.mean.b - refugia.trait.mean)) - female.fitness.cost.refugia

      #calculate the responses::
      response.intervention.a = heritability * ((intervention.selection.differential.a + male.selection.differential.intervention) / 2)
      response.refugia.b = heritability * ((refugia.selection.differential.b + male.selection.differential.refugia)/2)


      #Who then disperse or stay to lay the eggs.
      intervention.staying.a[[gonotrophic]] = intervention.selection.a * (1-dispersal.rate)
      refugia.leaving.b[[gonotrophic]] = refugia.selection.b * dispersal.rate

      intervention.leaving.a[[gonotrophic]] = intervention.selection.a * dispersal.rate
      refugia.staying.b[[gonotrophic]] = refugia.selection.b * (1-dispersal.rate)


      #egg laying then occurs::
      #weighting::
      next.gen.intervention.a.staying[[gonotrophic]] = intervention.trait.mean + response.intervention.a
      next.gen.intervention.b.joining[[gonotrophic]] = refugia.trait.mean + response.refugia.b

      next.gen.refugia.a.staying[[gonotrophic]] = intervention.trait.mean + response.intervention.a
      next.gen.refugia.b.joining[[gonotrophic]] = refugia.trait.mean + response.refugia.b


      #relative egg contributions
      number.intervention.staying.a[[gonotrophic]] = sum(intervention.staying.a[[gonotrophic]])
      number.refugia.leaving.b[[gonotrophic]] = sum(refugia.leaving.b[[gonotrophic]])
      total.laying.intervention[[gonotrophic]] = sum(number.intervention.staying.a[[gonotrophic]], number.refugia.leaving.b[[gonotrophic]])

      number.intervention.leaving.a[[gonotrophic]] = sum(intervention.leaving.a[[gonotrophic]])
      number.refugia.staying.b[[gonotrophic]] = sum(refugia.staying.b[[gonotrophic]])
      total.laying.refugia[[gonotrophic]] = sum(number.intervention.leaving.a[[gonotrophic]], number.refugia.staying.b[[gonotrophic]])
    }

    if(gonotrophic != 1){

      #First do the selection
      intervention.selection.a = (intervention.staying.a[[gonotrophic-1]] * female.exposure * survival.probability.1.a) + (intervention.staying.a[[gonotrophic-1]] * (1-female.exposure))
      intervention.selection.b = (refugia.leaving.b[[gonotrophic - 1]] * female.exposure * survival.probability.1.b) + (refugia.leaving.b[[gonotrophic - 1]] * (1-female.exposure))

      refugia.selection.a = intervention.leaving.a[[gonotrophic - 1]]
      refugia.selection.b = refugia.staying.b[[gonotrophic - 1]]


      #calculate the new means after selection:::
      #intervention site::
      new.intervention.mean.a = sum(intervention.selection.a * normal.distribution.trait.1.intervention)/(sum(intervention.selection.a))
      new.intervention.mean.b = sum(intervention.selection.b * normal.distribution.trait.1.refugia)/(sum(intervention.selection.b))
      #refugia::
      new.refugia.mean.a = sum(refugia.selection.a * normal.distribution.trait.1.intervention)/(sum(refugia.selection.a))
      new.refugia.mean.b = sum(refugia.selection.b * normal.distribution.trait.1.refugia)/(sum(refugia.selection.b))


      #Calculate the selection differentials::
      intervention.selection.differential.a = (exposure.scaling.factor *((new.intervention.mean.a - intervention.trait.mean))) - female.fitness.cost.intervention
      intervention.selection.differential.b = (exposure.scaling.factor *(new.intervention.mean.b - refugia.trait.mean)) - female.fitness.cost.refugia

      refugia.selection.differential.a = (exposure.scaling.factor *(new.intervention.mean.a - intervention.trait.mean)) - female.fitness.cost.intervention
      refugia.selection.differential.b = (exposure.scaling.factor *(new.refugia.mean.b - refugia.trait.mean)) - female.fitness.cost.refugia

      #calculate the responses::
      response.intervention.a =  heritability * ((intervention.selection.differential.a + male.selection.differential.intervention) / 2)
      response.intervention.b =  heritability * ((intervention.selection.differential.b + male.selection.differential.refugia) / 2)

      response.refugia.a = heritability * ((refugia.selection.differential.a + male.selection.differential.intervention)/2)
      response.refugia.b = heritability * ((refugia.selection.differential.b + male.selection.differential.refugia)/2)



      #Who then disperse or stay to lay the eggs.
      #Dispersal (staying in and joining Intervention)
      intervention.staying.a[[gonotrophic]] = intervention.selection.a * (1-dispersal.rate)
      intervention.staying.b[[gonotrophic]] = intervention.selection.b * (1-dispersal.rate)
      refugia.leaving.a[[gonotrophic]] = refugia.selection.a * dispersal.rate
      refugia.leaving.b[[gonotrophic]] = refugia.selection.b * dispersal.rate

      #Dispersal (staying in and joining refugia)
      intervention.leaving.a[[gonotrophic]] = intervention.selection.a * dispersal.rate
      intervention.leaving.b[[gonotrophic]] = intervention.selection.b * dispersal.rate
      refugia.staying.a[[gonotrophic]] = refugia.selection.a * (1-dispersal.rate)
      refugia.staying.b[[gonotrophic]] = refugia.selection.b * (1-dispersal.rate)

      #egg laying then occurs::

      next.gen.intervention.a.staying[[gonotrophic]] = intervention.trait.mean + response.intervention.a
      next.gen.intervention.a.joining[[gonotrophic]] = intervention.trait.mean + response.refugia.a
      next.gen.intervention.b.staying[[gonotrophic]] = refugia.trait.mean + response.intervention.b
      next.gen.intervention.b.joining[[gonotrophic]] = refugia.trait.mean + response.refugia.b

      next.gen.refugia.a.staying[[gonotrophic]] = intervention.trait.mean + response.refugia.a
      next.gen.refugia.a.joining[[gonotrophic]] = intervention.trait.mean + response.intervention.a
      next.gen.refugia.b.staying[[gonotrophic]] = refugia.trait.mean + response.refugia.b
      next.gen.refugia.b.joining[[gonotrophic]] = refugia.trait.mean + response.intervention.b

      #relative egg contributions
      number.intervention.staying.a[[gonotrophic]] = sum(intervention.staying.a[[gonotrophic]])
      number.intervention.staying.b[[gonotrophic]] = sum(intervention.staying.b[[gonotrophic]])
      number.refugia.leaving.a[[gonotrophic]] = sum(refugia.leaving.a[[gonotrophic]])
      number.refugia.leaving.b[[gonotrophic]] = sum(refugia.leaving.b[[gonotrophic]])


      total.laying.intervention[[gonotrophic]] = sum(number.intervention.staying.a[[gonotrophic]],
                                                     number.intervention.staying.b[[gonotrophic]],
                                                     number.refugia.leaving.a[[gonotrophic]],
                                                     number.refugia.leaving.b[[gonotrophic]])

      number.intervention.leaving.a[[gonotrophic]] = sum(intervention.leaving.a[[gonotrophic]])
      number.intervention.leaving.b[[gonotrophic]] = sum(intervention.leaving.b[[gonotrophic]])
      number.refugia.staying.a[[gonotrophic]] = sum(refugia.staying.a[[gonotrophic]])
      number.refugia.staying.b[[gonotrophic]] = sum(refugia.staying.b[[gonotrophic]])

      total.laying.refugia[[gonotrophic]] = sum(number.intervention.leaving.a[[gonotrophic]],
                                                number.intervention.leaving.b[[gonotrophic]],
                                                number.refugia.staying.a[[gonotrophic]],
                                                number.refugia.staying.b[[gonotrophic]])




    }
  }

  #weight the responses by the population sizes (number of females / number of eggs)

  #Calculate the mean of the next generation for the refugia:::

  #all the eggs laid in total
  total.egg.output.refugia = sum(unlist(total.laying.refugia))

  refugia.egg.batches = c(unlist(next.gen.refugia.a.staying), unlist(next.gen.refugia.a.joining), unlist(next.gen.refugia.b.staying), unlist(next.gen.refugia.b.joining))
  refugia.batch.sizes = c(unlist(number.refugia.staying.a), unlist(number.intervention.leaving.a), unlist(number.refugia.staying.b), unlist(number.intervention.leaving.b))

  update.refugia.mean = sum((refugia.egg.batches * (refugia.batch.sizes/total.egg.output.refugia)))

  #Prevent the mean resistance from falling below 0.
  update.refugia.mean = ifelse(update.refugia.mean < 0,
                               yes = 0,
                               no = update.refugia.mean)


  #and for intervention site:::
  total.egg.output.intervention = sum(unlist(total.laying.intervention))

  intervention.egg.batches = c(unlist(next.gen.intervention.a.staying), unlist(next.gen.intervention.a.joining), unlist(next.gen.intervention.b.staying), unlist(next.gen.intervention.b.joining))
  intervention.batch.sizes = c(unlist(number.intervention.staying.a), unlist(number.refugia.leaving.a), unlist(number.intervention.staying.b), unlist(number.refugia.leaving.b))

  intervention.weightings = intervention.batch.sizes/total.egg.output.intervention
  individual.batch.means = c(intervention.egg.batches)

  update.intervention.mean = sum((intervention.egg.batches * (intervention.batch.sizes/total.egg.output.intervention)))


  #Prevent the mean resistance from falling below 0.
  update.intervention.mean = ifelse(update.intervention.mean < 0,
                                    yes = 0,
                                    no = update.intervention.mean)


  return(list(update.intervention.mean, update.refugia.mean))
  }

  #If coverage is set at 1; then there is only the intervention site, the refugia does not exist therefore need to remove all the dispersal etc
#if dispersal == 0, then even if the refugia "exists" mosquitoes cannot fly to it therefore calculate as though only intervention site
  if(coverage == 1 | dispersal.rate == 0){

    for(gonotrophic in 1:n.cycles){

      if(gonotrophic == 1){
        #Do first gonotrophic cycle::
        #Selection in intervention:
        intervention.selection.a = (relative.frequency.trait.1.intervention * female.exposure* survival.probability.1.a) + (relative.frequency.trait.1.intervention * (1-female.exposure))


        #calculate the new mean after selection:::
        #intervention site::
        new.intervention.mean.a = sum(intervention.selection.a * normal.distribution.trait.1.intervention)/(sum(intervention.selection.a))

        #refugia::

        #Calculate the selection differentials::
        intervention.selection.differential.a = (exposure.scaling.factor*(new.intervention.mean.a - intervention.trait.mean)) - female.fitness.cost.intervention


        #calculate the responses::
        response.intervention.a = heritability * ((intervention.selection.differential.a + male.selection.differential.intervention) / 2)


        #Who then disperse or stay to lay the eggs.
        intervention.staying.a[[gonotrophic]] = intervention.selection.a

        #egg laying then occurs::
        #weighting::
        next.gen.intervention.a.staying[[gonotrophic]] = intervention.trait.mean + response.intervention.a

        #relative egg contributions
        number.intervention.staying.a[[gonotrophic]] = sum(intervention.staying.a[[gonotrophic]])
        total.laying.intervention[[gonotrophic]] = sum(number.intervention.staying.a[[gonotrophic]])
        }

      if(gonotrophic != 1){

        #First do the selection
        intervention.selection.a = (intervention.staying.a[[gonotrophic-1]] * female.exposure * survival.probability.1.a) + (intervention.staying.a[[gonotrophic-1]] * (1-female.exposure))


        #calculate the new means after selection:::
        #intervention site::
        new.intervention.mean.a = sum(intervention.selection.a * normal.distribution.trait.1.intervention)/(sum(intervention.selection.a))

        #Calculate the selection differentials::
        intervention.selection.differential.a = (exposure.scaling.factor *((new.intervention.mean.a - intervention.trait.mean))) - female.fitness.cost.intervention

         #calculate the responses::
        response.intervention.a =  heritability * ((intervention.selection.differential.a + male.selection.differential.intervention) / 2)


        intervention.staying.a[[gonotrophic]] = intervention.selection.a

         #egg laying then occurs::

        next.gen.intervention.a.staying[[gonotrophic]] = intervention.trait.mean + response.intervention.a

        #relative egg contributions
        number.intervention.staying.a[[gonotrophic]] = sum(intervention.staying.a[[gonotrophic]])

      }
    }

    #weight the responses by the population sizes (number of females / number of eggs)



    #Prevent the mean resistance from falling below 0. Refugia does not exist but keeep as zero to prevent anything weird happening
    update.refugia.mean = 0


    #and for intervention site:::
    total.egg.output.intervention = sum(unlist(total.laying.intervention))

    intervention.egg.batches = c(unlist(next.gen.intervention.a.staying))
    intervention.batch.sizes = c(unlist(number.intervention.staying.a))

    intervention.weightings = intervention.batch.sizes/total.egg.output.intervention
    individual.batch.means = c(intervention.egg.batches)

    update.intervention.mean = sum((intervention.egg.batches * (intervention.batch.sizes/total.egg.output.intervention)))


    #Prevent the mean resistance from falling below 0.
    update.intervention.mean = ifelse(update.intervention.mean < 0,
                                      yes = 0,
                                      no = update.intervention.mean)


    return(list(update.intervention.mean, update.refugia.mean))
  }


}
