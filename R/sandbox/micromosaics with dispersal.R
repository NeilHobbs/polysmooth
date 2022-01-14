multiple_gonotrophic_cycles_with_dispersal = function(coverage = 0.8,
                                                      natural.survival = 0.8,
                                                      dispersal.rate = 0.2,
                                                      female.exposure = 0.7,
                                                      refugia.trait.mean = 0,
                                                      heritability = 0.3,
                                                      intervention.trait.mean = 10,
                                                      n.cycles = 5,
                                                      male.selection.differential.intervention = 0,
                                                      male.selection.differential.refugia = 0,
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

refugia.selection.differential = list()
intervention.selection.differential = list()

refugia.response = list()
intervention.response = list()

refugia.pop.size = list()
intervention.pop.size = list()



for(gonotrophic in 1:n.cycles){

  if(gonotrophic == 1){
 #Do first gonotrophic cycle::
  #Selection:
intervention.selection.a = (relative.frequency.trait.1.intervention * female.exposure* survival.probability.1.a) + (relative.frequency.trait.1.intervention * natural.survival * (1-female.exposure))
refugia.selection.b = relative.frequency.trait.1.refugia * natural.survival






  #Who then disperse or stay to lay the eggs. Dispersal
intervention.staying.a[[gonotrophic]] = intervention.selection.a * (1-dispersal.rate)
refugia.leaving.b[[gonotrophic]] = refugia.selection.b * dispersal.rate

intervention.leaving.a[[gonotrophic]] = intervention.selection.a * dispersal.rate
refugia.staying.b[[gonotrophic]] = refugia.selection.b * (1-dispersal.rate)


#Mean, Selection Differential, Response, and Pop Size -- Refugia
  #obtain the means
refugia.part.b = sum(refugia.staying.b[[gonotrophic]] * normal.distribution.trait.1.refugia)/(sum(refugia.staying.b[[gonotrophic]]))
refugia.part.a = sum(intervention.leaving.a[[gonotrophic]] * normal.distribution.trait.1.intervention)/(sum(intervention.leaving.a[[gonotrophic]]))

  #weight the contributions
weighted.refugia.part.a = refugia.part.a * (sum(intervention.leaving.a[[gonotrophic]]))/(sum(intervention.leaving.a[[gonotrophic]], refugia.staying.b[[gonotrophic]]))
weighted.refugia.part.b = refugia.part.b * (sum(refugia.staying.b[[gonotrophic]]))/(sum(intervention.leaving.a[[gonotrophic]], refugia.staying.b[[gonotrophic]]))

  #calculate the new mean
refugia.update.mean[[gonotrophic]] = mean(weighted.refugia.part.a, weighted.refugia.part.b)

  #calculate the population size
refugia.pop.size[[gonotrophic]] = sum(refugia.staying.b[[gonotrophic]], intervention.leaving.a[[gonotrophic]])

  #Selection Differential:
refugia.selection.differential[[gonotrophic]] = refugia.update.mean[[gonotrophic]] - refugia.trait.mean

#calculate the response
refugia.response[[gonotrophic]] = exposure.scaling.factor*heritability*((refugia.selection.differential[[gonotrophic]] + male.selection.differential.refugia) / 2)

#Mean, Selection Differential, Response, and Pop Size -- Intervention
#obtain the means
intervention.part.b = sum(refugia.leaving.b[[gonotrophic]] * normal.distribution.trait.1.refugia)/(sum(refugia.leaving.b[[gonotrophic]]))
intervention.part.a = sum(intervention.staying.a[[gonotrophic]] * normal.distribution.trait.1.intervention)/(sum(intervention.staying.a[[gonotrophic]]))

#weight the contributions
weighted.intervention.part.a = intervention.part.a * (sum(intervention.staying.a[[gonotrophic]]))/(sum(refugia.leaving.b[[gonotrophic]], intervention.staying.a[[gonotrophic]]))
weighted.intervention.part.b = intervention.part.b * (sum(refugia.leaving.b[[gonotrophic]]))/(sum(refugia.leaving.b[[gonotrophic]], intervention.staying.a[[gonotrophic]]))

#calculate the new mean
intervention.update.mean[[gonotrophic]] = mean(weighted.intervention.part.a, weighted.intervention.part.b)

#calculate the population size
intervention.pop.size[[gonotrophic]] = sum(intervention.staying.a[[gonotrophic]], refugia.leaving.b[[gonotrophic]])

#Selection Differential:
intervention.selection.differential[[gonotrophic]] = intervention.update.mean[[gonotrophic]] - intervention.trait.mean

#calculate the response
intervention.response[[gonotrophic]] = exposure.scaling.factor*heritability*((intervention.selection.differential[[gonotrophic]] + male.selection.differential.intervention) / 2)

}
if(gonotrophic != 1){

  #Selection
intervention.selection.a = (intervention.staying.a[[gonotrophic-1]] * female.exposure * survival.probability.1.a) + (intervention.staying.a[[gonotrophic-1]] * (1-female.exposure)*natural.survival)
intervention.selection.b = (refugia.leaving.b[[gonotrophic - 1]] * female.exposure * survival.probability.1.b) + (refugia.leaving.b[[gonotrophic - 1]] * (1-female.exposure)*natural.survival)

refugia.selection.a = intervention.leaving.a[[gonotrophic - 1]] * natural.survival
refugia.selection.b = refugia.staying.b[[gonotrophic - 1]] * natural.survival

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




#Mean, Selection Differential, Response, and Pop Size -- Refugia
#obtain the means
refugia.part.b = ((sum(refugia.staying.b[[gonotrophic]] * normal.distribution.trait.1.refugia)/(sum(refugia.staying.b[[gonotrophic]]))) +
                    (sum(intervention.leaving.b[[gonotrophic]] * normal.distribution.trait.1.refugia)/(sum(intervention.leaving.b[[gonotrophic]]))))
refugia.part.a = ((sum(intervention.leaving.a[[gonotrophic]] * normal.distribution.trait.1.intervention)/(sum(intervention.leaving.a[[gonotrophic]]))) +
                    (sum(refugia.staying.a[[gonotrophic]] * normal.distribution.trait.1.intervention)/(sum(refugia.staying.a[[gonotrophic]]))))

#weight the contributions
weighted.refugia.part.a = refugia.part.a * (sum(intervention.leaving.a[[gonotrophic]], refugia.staying.a[[gonotrophic]]))/(sum(intervention.leaving.a[[gonotrophic]], refugia.staying.a[[gonotrophic]],
                                                                                                                               intervention.leaving.b[[gonotrophic]], refugia.staying.b[[gonotrophic]]))

weighted.refugia.part.b = refugia.part.b * (sum(refugia.staying.b[[gonotrophic]], intervention.leaving.b[[gonotrophic]]))/(sum(intervention.leaving.a[[gonotrophic]], refugia.staying.a[[gonotrophic]],
                                                                                             intervention.leaving.b[[gonotrophic]], refugia.staying.b[[gonotrophic]]))

#calculate the new mean
refugia.update.mean[[gonotrophic]] = mean(weighted.refugia.part.a, weighted.refugia.part.b)

#calculate the population size
refugia.pop.size[[gonotrophic]] = sum(intervention.leaving.a[[gonotrophic]], refugia.staying.a[[gonotrophic]],
                                      intervention.leaving.b[[gonotrophic]], refugia.staying.b[[gonotrophic]])

#Selection Differential:
refugia.selection.differential[[gonotrophic]] = refugia.update.mean[[gonotrophic]] - refugia.trait.mean

#calculate the response
refugia.response[[gonotrophic]] = exposure.scaling.factor*heritability*((refugia.selection.differential[[gonotrophic]] + male.selection.differential.refugia) / 2)



#Mean, Selection Differential, Response, and Pop Size -- Intervention
#obtain the means

intervention.part.b = ((sum(intervention.staying.b[[gonotrophic]] * normal.distribution.trait.1.refugia)/(sum(intervention.staying.b[[gonotrophic]]))) +
                    (sum(intervention.leaving.b[[gonotrophic]] * normal.distribution.trait.1.refugia)/(sum(intervention.leaving.b[[gonotrophic]]))))
intervention.part.a = ((sum(intervention.staying.a[[gonotrophic]] * normal.distribution.trait.1.intervention)/(sum(intervention.staying.a[[gonotrophic]]))) +
                    (sum(refugia.leaving.a[[gonotrophic]] * normal.distribution.trait.1.intervention)/(sum(refugia.leaving.a[[gonotrophic]]))))


#weight the contributions
weighted.refugia.part.a = refugia.part.a * (sum(intervention.leaving.a[[gonotrophic]], refugia.staying.a[[gonotrophic]]))/(sum(intervention.leaving.a[[gonotrophic]], refugia.staying.a[[gonotrophic]],
                                                                                                                               intervention.leaving.b[[gonotrophic]], refugia.staying.b[[gonotrophic]]))

weighted.refugia.part.b = refugia.part.b * (sum(refugia.staying.b[[gonotrophic]], intervention.leaving.b[[gonotrophic]]))/(sum(intervention.leaving.a[[gonotrophic]], refugia.staying.a[[gonotrophic]],
                                                                                                                               intervention.leaving.b[[gonotrophic]], refugia.staying.b[[gonotrophic]]))



weighted.intervention.part.a = intervention.part.a * (sum(intervention.staying.a[[gonotrophic]], refugia.leaving.a[[gonotrophic]]))/(sum(intervention.staying.a[[gonotrophic]], refugia.leaving.a[[gonotrophic]],
                                                                                                                                         intervention.staying.b[[gonotrophic]], refugia.leaving.b[[gonotrophic]]))
weighted.intervention.part.b = intervention.part.b * (sum(intervention.staying.b[[gonotrophic]], refugia.leaving.b[[gonotrophic]]))/(sum(intervention.staying.a[[gonotrophic]], refugia.leaving.a[[gonotrophic]],
                                                                                                                                         intervention.staying.b[[gonotrophic]], refugia.leaving.b[[gonotrophic]]))

#calculate the new mean
intervention.update.mean[[gonotrophic]] = mean(weighted.intervention.part.a, weighted.intervention.part.b)

#calculate the population size
intervention.pop.size[[gonotrophic]] = sum(intervention.staying.a[[gonotrophic]], refugia.leaving.a[[gonotrophic]],
                                           intervention.staying.b[[gonotrophic]], refugia.leaving.b[[gonotrophic]])

#Selection Differential:
intervention.selection.differential[[gonotrophic]] = intervention.update.mean[[gonotrophic]] - intervention.trait.mean

#calculate the response
intervention.response[[gonotrophic]] = exposure.scaling.factor*heritability*((intervention.selection.differential[[gonotrophic]] + male.selection.differential.intervention) / 2)
}
  }

  #weight the responses by the population sizes (number of females / number of eggs)

  #refugia
  total.ovipostion.refugia = sum(unlist(refugia.pop.size))
  total.oviposition.intervention = sum(unlist(refugia.pop.size))

  overall.response.refugia = sum(unlist(refugia.response)*unlist(refugia.pop.size)/total.ovipostion.refugia)
  #intervention
  overall.response.intervention = sum(unlist(intervention.response)*unlist(intervention.pop.size)/total.oviposition.intervention)

    return(list(overall.response.intervention, overall.response.refugia))


}
