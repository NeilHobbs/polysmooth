multiple_gonotrophic_cycles_mixture_dispersal_sd_scaled = function(intervention.trait.mean.i,
                                                         intervention.trait.mean.j,
                                                         refugia.trait.mean.i,
                                                         refugia.trait.mean.j,
                                                         standard.deviation,
                                                         vector.length,
                                                         female.exposure,
                                                         exposure.scaling.factor,
                                                         coverage,
                                                         dispersal.rate,
                                                         male.differential.intervention.i,
                                                         male.differential.intervention.j,
                                                         male.differential.refugia.i,
                                                         male.differential.refugia.j,
                                                         female.fitness.cost.i,
                                                         female.fitness.cost.j,
                                                         heritability.i,
                                                         heritability.j,
                                                         n.cycles,
                                                         half.population.bioassay.survival.resistance,
                                                         michaelis.menten.slope ,
                                                         maximum.bioassay.survival.proportion ,
                                                         regression.coefficient,
                                                         regression.intercept,
                                                         current.insecticide.efficacy.i,
                                                         current.insecticide.efficacy.j,
                                                         z.sd.intercept,
                                                         z.sd.coefficient,
                                                         cross.selection.i.j,
                                                         cross.selection.j.i){

  #create the fitness cost selection differentials for females::
  calc.female.fitness.cost.refugia.i  = female.fitness.cost.i * (sd_changes_with_z(current.z = refugia.trait.mean.i,
                                                                                            z.sd.intercept = z.sd.intercept,
                                                                                            z.sd.coefficient = z.sd.coefficient))

  calc.female.fitness.cost.intervention.i = female.fitness.cost.i* (sd_changes_with_z(current.z = intervention.trait.mean.i,
                                                                                                    z.sd.intercept = z.sd.intercept,
                                                                                                    z.sd.coefficient = z.sd.coefficient))


  calc.female.fitness.cost.refugia.j  = female.fitness.cost.j * (sd_changes_with_z(current.z = refugia.trait.mean.j,
                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                              z.sd.coefficient = z.sd.coefficient))

  calc.female.fitness.cost.intervention.j = female.fitness.cost.j * (sd_changes_with_z(current.z = intervention.trait.mean.j,
                                                                                                      z.sd.intercept = z.sd.intercept,
                                                                                                      z.sd.coefficient = z.sd.coefficient))



  #Step 1: create the Normal Distributions:::
  #Insecticide i: emerge in intervention:



  intervention.normal.distribution.i = create_normal_distribution(vector.length = vector.length,
                                                                  trait.mean = intervention.trait.mean.i,
                                                                  standard.deviation = (sd_changes_with_z(current.z = intervention.trait.mean.i,
                                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                                          z.sd.coefficient = z.sd.coefficient)))
  #Insecticide j: emerge in intervention:
  intervention.normal.distribution.j = create_normal_distribution(vector.length = vector.length,
                                                                  trait.mean = intervention.trait.mean.j,
                                                                  standard.deviation = (sd_changes_with_z(current.z = intervention.trait.mean.j,
                                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                                          z.sd.coefficient = z.sd.coefficient)))


  #Insecticide i: emerge in refugia:
  refugia.normal.distribution.i = create_normal_distribution(vector.length = vector.length,
                                                             trait.mean = refugia.trait.mean.i,
                                                             standard.deviation = (sd_changes_with_z(current.z = refugia.trait.mean.i,
                                                                                                     z.sd.intercept = z.sd.intercept,
                                                                                                     z.sd.coefficient = z.sd.coefficient)))
  #Insecticide j: emerge in refugia:
  refugia.normal.distribution.j = create_normal_distribution(vector.length = vector.length,
                                                             trait.mean = refugia.trait.mean.j,
                                                             standard.deviation = (sd_changes_with_z(current.z = refugia.trait.mean.j,
                                                                                                     z.sd.intercept = z.sd.intercept,
                                                                                                     z.sd.coefficient = z.sd.coefficient)))


  #Step 2: Create initial relative frequencies of the normal distibution:
  initial.intervention.densities.i = calculate_density_of_trait_values(vector.length = vector.length,
                                                                       trait.mean = intervention.trait.mean.i,
                                                                       standard.deviation = sd_changes_with_z(current.z = intervention.trait.mean.i,
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient))
  #Insecticide j: emerge in intervention:
  initial.intervention.densities.j = calculate_density_of_trait_values(vector.length = vector.length,
                                                                       trait.mean = intervention.trait.mean.j,
                                                                       standard.deviation = sd_changes_with_z(current.z = intervention.trait.mean.j,
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient))


  #Insecticide i: emerge in refugia:
  initial.refugia.densities.i = calculate_density_of_trait_values(vector.length = vector.length,
                                                                  trait.mean = refugia.trait.mean.i,
                                                                  standard.deviation = sd_changes_with_z(current.z = refugia.trait.mean.i,
                                                                                                         z.sd.intercept = z.sd.intercept,
                                                                                                         z.sd.coefficient = z.sd.coefficient))
  #Insecticide j: emerge in refugia:
  initial.refugia.densities.j = calculate_density_of_trait_values(vector.length = vector.length,
                                                                  trait.mean = refugia.trait.mean.j,
                                                                  standard.deviation = sd_changes_with_z(current.z = refugia.trait.mean.i,
                                                                                                         z.sd.intercept = z.sd.intercept,
                                                                                                         z.sd.coefficient = z.sd.coefficient))


  #Survival probabilities given PRS::
  survival.probability.int.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.normal.distribution.i,
                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                           regression.coefficient = regression.coefficient,
                                                                           regression.intercept = regression.intercept,
                                                                           current.insecticide.efficacy = current.insecticide.efficacy.i)
  #if traits from refugia.
  survival.probability.ref.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = refugia.normal.distribution.i,
                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                           regression.coefficient = regression.coefficient,
                                                                           regression.intercept = regression.intercept,
                                                                           current.insecticide.efficacy = current.insecticide.efficacy.i)


  survival.probability.int.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.normal.distribution.j,
                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                           regression.coefficient = regression.coefficient,
                                                                           regression.intercept = regression.intercept,
                                                                           current.insecticide.efficacy = current.insecticide.efficacy.j)


  survival.probability.ref.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = refugia.normal.distribution.j,
                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                           regression.coefficient = regression.coefficient,
                                                                           regression.intercept = regression.intercept,
                                                                           current.insecticide.efficacy = current.insecticide.efficacy.j)


  ref.staying.ref.i = list()
  ref.joining.int.i = list()
  int.staying.int.i = list()
  int.joining.ref.i = list()
  ref.number.in.ref.i = list()
  int.number.in.ref.i = list()
  int.number.in.int.i = list()
  ref.number.in.int.i = list()
  int.in.int.differential.i = list()
  int.in.ref.differential.i = list()
  ref.in.ref.differential.i = list()
  ref.in.int.differential.i = list()
  ref.response.in.ref.i = list()
  ref.response.in.int.i = list()
  int.response.in.int.i = list()
  int.response.in.ref.i = list()

  ref.staying.ref.j = list()
  ref.joining.int.j = list()
  int.staying.int.j = list()
  int.joining.ref.j  = list()
  ref.number.in.ref.j = list()
  int.number.in.ref.j = list()
  int.number.in.int.j = list()
  ref.number.in.int.j = list()
  int.in.int.differential.j = list()
  int.in.ref.differential.j = list()
  ref.in.ref.differential.j = list()
  ref.in.int.differential.j = list()
  ref.response.in.ref.j = list()
  ref.response.in.int.j = list()
  int.response.in.int.j = list()
  int.response.in.ref.j = list()


  if(coverage < 1){
    for(gonotrophic in 1:n.cycles){
      if(gonotrophic == 1){

        mean.survival.int.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.trait.mean.j,
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.j)


        ref.staying.ref.i[[gonotrophic]] = initial.refugia.densities.i * (1-coverage)*(1-dispersal.rate)
        ref.joining.int.i[[gonotrophic]]  = initial.refugia.densities.i * (1-coverage) * dispersal.rate

        int.staying.int.i[[gonotrophic]]  = (initial.intervention.densities.i * female.exposure * survival.probability.int.i * mean.survival.int.j * coverage * (1-dispersal.rate)) + (initial.intervention.densities.i * (1-female.exposure) * coverage * (1-dispersal.rate))
        int.joining.ref.i[[gonotrophic]]  = (initial.intervention.densities.i * female.exposure * survival.probability.int.i * mean.survival.int.j * coverage * dispersal.rate) + (initial.intervention.densities.i * (1-female.exposure) * coverage * dispersal.rate)



        ref.number.in.ref.i[[gonotrophic]]  = sum(ref.staying.ref.i[[gonotrophic]])
        int.number.in.ref.i[[gonotrophic]]  = sum(int.joining.ref.i[[gonotrophic]])

        int.number.in.int.i[[gonotrophic]]  = sum(int.staying.int.i[[gonotrophic]])
        ref.number.in.int.i[[gonotrophic]]  = sum(ref.joining.int.i[[gonotrophic]])

        int.in.int.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.i[[gonotrophic]] *intervention.normal.distribution.i) / int.number.in.int.i[[gonotrophic]] ) - intervention.trait.mean.i) - female.fitness.cost.i
        int.in.ref.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.joining.ref.i[[gonotrophic]] *intervention.normal.distribution.i) / int.number.in.ref.i[[gonotrophic]] ) - intervention.trait.mean.i) - female.fitness.cost.i

        ref.in.ref.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.staying.ref.i[[gonotrophic]] *refugia.normal.distribution.i) / ref.number.in.ref.i[[gonotrophic]] ) - refugia.trait.mean.i) - female.fitness.cost.i
        ref.in.int.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.joining.int.i[[gonotrophic]] *refugia.normal.distribution.i) / ref.number.in.int.i[[gonotrophic]] ) - refugia.trait.mean.i) - female.fitness.cost.i

        ref.response.in.ref.i[[gonotrophic]]  = heritability.i * ((ref.in.ref.differential.i[[gonotrophic]]  + male.differential.refugia.i) / 2)
        ref.response.in.int.i[[gonotrophic]]  = heritability.i * ((ref.in.int.differential.i[[gonotrophic]]  + male.differential.refugia.i) / 2)
        int.response.in.int.i[[gonotrophic]]  = heritability.i * ((int.in.int.differential.i[[gonotrophic]]  + male.differential.intervention.i) / 2)
        int.response.in.ref.i[[gonotrophic]]  = heritability.i * ((int.in.ref.differential.i[[gonotrophic]]  + male.differential.intervention.i) / 2)


        mean.survival.int.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.trait.mean.i,
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.i)


        ref.staying.ref.j[[gonotrophic]] = initial.refugia.densities.j * (1-coverage)*(1-dispersal.rate)
        ref.joining.int.j[[gonotrophic]]  = initial.refugia.densities.j * (1-coverage) * dispersal.rate

        int.staying.int.j[[gonotrophic]]  = (initial.intervention.densities.j * female.exposure * survival.probability.int.j * mean.survival.int.i * coverage * (1-dispersal.rate)) + (initial.intervention.densities.j * (1-female.exposure) * coverage * (1-dispersal.rate))
        int.joining.ref.j[[gonotrophic]]  = (initial.intervention.densities.j * female.exposure * survival.probability.int.j * mean.survival.int.i * coverage * dispersal.rate) + (initial.intervention.densities.j * (1-female.exposure) * coverage * dispersal.rate)



        ref.number.in.ref.j[[gonotrophic]]  = sum(ref.staying.ref.j[[gonotrophic]] )
        int.number.in.ref.j[[gonotrophic]]  = sum(int.joining.ref.j[[gonotrophic]] )

        int.number.in.int.j[[gonotrophic]]  = sum(int.staying.int.j[[gonotrophic]] )
        ref.number.in.int.j[[gonotrophic]]  = sum(ref.joining.int.j[[gonotrophic]] )

        int.in.int.differential.j[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.j[[gonotrophic]] *intervention.normal.distribution.j) / int.number.in.int.j[[gonotrophic]] ) - intervention.trait.mean.j) - female.fitness.cost.j
        int.in.ref.differential.j[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.joining.ref.j[[gonotrophic]] *intervention.normal.distribution.j) / int.number.in.ref.j[[gonotrophic]] ) - intervention.trait.mean.j) - female.fitness.cost.j

        ref.in.ref.differential.j[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.staying.ref.j[[gonotrophic]] *refugia.normal.distribution.j) / ref.number.in.ref.j[[gonotrophic]] ) - refugia.trait.mean.j) - female.fitness.cost.j
        ref.in.int.differential.j[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.joining.int.j[[gonotrophic]] *refugia.normal.distribution.j) / ref.number.in.int.j[[gonotrophic]] ) - refugia.trait.mean.j) - female.fitness.cost.j

        ref.response.in.ref.j[[gonotrophic]]  = heritability.j * ((ref.in.ref.differential.j[[gonotrophic]]  + male.differential.refugia.j) / 2)
        ref.response.in.int.j[[gonotrophic]]  = heritability.j * ((ref.in.int.differential.j[[gonotrophic]]  + male.differential.refugia.j) / 2)
        int.response.in.int.j[[gonotrophic]]  = heritability.j * ((int.in.int.differential.j[[gonotrophic]]  + male.differential.intervention.j) / 2)
        int.response.in.ref.j[[gonotrophic]]  = heritability.j * ((int.in.ref.differential.j[[gonotrophic]]  + male.differential.intervention.j) / 2)

      }

      if(gonotrophic != 1){

        mean.survival.int.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean =  (sum(int.staying.int.j[[gonotrophic-1]] * intervention.normal.distribution.j)/(sum(int.staying.int.j[[gonotrophic-1]]))),
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.j)

        mean.survival.ref.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean =  sum((ref.joining.int.j[[gonotrophic-1]] * refugia.normal.distribution.j))/(sum(int.staying.int.j[[gonotrophic-1]])),
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.j)




        ref.staying.ref.i[[gonotrophic]] = (ref.staying.ref.i[[gonotrophic-1]] *(1-dispersal.rate)) +
          (ref.joining.int.i[[gonotrophic-1]] * (1-female.exposure)*dispersal.rate) +
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * dispersal.rate * mean.survival.ref.j * survival.probability.ref.i)

        ref.joining.int.i[[gonotrophic]]  = (ref.joining.int.i[[gonotrophic-1]] * (1-female.exposure)*(1-dispersal.rate)) +
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.ref.j * survival.probability.ref.i) +
          (ref.staying.ref.i[[gonotrophic-1]] * dispersal.rate)


        int.staying.int.i[[gonotrophic]]  = (int.staying.int.i[[gonotrophic-1]] * (1-female.exposure) * (1-dispersal.rate)) +
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.int.j * survival.probability.int.i)+
          (int.joining.ref.i[[gonotrophic-1]] * dispersal.rate)

        int.joining.ref.i[[gonotrophic]]  = (int.joining.ref.i[[gonotrophic-1]] * (1-dispersal.rate))+
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * dispersal.rate * mean.survival.int.j * survival.probability.int.i)+
          (int.staying.int.i[[gonotrophic-1]] * (1-female.exposure) * dispersal.rate)



        ref.number.in.ref.i[[gonotrophic]]  = sum(ref.staying.ref.i[[gonotrophic]] )
        int.number.in.ref.i[[gonotrophic]]  = sum(int.joining.ref.i[[gonotrophic]] )

        int.number.in.int.i[[gonotrophic]]  = sum(int.staying.int.i[[gonotrophic]] )
        ref.number.in.int.i[[gonotrophic]]  = sum(ref.joining.int.i[[gonotrophic]] )

        int.in.int.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.i[[gonotrophic]] *intervention.normal.distribution.i) / int.number.in.int.i[[gonotrophic]] ) - intervention.trait.mean.i) - female.fitness.cost.i
        int.in.ref.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.joining.ref.i[[gonotrophic]] *intervention.normal.distribution.i) / int.number.in.ref.i[[gonotrophic]] ) - intervention.trait.mean.i) - female.fitness.cost.i

        ref.in.ref.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.staying.ref.i[[gonotrophic]] *refugia.normal.distribution.i) / ref.number.in.ref.i[[gonotrophic]] ) - refugia.trait.mean.i) - female.fitness.cost.i
        ref.in.int.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.joining.int.i[[gonotrophic]] *refugia.normal.distribution.i) / ref.number.in.int.i[[gonotrophic]] ) - refugia.trait.mean.i) - female.fitness.cost.i

        ref.response.in.ref.i[[gonotrophic]]  = heritability.i * ((ref.in.ref.differential.i[[gonotrophic]]  + male.differential.refugia.i) / 2)
        ref.response.in.int.i[[gonotrophic]]  = heritability.i * ((ref.in.int.differential.i[[gonotrophic]]  + male.differential.refugia.i) / 2)
        int.response.in.int.i[[gonotrophic]]  = heritability.i * ((int.in.int.differential.i[[gonotrophic]]  + male.differential.intervention.i) / 2)
        int.response.in.ref.i[[gonotrophic]]  = heritability.i * ((int.in.ref.differential.i[[gonotrophic]]  + male.differential.intervention.i) / 2)



        mean.survival.int.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean =  sum((int.staying.int.i[[gonotrophic-1]] * intervention.normal.distribution.i))/(sum(int.staying.int.i[[gonotrophic-1]])),
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.i)

        mean.survival.ref.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean =  (ref.joining.int.i[[gonotrophic-1]] * refugia.normal.distribution.i)/(sum(int.staying.int.i[[gonotrophic-1]])),
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.i)




        ref.staying.ref.j[[gonotrophic]] = (ref.staying.ref.j[[gonotrophic-1]] *(1-dispersal.rate)) +
          (ref.joining.int.j[[gonotrophic-1]] * (1-female.exposure)*dispersal.rate) +
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * dispersal.rate * mean.survival.ref.i * survival.probability.ref.j)

        ref.joining.int.j[[gonotrophic]]  = (ref.joining.int.j[[gonotrophic-1]] * (1-female.exposure)*(1-dispersal.rate)) +
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.ref.i * survival.probability.ref.j) +
          (ref.staying.ref.j[[gonotrophic-1]] * dispersal.rate)


        int.staying.int.j[[gonotrophic]]  = (int.staying.int.j[[gonotrophic-1]] * (1-female.exposure) * (1-dispersal.rate))+
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.int.i * survival.probability.int.j)+
          (int.joining.ref.j[[gonotrophic-1]] * dispersal.rate)

        int.joining.ref.j[[gonotrophic]]  = (int.joining.ref.j[[gonotrophic-1]] * (1-dispersal.rate))+
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * dispersal.rate * mean.survival.int.i * survival.probability.int.j)+
          (int.staying.int.j[[gonotrophic-1]] * (1-female.exposure) * dispersal.rate)



        ref.number.in.ref.j[[gonotrophic]]  = sum(ref.staying.ref.j[[gonotrophic]] )
        int.number.in.ref.j[[gonotrophic]]  = sum(int.joining.ref.j[[gonotrophic]] )

        int.number.in.int.j[[gonotrophic]]  = sum(int.staying.int.j[[gonotrophic]] )
        ref.number.in.int.j[[gonotrophic]]  = sum(ref.joining.int.j[[gonotrophic]] )

        int.in.int.differential.j[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.j[[gonotrophic]] *intervention.normal.distribution.j) / int.number.in.int.j[[gonotrophic]] ) - intervention.trait.mean.j) - female.fitness.cost.j
        int.in.ref.differential.j[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.joining.ref.j[[gonotrophic]] *intervention.normal.distribution.j) / int.number.in.ref.j[[gonotrophic]] ) - intervention.trait.mean.j) - female.fitness.cost.j

        ref.in.ref.differential.j[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.staying.ref.j[[gonotrophic]] *refugia.normal.distribution.j) / ref.number.in.ref.j[[gonotrophic]] ) - refugia.trait.mean.j) - female.fitness.cost.j
        ref.in.int.differential.j[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.joining.int.j[[gonotrophic]] *refugia.normal.distribution.j) / ref.number.in.int.j[[gonotrophic]] ) - refugia.trait.mean.j) - female.fitness.cost.j

        ref.response.in.ref.j[[gonotrophic]]  = heritability.j * ((ref.in.ref.differential.j[[gonotrophic]]  + male.differential.refugia.j) / 2)
        ref.response.in.int.j[[gonotrophic]]  = heritability.j * ((ref.in.int.differential.j[[gonotrophic]]  + male.differential.refugia.j) / 2)
        int.response.in.int.j[[gonotrophic]]  = heritability.j * ((int.in.int.differential.j[[gonotrophic]]  + male.differential.intervention.j) / 2)
        int.response.in.ref.j[[gonotrophic]]  = heritability.j * ((int.in.ref.differential.j[[gonotrophic]]  + male.differential.intervention.j) / 2)

      }
    }
    #total eggs laid for Trait i in refugia
    N.total.ref.i = sum(unlist(ref.number.in.ref.i), unlist(int.number.in.ref.i))

    #of which from "refugia" females:
    N.ref.in.ref.i = sum(unlist(ref.number.in.ref.i))

    #of which from "intervention" females:
    N.int.in.ref.i = sum(unlist(int.number.in.ref.i))

    av.ref.in.ref.response.i = sum(((unlist(ref.response.in.ref.i) + (cross.selection.j.i * unlist(ref.response.in.ref.j)))* (unlist(ref.number.in.ref.i)/N.ref.in.ref.i)))
    av.int.in.ref.response.i = sum(((unlist(int.response.in.ref.i) + (cross.selection.j.i * unlist(int.response.in.ref.j)))* (unlist(int.number.in.ref.i)/N.int.in.ref.i)))

    final.ref.mean.i = ((N.ref.in.ref.i * (refugia.trait.mean.i + av.ref.in.ref.response.i)) +
                          (N.int.in.ref.i * (intervention.trait.mean.i + av.int.in.ref.response.i)))/(N.total.ref.i)


    #total eggs laid for Trait i in intervention
    N.total.int.i = sum(unlist(int.number.in.int.i), unlist(ref.number.in.int.i))

    #of which from "refugia" females:
    N.ref.in.int.i = sum(unlist(ref.number.in.int.i))

    #of which from "intervention" females:
    N.int.in.int.i = sum(unlist(int.number.in.int.i))

    av.int.in.int.response.i = sum(((unlist(int.response.in.int.i) + (cross.selection.j.i * unlist(int.response.in.int.j))) * (unlist(int.number.in.int.i)/N.int.in.int.i)))
    av.ref.in.int.response.i = sum(((unlist(ref.response.in.int.i) + (cross.selection.j.i * unlist(ref.response.in.int.j))) * (unlist(ref.number.in.int.i)/N.ref.in.int.i)))

    final.int.mean.i = ((N.int.in.int.i * (intervention.trait.mean.i + av.int.in.int.response.i)) +
                          (N.ref.in.int.i * (refugia.trait.mean.i + av.ref.in.int.response.i)))/(N.total.int.i)



    ###Repeat for Trait j:::
    #total eggs laid for Trait i in refugia
    N.total.ref.j = sum(unlist(ref.number.in.ref.j), unlist(int.number.in.ref.j))

    #of which from "refugia" females:
    N.ref.in.ref.j = sum(unlist(ref.number.in.ref.j))

    #of which from "intervention" females:
    N.int.in.ref.j = sum(unlist(int.number.in.ref.j))

    av.ref.in.ref.response.j = sum(((unlist(ref.response.in.ref.j) + (cross.selection.i.j * unlist(ref.response.in.ref.i)))* (unlist(ref.number.in.ref.j)/N.ref.in.ref.j)))
    av.int.in.ref.response.j = sum(((unlist(int.response.in.ref.j) + (cross.selection.i.j * unlist(int.response.in.ref.i)))* (unlist(int.number.in.ref.i)/N.int.in.ref.j)))

    final.ref.mean.j = ((N.ref.in.ref.j * (refugia.trait.mean.j + av.ref.in.ref.response.j)) +
                          (N.int.in.ref.j * (intervention.trait.mean.j + av.int.in.ref.response.j)))/(N.total.ref.j)


    #total eggs laid for Trait i in intervention
    N.total.int.j = sum(unlist(int.number.in.int.j), unlist(ref.number.in.int.j))

    #of which from "refugia" females:
    N.ref.in.int.j = sum(unlist(ref.number.in.int.j))

    #of which from "intervention" females:
    N.int.in.int.j = sum(unlist(int.number.in.int.j))

    av.int.in.int.response.j = sum(((unlist(int.response.in.int.j) + (cross.selection.i.j * unlist(int.response.in.int.i)))* (unlist(int.number.in.int.j)/N.int.in.int.j)))
    av.ref.in.int.response.j = sum(((unlist(ref.response.in.int.j) + (cross.selection.i.j * unlist(ref.response.in.int.i)))* (unlist(ref.number.in.int.j)/N.ref.in.int.j)))


    final.int.mean.j = ((N.int.in.int.j * (intervention.trait.mean.j + av.int.in.int.response.j)) +
                          (N.ref.in.int.j * (refugia.trait.mean.j + av.ref.in.int.response.j)))/(N.total.int.j)


    #prevent mean PRS values falling below 0:::

    final.ref.mean.i = ifelse(final.ref.mean.i < 0,
                              yes = 0,
                              no = final.ref.mean.i)

    final.int.mean.i = ifelse(final.int.mean.i < 0,
                              yes = 0,
                              no = final.int.mean.i)

    final.ref.mean.j = ifelse(final.ref.mean.j < 0,
                              yes = 0,
                              no = final.ref.mean.j)

    final.int.mean.j = ifelse(final.int.mean.j < 0,
                              yes = 0,
                              no = final.int.mean.j)


    return(list(final.int.mean.i, final.ref.mean.i, final.int.mean.j, final.ref.mean.j))
  }

  if(coverage == 1 | dispersal.rate == 0){
    for(gonotrophic in 1:n.cycles){
      if(gonotrophic == 1){

        mean.survival.int.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.trait.mean.j,
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.j)



        int.staying.int.i[[gonotrophic]]  = (initial.intervention.densities.i * female.exposure * survival.probability.int.i * mean.survival.int.j * coverage) + (initial.intervention.densities.i * (1-female.exposure) * coverage)

        int.number.in.int.i[[gonotrophic]]  = sum(int.staying.int.i[[gonotrophic]])

        int.in.int.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.i[[gonotrophic]] *intervention.normal.distribution.i) / int.number.in.int.i[[gonotrophic]] ) - intervention.trait.mean.i) - female.fitness.cost.i

        int.response.in.int.i[[gonotrophic]]  = heritability.i * ((int.in.int.differential.i[[gonotrophic]]  + male.differential.intervention.i) / 2)

        mean.survival.int.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.trait.mean.i,
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.i)



        int.staying.int.j[[gonotrophic]]  = (initial.intervention.densities.j * female.exposure * survival.probability.int.j * mean.survival.int.i * coverage) + (initial.intervention.densities.j * (1-female.exposure) * coverage)

        int.number.in.int.j[[gonotrophic]]  = sum(int.staying.int.j[[gonotrophic]] )

        int.in.int.differential.j[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.j[[gonotrophic]] *intervention.normal.distribution.j) / int.number.in.int.j[[gonotrophic]] ) - intervention.trait.mean.j) - female.fitness.cost.j


        int.response.in.int.j[[gonotrophic]]  = heritability.j * ((int.in.int.differential.j[[gonotrophic]]  + male.differential.intervention.j) / 2)
      }

      if(gonotrophic != 1){

        mean.survival.int.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean =  (sum(int.staying.int.j[[gonotrophic-1]] * intervention.normal.distribution.j)/(sum(int.staying.int.j[[gonotrophic-1]]))),
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.j)



        int.staying.int.i[[gonotrophic]]  = (int.staying.int.i[[gonotrophic-1]] * (1-female.exposure)) +
          (int.staying.int.i[[gonotrophic-1]] * female.exposure  * mean.survival.int.j * survival.probability.int.i)


        int.number.in.int.i[[gonotrophic]]  = sum(int.staying.int.i[[gonotrophic]] )

        int.in.int.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.i[[gonotrophic]] *intervention.normal.distribution.i) / int.number.in.int.i[[gonotrophic]] ) - intervention.trait.mean.i) - female.fitness.cost.i

        int.response.in.int.i[[gonotrophic]]  = heritability.i * ((int.in.int.differential.i[[gonotrophic]]  + male.differential.intervention.i) / 2)

        mean.survival.int.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean =  sum((int.staying.int.i[[gonotrophic-1]] * intervention.normal.distribution.i))/(sum(int.staying.int.i[[gonotrophic-1]])),
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.i)



        int.staying.int.j[[gonotrophic]]  = (int.staying.int.j[[gonotrophic-1]] * (1-female.exposure))+
          (int.staying.int.j[[gonotrophic-1]] * female.exposure  * mean.survival.int.i * survival.probability.int.j)


        int.number.in.int.j[[gonotrophic]]  = sum(int.staying.int.j[[gonotrophic]] )


        int.in.int.differential.j[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.j[[gonotrophic]] *intervention.normal.distribution.j) / int.number.in.int.j[[gonotrophic]] ) - intervention.trait.mean.j) - female.fitness.cost.j

        int.response.in.int.j[[gonotrophic]]  = heritability.j * ((int.in.int.differential.j[[gonotrophic]]  + male.differential.intervention.j) / 2)
      }
    }

    final.ref.mean.i = 0

    #total eggs laid for Trait i in intervention
    N.total.int.i = sum(unlist(int.number.in.int.i))

    #of which from "intervention" females:
    N.int.in.int.i = sum(unlist(int.number.in.int.i))

    av.int.in.int.response.i = sum(((unlist(int.response.in.int.i) + (cross.selection.j.i * unlist(int.response.in.int.j)))* (unlist(int.number.in.int.i)/N.int.in.int.i)))

    final.int.mean.i = intervention.trait.mean.i + av.int.in.int.response.i


    ###Repeat for Trait j:::

    final.ref.mean.j = 0

    #total eggs laid for Trait i in intervention
    N.total.int.j = sum(unlist(int.number.in.int.j))

    #of which from "intervention" females:
    N.int.in.int.j = sum(unlist(int.number.in.int.j))

    av.int.in.int.response.j = sum(((unlist(int.response.in.int.j) + (cross.selection.i.j * unlist(int.response.in.int.i)))* (unlist(int.number.in.int.j)/N.int.in.int.j)))

    final.int.mean.j = intervention.trait.mean.j + av.int.in.int.response.j

    #prevent mean PRS values falling below 0:::

    final.ref.mean.i = ifelse(final.ref.mean.i < 0,
                              yes = 0,
                              no = final.ref.mean.i)

    final.int.mean.i = ifelse(final.int.mean.i < 0,
                              yes = 0,
                              no = final.int.mean.i)

    final.ref.mean.j = ifelse(final.ref.mean.j < 0,
                              yes = 0,
                              no = final.ref.mean.j)

    final.int.mean.j = ifelse(final.int.mean.j < 0,
                              yes = 0,
                              no = final.int.mean.j)


    return(list(final.int.mean.i, final.ref.mean.i, final.int.mean.j, final.ref.mean.j))
  }

}
