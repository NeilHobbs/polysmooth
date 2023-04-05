multiple_gonotrophic_cycles_combinations_dispersal_not_deployed = function(intervention.trait.mean.i,
                                                                           intervention.trait.mean.j,
                                                                           intervention.trait.mean.tracked,
                                                                           refugia.trait.mean.i,
                                                                           refugia.trait.mean.j,
                                                                           refugia.trait.mean.tracked,
                                                                           standard.deviation,
                                                                           vector.length,
                                                                           female.exposure,
                                                                           exposure.scaling.factor,
                                                                           coverage,
                                                                           dispersal.rate,
                                                                           male.differential.intervention.i,
                                                                           male.differential.intervention.j,
                                                                           male.differential.intervention.tracked,
                                                                           male.differential.refugia.i,
                                                                           male.differential.refugia.j,
                                                                           male.differential.refugia.tracked,
                                                                           female.fitness.cost.i,
                                                                           female.fitness.cost.j,
                                                                           female.fitness.cost.tracked,
                                                                           heritability.i,
                                                                           heritability.j,
                                                                           heritability.tracked,
                                                                           n.cycles,
                                                                           half.population.bioassay.survival.resistance,
                                                                           michaelis.menten.slope ,
                                                                           maximum.bioassay.survival.proportion ,
                                                                           regression.coefficient,
                                                                           regression.intercept,
                                                                           current.insecticide.efficacy.i,
                                                                           current.insecticide.efficacy.j,
                                                                           probability.only.i,
                                                                           probability.only.j,
                                                                           probability.both.i.j,
                                                                           coverage.i,
                                                                           coverage.j,
                                                                           coverage.ij,
                                                                           cross.selection.i.k,
                                                                           cross.selection.j.k,
                                                                           between.gonotrophic.survival)
{


  #Step 1: create the Normal Distributions:::
  #Insecticide i: emerge in intervention:
  intervention.normal.distribution.i = create_normal_distribution(vector.length = vector.length,
                                                                  trait.mean = intervention.trait.mean.i,
                                                                  standard.deviation = standard.deviation)
  #Insecticide j: emerge in intervention:
  intervention.normal.distribution.j = create_normal_distribution(vector.length = vector.length,
                                                                  trait.mean = intervention.trait.mean.j,
                                                                  standard.deviation = standard.deviation)


  intervention.normal.distribution.tracked = create_normal_distribution(vector.length = vector.length,
                                                                  trait.mean = intervention.trait.mean.tracked,
                                                                  standard.deviation = standard.deviation)


  #Insecticide i: emerge in refugia:
  refugia.normal.distribution.i = create_normal_distribution(vector.length = vector.length,
                                                             trait.mean = refugia.trait.mean.i,
                                                             standard.deviation = standard.deviation)
  #Insecticide j: emerge in refugia:
  refugia.normal.distribution.j = create_normal_distribution(vector.length = vector.length,
                                                             trait.mean = refugia.trait.mean.j,
                                                             standard.deviation = standard.deviation)

  refugia.normal.distribution.tracked = create_normal_distribution(vector.length = vector.length,
                                                             trait.mean = refugia.trait.mean.tracked,
                                                             standard.deviation = standard.deviation)
  #Step 2: Create initial relative frequencies of the normal distibution:
  initial.intervention.densities.i = calculate_density_of_trait_values(vector.length = vector.length,
                                                                       trait.mean = intervention.trait.mean.i,
                                                                       standard.deviation = standard.deviation)
  #Insecticide j: emerge in intervention:
  initial.intervention.densities.j = calculate_density_of_trait_values(vector.length = vector.length,
                                                                       trait.mean = intervention.trait.mean.j,
                                                                       standard.deviation = standard.deviation)

  initial.intervention.densities.tracked = calculate_density_of_trait_values(vector.length = vector.length,
                                                                       trait.mean = intervention.trait.mean.tracked,
                                                                       standard.deviation = standard.deviation)
  #Insecticide i: emerge in refugia:
  initial.refugia.densities.i = calculate_density_of_trait_values(vector.length = vector.length,
                                                                  trait.mean = refugia.trait.mean.i,
                                                                  standard.deviation = standard.deviation)
  #Insecticide j: emerge in refugia:
  initial.refugia.densities.j = calculate_density_of_trait_values(vector.length = vector.length,
                                                                  trait.mean = refugia.trait.mean.j,
                                                                  standard.deviation = standard.deviation)

  initial.refugia.densities.tracked = calculate_density_of_trait_values(vector.length = vector.length,
                                                                  trait.mean = refugia.trait.mean.tracked,
                                                                  standard.deviation = standard.deviation)
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

  ref.staying.ref.tracked = list()
  ref.joining.int.tracked = list()
  int.staying.int.tracked = list()
  int.joining.ref.tracked = list()
  ref.number.in.ref.tracked = list()
  int.number.in.ref.tracked = list()
  int.number.in.int.tracked = list()
  ref.number.in.int.tracked = list()
  int.in.int.differential.tracked = list()
  int.in.ref.differential.tracked = list()
  ref.in.ref.differential.tracked = list()
  ref.in.int.differential.tracked = list()
  ref.response.in.ref.tracked = list()
  ref.response.in.int.tracked = list()
  int.response.in.int.tracked = list()
  int.response.in.ref.tracked = list()

  if(coverage < 1){

        mean.survival.int.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.trait.mean.j,
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.j)


        ref.staying.ref.i[[1]] = initial.refugia.densities.i * (1-coverage)*(1-dispersal.rate)
        ref.joining.int.i[[1]]  = initial.refugia.densities.i * (1-coverage) * dispersal.rate

        int.staying.int.i[[1]]  = (initial.intervention.densities.i * female.exposure * survival.probability.int.i * coverage.i * coverage * (1-dispersal.rate)) +
          (initial.intervention.densities.i * female.exposure * coverage.j * mean.survival.int.j * coverage * (1-dispersal.rate)) +
          (initial.intervention.densities.i * female.exposure * coverage.ij  * probability.only.i * survival.probability.int.i * coverage * (1-dispersal.rate)) + #i only
          (initial.intervention.densities.i * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage * (1-dispersal.rate)) + #j only
          (initial.intervention.densities.i * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.j * survival.probability.int.i * coverage * (1-dispersal.rate)) + #both ij
          (initial.intervention.densities.i * (1-female.exposure) * coverage * (1-dispersal.rate))


        int.joining.ref.i[[1]]  = (initial.intervention.densities.i * female.exposure * survival.probability.int.i  * coverage * coverage.i * dispersal.rate) +
          (initial.intervention.densities.i * female.exposure * mean.survival.int.j * coverage * coverage.j* dispersal.rate) +
          (initial.intervention.densities.i * female.exposure * coverage.ij  * probability.only.i * survival.probability.int.i * coverage * dispersal.rate) + #i only
          (initial.intervention.densities.i * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage * dispersal.rate) + #j only
          (initial.intervention.densities.i * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.j * survival.probability.int.i * coverage * dispersal.rate) + #both ij
          (initial.intervention.densities.i * (1-female.exposure) * coverage * dispersal.rate)



        ref.number.in.ref.i[[1]]  = sum(ref.staying.ref.i[[1]] )
        int.number.in.ref.i[[1]]  = sum(int.joining.ref.i[[1]] )

        int.number.in.int.i[[1]]  = sum(int.staying.int.i[[1]] )
        ref.number.in.int.i[[1]]  = sum(ref.joining.int.i[[1]] )

        int.in.int.differential.i[[1]]  = exposure.scaling.factor * ((sum(int.staying.int.i[[1]] *intervention.normal.distribution.i) / int.number.in.int.i[[1]] ) - intervention.trait.mean.i) - female.fitness.cost.i
        int.in.ref.differential.i[[1]]  = exposure.scaling.factor * ((sum(int.joining.ref.i[[1]] *intervention.normal.distribution.i) / int.number.in.ref.i[[1]] ) - intervention.trait.mean.i) - female.fitness.cost.i

        ref.in.ref.differential.i[[1]]  = exposure.scaling.factor * ((sum(ref.staying.ref.i[[1]] *refugia.normal.distribution.i) / ref.number.in.ref.i[[1]] ) - refugia.trait.mean.i) - female.fitness.cost.i
        ref.in.int.differential.i[[1]]  = exposure.scaling.factor * ((sum(ref.joining.int.i[[1]] *refugia.normal.distribution.i) / ref.number.in.int.i[[1]] ) - refugia.trait.mean.i) - female.fitness.cost.i

        ref.response.in.ref.i[[1]]  = heritability.i * ((ref.in.ref.differential.i[[1]]  + male.differential.refugia.i) / 2)
        ref.response.in.int.i[[1]]  = heritability.i * ((ref.in.int.differential.i[[1]]  + male.differential.refugia.i) / 2)
        int.response.in.int.i[[1]]  = heritability.i * ((int.in.int.differential.i[[1]]  + male.differential.intervention.i) / 2)
        int.response.in.ref.i[[1]]  = heritability.i * ((int.in.ref.differential.i[[1]]  + male.differential.intervention.i) / 2)


        mean.survival.int.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.trait.mean.i,
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.i)


        ref.staying.ref.j[[1]] = initial.refugia.densities.j * (1-coverage)*(1-dispersal.rate)
        ref.joining.int.j[[1]]  = initial.refugia.densities.j * (1-coverage) * dispersal.rate

        int.staying.int.j[[1]]  = (initial.intervention.densities.j * female.exposure * survival.probability.int.j * coverage.j * coverage * (1-dispersal.rate)) +
          (initial.intervention.densities.j * female.exposure * coverage.i * mean.survival.int.i * coverage * (1-dispersal.rate)) +
          (initial.intervention.densities.j * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage * (1-dispersal.rate)) + #i only
          (initial.intervention.densities.j * female.exposure * coverage.ij  * probability.only.j * survival.probability.int.j * coverage * (1-dispersal.rate)) + #j only
          (initial.intervention.densities.j * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * survival.probability.int.j * coverage * (1-dispersal.rate)) + #both ij
          (initial.intervention.densities.j * (1-female.exposure) * coverage * (1-dispersal.rate))


        int.joining.ref.j[[1]]  = (initial.intervention.densities.j * female.exposure * mean.survival.int.i * coverage *coverage.i* dispersal.rate) +
          (initial.intervention.densities.j * female.exposure * survival.probability.int.j * coverage.j * coverage * dispersal.rate) +
          (initial.intervention.densities.j * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage * dispersal.rate) + #i only
          (initial.intervention.densities.j * female.exposure * coverage.ij  * probability.only.j * survival.probability.int.j * coverage * dispersal.rate) + #j only
          (initial.intervention.densities.j * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * survival.probability.int.j * coverage * dispersal.rate) + #both ij
          (initial.intervention.densities.j * (1-female.exposure) * coverage * dispersal.rate)



        ref.number.in.ref.j[[1]]  = sum(ref.staying.ref.j[[1]] )
        int.number.in.ref.j[[1]]  = sum(int.joining.ref.j[[1]] )

        int.number.in.int.j[[1]]  = sum(int.staying.int.j[[1]] )
        ref.number.in.int.j[[1]]  = sum(ref.joining.int.j[[1]] )

        int.in.int.differential.j[[1]]  = exposure.scaling.factor * ((sum(int.staying.int.j[[1]] *intervention.normal.distribution.j) / int.number.in.int.j[[1]] ) - intervention.trait.mean.j) - female.fitness.cost.j
        int.in.ref.differential.j[[1]]  = exposure.scaling.factor * ((sum(int.joining.ref.j[[1]] *intervention.normal.distribution.j) / int.number.in.ref.j[[1]] ) - intervention.trait.mean.j) - female.fitness.cost.j

        ref.in.ref.differential.j[[1]]  = exposure.scaling.factor * ((sum(ref.staying.ref.j[[1]] *refugia.normal.distribution.j) / ref.number.in.ref.j[[1]] ) - refugia.trait.mean.j) - female.fitness.cost.j
        ref.in.int.differential.j[[1]]  = exposure.scaling.factor * ((sum(ref.joining.int.j[[1]] *refugia.normal.distribution.j) / ref.number.in.int.j[[1]] ) - refugia.trait.mean.j) - female.fitness.cost.j

        ref.response.in.ref.j[[1]]  = heritability.j * ((ref.in.ref.differential.j[[1]]  + male.differential.refugia.j) / 2)
        ref.response.in.int.j[[1]]  = heritability.j * ((ref.in.int.differential.j[[1]]  + male.differential.refugia.j) / 2)
        int.response.in.int.j[[1]]  = heritability.j * ((int.in.int.differential.j[[1]]  + male.differential.intervention.j) / 2)
        int.response.in.ref.j[[1]]  = heritability.j * ((int.in.ref.differential.j[[1]]  + male.differential.intervention.j) / 2)



        #The Tracked Insecticide::::
        ref.staying.ref.tracked[[1]] = initial.refugia.densities.tracked* (1-coverage)*(1-dispersal.rate)
        ref.joining.int.tracked[[1]]  = initial.refugia.densities.tracked * (1-coverage) * dispersal.rate

        int.staying.int.tracked[[1]]  = (initial.intervention.densities.tracked * female.exposure * mean.survival.int.i * coverage.i * coverage * (1-dispersal.rate)) +
          (initial.intervention.densities.tracked * female.exposure * coverage.j * mean.survival.int.j * coverage * (1-dispersal.rate)) +
          (initial.intervention.densities.tracked * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage * (1-dispersal.rate)) + #i only
          (initial.intervention.densities.tracked * female.exposure * coverage.ij  * mean.survival.int.j * mean.survival.int.j * coverage * (1-dispersal.rate)) + #j only
          (initial.intervention.densities.tracked * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.j * mean.survival.int.i * coverage * (1-dispersal.rate)) + #both ij
          (initial.intervention.densities.tracked * (1-female.exposure) * coverage * (1-dispersal.rate))


        int.joining.ref.tracked[[1]]  = (initial.intervention.densities.tracked * female.exposure * mean.survival.int.i  * coverage * coverage.i * dispersal.rate) +
          (initial.intervention.densities.tracked * female.exposure * mean.survival.int.j * coverage * coverage.j* dispersal.rate) +
          (initial.intervention.densities.tracked * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage * dispersal.rate) + #i only
          (initial.intervention.densities.tracked * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage * dispersal.rate) + #j only
          (initial.intervention.densities.tracked * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.j * mean.survival.int.i * coverage * dispersal.rate) + #both ij
          (initial.intervention.densities.tracked * (1-female.exposure) * coverage * dispersal.rate)



        ref.number.in.ref.tracked[[1]]  = sum(ref.staying.ref.tracked[[1]] )
        int.number.in.ref.tracked[[1]]  = sum(int.joining.ref.tracked[[1]] )

        int.number.in.int.tracked[[1]]  = sum(int.staying.int.tracked[[1]] )
        ref.number.in.int.tracked[[1]]  = sum(ref.joining.int.tracked[[1]] )

        int.in.int.differential.tracked[[1]]  = exposure.scaling.factor * ((sum(int.staying.int.tracked[[1]] *intervention.normal.distribution.tracked) / int.number.in.int.tracked[[1]] ) - intervention.trait.mean.tracked) - female.fitness.cost.tracked
        int.in.ref.differential.tracked[[1]]  = exposure.scaling.factor * ((sum(int.joining.ref.tracked[[1]] *intervention.normal.distribution.tracked) / int.number.in.ref.tracked[[1]] ) - intervention.trait.mean.tracked) - female.fitness.cost.tracked

        ref.in.ref.differential.tracked[[1]]  = exposure.scaling.factor * ((sum(ref.staying.ref.tracked[[1]] *refugia.normal.distribution.tracked) / ref.number.in.ref.tracked[[1]] ) - refugia.trait.mean.tracked) - female.fitness.cost.tracked
        ref.in.int.differential.tracked[[1]]  = exposure.scaling.factor * ((sum(ref.joining.int.tracked[[1]] *refugia.normal.distribution.tracked) / ref.number.in.int.tracked[[1]] ) - refugia.trait.mean.tracked) - female.fitness.cost.tracked

        ref.response.in.ref.tracked[[1]]  = heritability.tracked * ((ref.in.ref.differential.tracked[[1]]  + male.differential.refugia.tracked) / 2)
        ref.response.in.int.tracked[[1]]  = heritability.tracked * ((ref.in.int.differential.tracked[[1]]  + male.differential.refugia.tracked) / 2)
        int.response.in.int.tracked[[1]]  = heritability.tracked * ((int.in.int.differential.tracked[[1]]  + male.differential.intervention.tracked) / 2)
        int.response.in.ref.tracked[[1]]  = heritability.tracked * ((int.in.ref.differential.tracked[[1]]  + male.differential.intervention.tracked) / 2)


        if(n.cycles > 1){
      for(gonotrophic in 2:n.cycles){

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




        ref.staying.ref.i[[gonotrophic]] = ((ref.staying.ref.i[[gonotrophic-1]] * (1-dispersal.rate)) +
          (ref.joining.int.i[[gonotrophic-1]] * (1-female.exposure)*dispersal.rate) +
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * dispersal.rate * coverage.i * survival.probability.ref.i)+
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * dispersal.rate * mean.survival.ref.j * coverage.j)+
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * survival.probability.int.i * coverage * dispersal.rate) + #i only
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage * dispersal.rate) + #j only
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.j * survival.probability.int.i * coverage * dispersal.rate))*between.gonotrophic.survival #both ij



        ref.joining.int.i[[gonotrophic]]  = ((ref.joining.int.i[[gonotrophic-1]] * (1-female.exposure)*(1-dispersal.rate)) +
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * coverage.i * survival.probability.ref.i) +
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.ref.j * coverage.j) +
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * survival.probability.int.i * coverage * (1-dispersal.rate)) + #i only
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage * (1-dispersal.rate)) + #j only
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.j * survival.probability.int.i * coverage * (1-dispersal.rate)) + #both ij
          (ref.staying.ref.i[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival


        int.staying.int.i[[gonotrophic]]  = ((int.staying.int.i[[gonotrophic-1]] * (1-female.exposure) * (1-dispersal.rate)) +
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * coverage.i * survival.probability.int.i)+
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.int.j * coverage.j)+
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * survival.probability.int.i * coverage * (1-dispersal.rate)) + #i only
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage * (1-dispersal.rate)) + #j only
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.j * survival.probability.int.i * coverage * (1-dispersal.rate)) + #both ij
          (int.joining.ref.i[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival

        int.joining.ref.i[[gonotrophic]]  = ((int.joining.ref.i[[gonotrophic-1]] * (1-dispersal.rate))+
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * dispersal.rate * coverage.i * survival.probability.int.i)+
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * dispersal.rate * mean.survival.int.j * coverage.j)+
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * survival.probability.int.i * coverage * dispersal.rate) + #i only
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage * dispersal.rate) + #j only
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.j * survival.probability.int.i * coverage * dispersal.rate) +
          (int.staying.int.i[[gonotrophic-1]] * (1-female.exposure) * dispersal.rate))*between.gonotrophic.survival



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




        ref.staying.ref.j[[gonotrophic]] = ((ref.staying.ref.j[[gonotrophic-1]] *(1-dispersal.rate)) +
          (ref.joining.int.j[[gonotrophic-1]] * (1-female.exposure)*dispersal.rate) +
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * dispersal.rate * coverage.j * survival.probability.ref.j)+
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * dispersal.rate * mean.survival.ref.i * coverage.j)+
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * survival.probability.int.j * coverage * dispersal.rate) + #j only
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage * dispersal.rate) + #ionly
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * survival.probability.int.j * coverage * dispersal.rate))*between.gonotrophic.survival  #both ij



        ref.joining.int.j[[gonotrophic]]  = ((ref.joining.int.j[[gonotrophic-1]] * (1-female.exposure)*(1-dispersal.rate)) +
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * coverage.j * survival.probability.ref.j) +
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.ref.i * coverage.i) +
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * survival.probability.int.j * coverage * (1-dispersal.rate)) + #j only
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage * (1-dispersal.rate)) + #i only
          (ref.joining.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * survival.probability.int.j * coverage * (1-dispersal.rate)) + #both ij
          (ref.staying.ref.j[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival


        int.staying.int.j[[gonotrophic]]  = ((int.staying.int.j[[gonotrophic-1]] * (1-female.exposure) * (1-dispersal.rate)) +
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * coverage.j * survival.probability.int.j)+
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.int.i * coverage.i)+
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * survival.probability.int.j * coverage * (1-dispersal.rate)) + #j only
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage * (1-dispersal.rate)) + #i only
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * survival.probability.int.j * coverage * (1-dispersal.rate)) + #both ij
          (int.joining.ref.j[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival

        int.joining.ref.j[[gonotrophic]]  = ((int.joining.ref.j[[gonotrophic-1]] * (1-dispersal.rate))+
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * dispersal.rate * coverage.j * survival.probability.int.j)+
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * dispersal.rate * mean.survival.int.i * coverage.i)+
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * survival.probability.int.j * coverage * dispersal.rate) + #j only
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage * dispersal.rate) + #i only
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * survival.probability.int.j * coverage * dispersal.rate) +
          (int.staying.int.j[[gonotrophic-1]] * (1-female.exposure) * dispersal.rate))*between.gonotrophic.survival



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



        #And the tracked insecticide::::::
        ref.staying.ref.tracked[[gonotrophic]] = ((ref.staying.ref.tracked[[gonotrophic-1]] *(1-dispersal.rate)) +
          (ref.joining.int.tracked[[gonotrophic-1]] * (1-female.exposure)*dispersal.rate) +
          (ref.joining.int.tracked[[gonotrophic-1]] * female.exposure * dispersal.rate * coverage.j * mean.survival.ref.j)+
          (ref.joining.int.tracked[[gonotrophic-1]] * female.exposure * dispersal.rate * mean.survival.ref.i * coverage.i)+
          (ref.joining.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * mean.survival.ref.i * coverage * dispersal.rate) + #j only
          (ref.joining.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * mean.survival.ref.i * coverage * dispersal.rate) + #ionly
          (ref.joining.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.ref.i * mean.survival.ref.j * coverage * dispersal.rate))*between.gonotrophic.survival  #both ij



        ref.joining.int.tracked[[gonotrophic]]  = ((ref.joining.int.tracked[[gonotrophic-1]] * (1-female.exposure)*(1-dispersal.rate)) +
          (ref.joining.int.tracked[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * coverage.j * mean.survival.ref.j) +
          (ref.joining.int.tracked[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.ref.i * coverage.i) +
          (ref.joining.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * mean.survival.ref.j * coverage * (1-dispersal.rate)) + #j only
          (ref.joining.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * mean.survival.ref.i * coverage * (1-dispersal.rate)) + #i only
          (ref.joining.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.ref.i * mean.survival.ref.j * coverage * (1-dispersal.rate)) + #both ij
          (ref.joining.int.tracked[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival


        int.staying.int.tracked[[gonotrophic]]  = ((int.staying.int.tracked[[gonotrophic-1]] * (1-female.exposure) * (1-dispersal.rate)) +
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * coverage.j * mean.survival.int.j)+
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.int.i * coverage.i)+
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage * (1-dispersal.rate)) + #j only
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage * (1-dispersal.rate)) + #i only
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * mean.survival.int.j * coverage * (1-dispersal.rate)) + #both ij
          (int.joining.ref.tracked[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival

        int.joining.ref.tracked[[gonotrophic]]  = ((int.joining.ref.tracked[[gonotrophic-1]] * (1-dispersal.rate))+
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * dispersal.rate * coverage.j * mean.survival.int.j)+
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * dispersal.rate * mean.survival.int.i * coverage.i)+
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage * dispersal.rate) + #j only
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage * dispersal.rate) + #i only
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * mean.survival.int.j * coverage * dispersal.rate) +
          (int.staying.int.tracked[[gonotrophic-1]] * (1-female.exposure) * dispersal.rate))*between.gonotrophic.survival



        ref.number.in.ref.tracked[[gonotrophic]]  = sum(ref.staying.ref.tracked[[gonotrophic]] )
        int.number.in.ref.tracked[[gonotrophic]]  = sum(int.joining.ref.tracked[[gonotrophic]] )

        int.number.in.int.tracked[[gonotrophic]]  = sum(int.staying.int.tracked[[gonotrophic]] )
        ref.number.in.int.tracked[[gonotrophic]]  = sum(ref.joining.int.tracked[[gonotrophic]] )

        int.in.int.differential.tracked[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.tracked[[gonotrophic]] *intervention.normal.distribution.tracked) / int.number.in.int.tracked[[gonotrophic]] ) - intervention.trait.mean.tracked) - female.fitness.cost.tracked
        int.in.ref.differential.tracked[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.joining.ref.tracked[[gonotrophic]] *intervention.normal.distribution.tracked) / int.number.in.ref.tracked[[gonotrophic]] ) - intervention.trait.mean.tracked) - female.fitness.cost.tracked

        ref.in.ref.differential.tracked[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.staying.ref.tracked[[gonotrophic]] *refugia.normal.distribution.tracked) / ref.number.in.ref.tracked[[gonotrophic]] ) - refugia.trait.mean.tracked) - female.fitness.cost.tracked
        ref.in.int.differential.tracked[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.joining.int.tracked[[gonotrophic]] *refugia.normal.distribution.tracked) / ref.number.in.int.tracked[[gonotrophic]] ) - refugia.trait.mean.tracked) - female.fitness.cost.tracked

        ref.response.in.ref.tracked[[gonotrophic]]  = heritability.tracked * ((ref.in.ref.differential.tracked[[gonotrophic]]  + male.differential.refugia.tracked) / 2)
        ref.response.in.int.tracked[[gonotrophic]]  = heritability.tracked * ((ref.in.int.differential.tracked[[gonotrophic]]  + male.differential.refugia.tracked) / 2)
        int.response.in.int.tracked[[gonotrophic]]  = heritability.tracked * ((int.in.int.differential.tracked[[gonotrophic]]  + male.differential.intervention.tracked) / 2)
        int.response.in.ref.tracked[[gonotrophic]]  = heritability.tracked * ((int.in.ref.differential.tracked[[gonotrophic]]  + male.differential.intervention.tracked) / 2)


      }
    }
    #repeat for tracked (not deployed) insecticide:::::;:
    #total eggs laid  in refugia
    N.total.ref.tracked = sum(unlist(ref.number.in.ref.tracked ), unlist(int.number.in.ref.tracked ))

    #of which from "refugia" females:
    N.ref.in.ref.tracked  = sum(unlist(ref.number.in.ref.tracked ))

    #of which from "intervention" females:
    N.int.in.ref.tracked  = sum(unlist(int.number.in.ref.tracked ))

    av.ref.in.ref.response.tracked  = sum(((unlist(ref.response.in.ref.tracked ) + (cross.selection.i.k * unlist(ref.response.in.ref.i)) + (cross.selection.j.k * unlist(ref.response.in.ref.j))) * (unlist(ref.number.in.ref.tracked )/N.ref.in.ref.tracked )))
    av.int.in.ref.response.tracked  = sum(((unlist(int.response.in.ref.tracked ) + (cross.selection.i.k * unlist(int.response.in.ref.i)) + (cross.selection.j.k * unlist(int.response.in.ref.j))) * (unlist(int.number.in.ref.tracked )/N.int.in.ref.tracked )))

    final.ref.mean.tracked  = ((N.ref.in.ref.tracked  * (refugia.trait.mean.tracked  + av.ref.in.ref.response.tracked )) +
                                 (N.int.in.ref.tracked  * (intervention.trait.mean.tracked  + av.int.in.ref.response.tracked )))/(N.total.ref.tracked )


    #total eggs laid for Trait i in intervention
    N.total.int.tracked  = sum(unlist(int.number.in.int.tracked ), unlist(ref.number.in.int.tracked ))

    #of which from "refugia" females:
    N.ref.in.int.tracked = sum(unlist(ref.number.in.int.tracked ))

    #of which from "intervention" females:
    N.int.in.int.tracked  = sum(unlist(int.number.in.int.tracked ))

    av.int.in.int.response.tracked  = sum(((unlist(int.response.in.int.tracked ) + (cross.selection.i.k * unlist(int.response.in.int.i)) + (cross.selection.j.k * unlist(int.response.in.int.j))) * (unlist(int.number.in.int.tracked )/N.int.in.int.tracked )))
    av.ref.in.int.response.tracked  = sum(((unlist(ref.response.in.int.tracked ) + (cross.selection.i.k * unlist(ref.response.in.int.i)) + (cross.selection.j.k * unlist(ref.response.in.int.j))) * (unlist(ref.number.in.int.tracked )/N.ref.in.int.tracked )))

    final.int.mean.tracked  = ((N.int.in.int.tracked  * (intervention.trait.mean.tracked  + av.int.in.int.response.tracked )) +
                                 (N.ref.in.int.tracked  * (refugia.trait.mean.tracked  + av.ref.in.int.response.tracked )))/(N.total.int.tracked )





    return(list(final.int.mean.tracked , final.ref.mean.tracked))
  }

  if(coverage == 1 | dispersal.rate == 0){

        mean.survival.int.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.trait.mean.j,
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.j)


        int.staying.int.i[[1]]  = (initial.intervention.densities.i * female.exposure * survival.probability.int.i * coverage.i * coverage) +
          (initial.intervention.densities.i * female.exposure * coverage.j * mean.survival.int.j * coverage) +
          (initial.intervention.densities.i * female.exposure * coverage.ij  * probability.only.i * survival.probability.int.i * coverage) + #i only
          (initial.intervention.densities.i * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage ) + #j only
          (initial.intervention.densities.i * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.j * survival.probability.int.i * coverage) + #both ij
          (initial.intervention.densities.i * (1-female.exposure) * coverage)

        int.number.in.int.i[[1]]  = sum(int.staying.int.i[[1]] )

        int.in.int.differential.i[[1]]  = exposure.scaling.factor * ((sum(int.staying.int.i[[1]] *intervention.normal.distribution.i) / int.number.in.int.i[[1]] ) - intervention.trait.mean.i) - female.fitness.cost.i

        int.response.in.int.i[[1]]  = heritability.i * ((int.in.int.differential.i[[1]]  + male.differential.intervention.i) / 2)

        mean.survival.int.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.trait.mean.i,
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.i)


        int.staying.int.j[[1]]  = (initial.intervention.densities.j * female.exposure * survival.probability.int.j * coverage.j * coverage) +
          (initial.intervention.densities.j * female.exposure * coverage.i * mean.survival.int.i * coverage) +
          (initial.intervention.densities.j * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage) + #i only
          (initial.intervention.densities.j * female.exposure * coverage.ij  * probability.only.j * survival.probability.int.j * coverage) + #j only
          (initial.intervention.densities.j * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * survival.probability.int.j * coverage) + #both ij
          (initial.intervention.densities.j * (1-female.exposure) * coverage)


        int.number.in.int.j[[1]]  = sum(int.staying.int.j[[1]] )


        int.in.int.differential.j[[1]]  = exposure.scaling.factor * ((sum(int.staying.int.j[[1]] *intervention.normal.distribution.j) / int.number.in.int.j[[1]] ) - intervention.trait.mean.j) - female.fitness.cost.j

        int.response.in.int.j[[1]]  = heritability.j * ((int.in.int.differential.j[[1]]  + male.differential.intervention.j) / 2)


        ##And the tracked insecticide:::::
        int.staying.int.tracked[[1]]  = (initial.intervention.densities.tracked * female.exposure * mean.survival.int.j * coverage.j * coverage) +
          (initial.intervention.densities.tracked * female.exposure * coverage.i * mean.survival.int.i * coverage) +
          (initial.intervention.densities.tracked * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage) + #i only
          (initial.intervention.densities.tracked * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage) + #j only
          (initial.intervention.densities.tracked * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * mean.survival.int.j * coverage) + #both ij
          (initial.intervention.densities.tracked * (1-female.exposure) * coverage)


        int.number.in.int.tracked[[1]]  = sum(int.staying.int.tracked[[1]] )


        int.in.int.differential.tracked[[1]]  = exposure.scaling.factor * ((sum(int.staying.int.tracked[[1]] *intervention.normal.distribution.tracked) / int.number.in.int.tracked[[1]] ) - intervention.trait.mean.tracked) - female.fitness.cost.tracked

        int.response.in.int.tracked[[1]]  = heritability.tracked * ((int.in.int.differential.tracked[[1]]  + male.differential.intervention.tracked) / 2)



        if(n.cycles > 1){
      for(gonotrophic in 2:n.cycles){

        mean.survival.int.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean =  (sum(int.staying.int.j[[gonotrophic-1]] * intervention.normal.distribution.j)/(sum(int.staying.int.j[[gonotrophic-1]]))),
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.j)





        int.staying.int.i[[gonotrophic]]  = ((int.staying.int.i[[gonotrophic-1]] * (1-female.exposure) ) +
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.i * survival.probability.int.i)+
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * mean.survival.int.j * coverage.j)+
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * survival.probability.int.i * coverage) + #i only
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage) + #j only
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.j * survival.probability.int.i * coverage))*between.gonotrophic.survival #both ij


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


        int.staying.int.j[[gonotrophic]]  = ((int.staying.int.j[[gonotrophic-1]] * (1-female.exposure)) +
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.j * survival.probability.int.j)+
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * mean.survival.int.i * coverage.i)+
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * survival.probability.int.j * coverage) + #j only
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage) + #i only
          (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * survival.probability.int.j * coverage))*between.gonotrophic.survival #both ij

        int.number.in.int.j[[gonotrophic]]  = sum(int.staying.int.j[[gonotrophic]] )

        int.in.int.differential.j[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.j[[gonotrophic]] *intervention.normal.distribution.j) / int.number.in.int.j[[gonotrophic]] ) - intervention.trait.mean.j) - female.fitness.cost.j

        int.response.in.int.j[[gonotrophic]]  = heritability.j * ((int.in.int.differential.j[[gonotrophic]]  + male.differential.intervention.j) / 2)


        #And for Tracked (not deployed) insecticide:::::

        int.staying.int.tracked[[gonotrophic]]  = ((int.staying.int.tracked[[gonotrophic-1]] * (1-female.exposure)) +
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * coverage.j * mean.survival.int.j)+
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * mean.survival.int.i * coverage.i)+
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.j * mean.survival.int.j * coverage) + #j only
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.only.i * mean.survival.int.i * coverage) + #i only
          (int.staying.int.tracked[[gonotrophic-1]] * female.exposure * coverage.ij  * probability.both.i.j * mean.survival.int.i * mean.survival.int.j * coverage))*between.gonotrophic.survival #both ij

        int.number.in.int.tracked[[gonotrophic]]  = sum(int.staying.int.tracked[[gonotrophic]] )

        int.in.int.differential.tracked[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.tracked[[gonotrophic]] *intervention.normal.distribution.tracked) / int.number.in.int.tracked[[gonotrophic]] ) - intervention.trait.mean.tracked) - female.fitness.cost.tracked

        int.response.in.int.tracked[[gonotrophic]]  = heritability.tracked * ((int.in.int.differential.tracked[[gonotrophic]]  + male.differential.intervention.tracked) / 2)





      }
    }
    final.ref.mean.i = 0

    #total eggs laid for Trait i in intervention
    N.total.int.i = sum(unlist(int.number.in.int.i))

    #of which from "intervention" females:
    N.int.in.int.i = sum(unlist(int.number.in.int.i))

    av.int.in.int.response.i = sum((unlist(int.response.in.int.i) * (unlist(int.number.in.int.i)/N.int.in.int.i)))

    final.int.mean.i = intervention.trait.mean.i + av.int.in.int.response.i


    ###Repeat for Trait j:::

    final.ref.mean.j = 0

    #total eggs laid for Trait i in intervention
    N.total.int.j = sum(unlist(int.number.in.int.j))

    #of which from "intervention" females:
    N.int.in.int.j = sum(unlist(int.number.in.int.j))

    av.int.in.int.response.j = sum((unlist(int.response.in.int.j) * (unlist(int.number.in.int.j)/N.int.in.int.j)))

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


    #And for Tracked insecticide:::
    #total eggs laid for Trait i in intervention
    N.total.int.tracked = sum(unlist(int.number.in.int.tracked))

    #of which from "intervention" females:
    final.ref.mean.tracked =0 #force to be zero as refugia "does not exist"

    N.int.in.int.tracked = sum(unlist(int.number.in.int.tracked))

    av.int.in.int.response.tracked = sum(((unlist(int.response.in.int.tracked) + (cross.selection.i.k * unlist(int.response.in.int.i)) + (cross.selection.j.k * unlist(int.response.in.int.j)))* (unlist(int.number.in.int.tracked)/N.int.in.int.tracked)))

    final.int.mean.tracked = intervention.trait.mean.tracked + av.int.in.int.response.tracked

    final.ref.mean.tracked = ifelse(final.ref.mean.tracked < 0,
                                    yes = 0,
                                    no = final.ref.mean.tracked)

    final.int.mean.tracked = ifelse(final.int.mean.tracked < 0,
                                    yes = 0,
                                    no = final.int.mean.tracked)

    return(list(final.int.mean.tracked, final.ref.mean.tracked))
  }

}
