####
#Adding in reduced dose mixture LLIN in a micromosaic with standard LLIN.

perform_male_mixture_micromosaic_differential_smooth_sd_scaled = function(coverage,
                                                                          coverage.i,
                                                                          coverage.ij,
                                                                          trait.mean.1,
                                                                          trait.mean.2,
                                                                          z.sd.intercept,
                                                                          z.sd.coefficient,
                                                                          vector.length,
                                                                          female.exposure,
                                                                          male.exposure,
                                                                          current.insecticide.efficacy.1,
                                                                          current.insecticide.efficacy.2,
                                                                          current.insecticide.efficacy.i.mix,
                                                                          regression.coefficient,
                                                                          regression.intercept,
                                                                          half.population.bioassay.survival.resistance,
                                                                          michaelis.menten.slope,
                                                                          maximum.bioassay.survival.proportion){

  #create the starting conditions for the first gonotrophic cycle
  #Values of the Normal Distrition of Trait 1 (insecticide 1)
  intervention.normal.distribution.i = create_normal_distribution(vector.length = vector.length,
                                                                  trait.mean = trait.mean.1,
                                                                  standard.deviation = sd_changes_with_z(current.z = trait.mean.1,
                                                                                                         z.sd.intercept = z.sd.intercept,
                                                                                                         z.sd.coefficient = z.sd.coefficient))

  #Values of the Normal distribution of Trait 2 (insecticide 2)
  intervention.normal.distribution.j = create_normal_distribution(vector.length = vector.length,
                                                                  trait.mean = trait.mean.2,
                                                                  standard.deviation = sd_changes_with_z(current.z = trait.mean.2,
                                                                                                         z.sd.intercept = z.sd.intercept,
                                                                                                         z.sd.coefficient = z.sd.coefficient))

  #Relative Frequency of each of Trait 1 of the Normal Distribution
  initial.intervention.densities.i = calculate_density_of_trait_values(vector.length = vector.length,
                                                                       trait.mean = trait.mean.1,
                                                                       standard.deviation = sd_changes_with_z(current.z = trait.mean.1,
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient))


  #Relative Frequency of each of Trait 2 of the Normal Distribution
  initial.intervention.densities.j = calculate_density_of_trait_values(vector.length = vector.length,
                                                                       trait.mean = trait.mean.2,
                                                                       standard.deviation = sd_changes_with_z(current.z = trait.mean.2,
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient))


  #Create a vector of the field survival to the first insecticide dependent on the insecticide efficacy
  survival.probability.int.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.normal.distribution.i,
                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                           regression.coefficient = regression.coefficient,
                                                                           regression.intercept = regression.intercept,
                                                                           current.insecticide.efficacy = current.insecticide.efficacy.1)


  survival.probability.int.i.mix = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.normal.distribution.i,
                                                                                                                                                 half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                 michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                 maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                               regression.coefficient = regression.coefficient,
                                                                               regression.intercept = regression.intercept,
                                                                               current.insecticide.efficacy = current.insecticide.efficacy.i.mix)



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

  mean.survival.int.i.mix = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = trait.mean.1,
                                                                                                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                          michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                        regression.coefficient = regression.coefficient,
                                                                        regression.intercept = regression.intercept,
                                                                        current.insecticide.efficacy = current.insecticide.efficacy.i.mix)

  mean.survival.int.j = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = trait.mean.2,
                                                                                                                                      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                      michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                      maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                    regression.coefficient = regression.coefficient,
                                                                    regression.intercept = regression.intercept,
                                                                    current.insecticide.efficacy = current.insecticide.efficacy.2)




  update.frequencies.i  = (initial.intervention.densities.i * (female.exposure*male.exposure) * survival.probability.int.i * coverage.i * coverage) +
    (initial.intervention.densities.i * (female.exposure*male.exposure) * coverage.ij  * mean.survival.int.j * survival.probability.int.i.mix * coverage) + #both ij
    (initial.intervention.densities.i * (1-(female.exposure*male.exposure)) * coverage)



  total.males.surviving.i  = sum(update.frequencies.i)


  male.differential.i  = ((sum(update.frequencies.i *intervention.normal.distribution.i) / total.males.surviving.i ) - trait.mean.1)



  update.frequencies.j  = (initial.intervention.densities.j * (female.exposure*male.exposure) * coverage.i * mean.survival.int.i * coverage) +
    (initial.intervention.densities.j * (female.exposure*male.exposure) * coverage.ij  *  mean.survival.int.i.mix * survival.probability.int.j * coverage) + #both ij
    (initial.intervention.densities.j * (1-(female.exposure*male.exposure)) * coverage)



  total.males.surviving.j  = sum(update.frequencies.j)


  male.differential.j  = ((sum(update.frequencies.j *intervention.normal.distribution.j) / total.males.surviving.j ) - trait.mean.2)

  return(list(male.differential.i, male.differential.j))
}


multiple_gonotrophic_cycles_mixture_micromosaic_dispersal_sd_scaled = function(intervention.trait.mean.i,
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
                                                                               current.insecticide.efficacy.i.mix,
                                                                               current.insecticide.efficacy.j,
                                                                               coverage.i,
                                                                               coverage.ij,
                                                                               z.sd.intercept,
                                                                               z.sd.coefficient,
                                                                               cross.selection.i.j,
                                                                               cross.selection.j.i,
                                                                               between.gonotrophic.survival){

  #create the fitness cost selection differentials for females::
  calculate.female.fitness.cost.refugia.i  = female.fitness.cost.i * (sd_changes_with_z(current.z = refugia.trait.mean.i,
                                                                                        z.sd.intercept = z.sd.intercept,
                                                                                        z.sd.coefficient = z.sd.coefficient))

  calculate.female.fitness.cost.intervention.i = female.fitness.cost.i* (sd_changes_with_z(current.z = intervention.trait.mean.i,
                                                                                           z.sd.intercept = z.sd.intercept,
                                                                                           z.sd.coefficient = z.sd.coefficient))


  calculate.female.fitness.cost.refugia.j  = female.fitness.cost.j * (sd_changes_with_z(current.z = refugia.trait.mean.j,
                                                                                        z.sd.intercept = z.sd.intercept,
                                                                                        z.sd.coefficient = z.sd.coefficient))

  calculate.female.fitness.cost.intervention.j = female.fitness.cost.j * (sd_changes_with_z(current.z = intervention.trait.mean.j,
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
  #Survival probabilities given PRS::
  survival.probability.int.i.mix = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.normal.distribution.i,
                                                                                                                                                 half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                 michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                 maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                               regression.coefficient = regression.coefficient,
                                                                               regression.intercept = regression.intercept,
                                                                               current.insecticide.efficacy = current.insecticide.efficacy.i.mix)
  #if traits from refugia.
  survival.probability.ref.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = refugia.normal.distribution.i,
                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                           regression.coefficient = regression.coefficient,
                                                                           regression.intercept = regression.intercept,
                                                                           current.insecticide.efficacy = current.insecticide.efficacy.i)


  survival.probability.ref.i.mix = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = refugia.normal.distribution.i,
                                                                                                                                                 half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                 michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                 maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                               regression.coefficient = regression.coefficient,
                                                                               regression.intercept = regression.intercept,
                                                                               current.insecticide.efficacy = current.insecticide.efficacy.i.mix)



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
      (initial.intervention.densities.i * female.exposure * coverage.ij  * mean.survival.int.j * survival.probability.int.i.mix * coverage * (1-dispersal.rate)) + #both ij
      (initial.intervention.densities.i * (1-female.exposure) * coverage * (1-dispersal.rate))


    int.joining.ref.i[[1]]  = (initial.intervention.densities.i * female.exposure * survival.probability.int.i  * coverage * coverage.i * dispersal.rate) +
      (initial.intervention.densities.i * female.exposure * coverage.ij  * mean.survival.int.j * survival.probability.int.i.mix * coverage * dispersal.rate) + #both ij
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

    mean.survival.int.i.mix = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = intervention.trait.mean.i,
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.i.mix)


    ref.staying.ref.j[[1]] = initial.refugia.densities.j * (1-coverage)*(1-dispersal.rate)
    ref.joining.int.j[[1]]  = initial.refugia.densities.j * (1-coverage) * dispersal.rate

    int.staying.int.j[[1]]  =   (initial.intervention.densities.j * female.exposure * coverage.i * mean.survival.int.i * coverage * (1-dispersal.rate)) +
      (initial.intervention.densities.j * female.exposure * coverage.ij  *  mean.survival.int.i.mix * survival.probability.int.j * coverage * (1-dispersal.rate)) + #both ij
      (initial.intervention.densities.j * (1-female.exposure) * coverage * (1-dispersal.rate))


    int.joining.ref.j[[1]]  = (initial.intervention.densities.j * female.exposure * mean.survival.int.i * coverage *coverage.i* dispersal.rate) +
      (initial.intervention.densities.j * female.exposure * coverage.ij  * mean.survival.int.i.mix * survival.probability.int.j * coverage * dispersal.rate) + #both ij
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
                                              (ref.joining.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * mean.survival.int.j * survival.probability.int.i.mix * coverage * dispersal.rate))*between.gonotrophic.survival #both ij



        ref.joining.int.i[[gonotrophic]]  = ((ref.joining.int.i[[gonotrophic-1]] * (1-female.exposure)*(1-dispersal.rate)) +
                                               (ref.joining.int.i[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * coverage.i * survival.probability.ref.i) +
                                               (ref.joining.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * mean.survival.int.j * survival.probability.int.i.mix * coverage * (1-dispersal.rate)) + #both ij
                                               (ref.staying.ref.i[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival


        int.staying.int.i[[gonotrophic]]  = ((int.staying.int.i[[gonotrophic-1]] * (1-female.exposure) * (1-dispersal.rate)) +
                                               (int.staying.int.i[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * coverage.i * survival.probability.int.i)+
                                               (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  *  mean.survival.int.j * survival.probability.int.i.mix * coverage * (1-dispersal.rate)) + #both ij
                                               (int.joining.ref.i[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival

        int.joining.ref.i[[gonotrophic]]  = ((int.joining.ref.i[[gonotrophic-1]] * (1-dispersal.rate))+
                                               (int.staying.int.i[[gonotrophic-1]] * female.exposure * dispersal.rate * coverage.i * survival.probability.int.i)+
                                               (int.staying.int.i[[gonotrophic-1]] * female.exposure * coverage.ij  * mean.survival.int.j * survival.probability.int.i.mix * coverage * dispersal.rate) +
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

        mean.survival.int.i.mix = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean =  sum((int.staying.int.i[[gonotrophic-1]] * intervention.normal.distribution.i))/(sum(int.staying.int.i[[gonotrophic-1]])),
                                                                                                                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                              regression.coefficient = regression.coefficient,
                                                                              regression.intercept = regression.intercept,
                                                                              current.insecticide.efficacy = current.insecticide.efficacy.i.mix)

        mean.survival.ref.i = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean =  (ref.joining.int.i[[gonotrophic-1]] * refugia.normal.distribution.i)/(sum(int.staying.int.i[[gonotrophic-1]])),
                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                          regression.coefficient = regression.coefficient,
                                                                          regression.intercept = regression.intercept,
                                                                          current.insecticide.efficacy = current.insecticide.efficacy.i)

        mean.survival.ref.i.mix = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean =  (ref.joining.int.i[[gonotrophic-1]] * refugia.normal.distribution.i)/(sum(int.staying.int.i[[gonotrophic-1]])),
                                                                                                                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion),
                                                                              regression.coefficient = regression.coefficient,
                                                                              regression.intercept = regression.intercept,
                                                                              current.insecticide.efficacy = current.insecticide.efficacy.i.mix)



        ref.staying.ref.j[[gonotrophic]] = ((ref.staying.ref.j[[gonotrophic-1]] *(1-dispersal.rate)) +
                                              (ref.joining.int.j[[gonotrophic-1]] * (1-female.exposure)*dispersal.rate) +
                                              (ref.joining.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  * mean.survival.int.i.mix * survival.probability.int.j * coverage * dispersal.rate))*between.gonotrophic.survival  #both ij



        ref.joining.int.j[[gonotrophic]]  = ((ref.joining.int.j[[gonotrophic-1]] * (1-female.exposure)*(1-dispersal.rate)) +
                                               (ref.joining.int.j[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.ref.i * coverage.i) +
                                               (ref.joining.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  *  mean.survival.int.i.mix * survival.probability.int.j * coverage * (1-dispersal.rate)) + #both ij
                                               (ref.staying.ref.j[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival


        int.staying.int.j[[gonotrophic]]  = ((int.staying.int.j[[gonotrophic-1]] * (1-female.exposure) * (1-dispersal.rate)) +
                                               (int.staying.int.j[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) * mean.survival.int.i * coverage.i)+
                                               (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  *  mean.survival.int.i.mix * survival.probability.int.j * coverage * (1-dispersal.rate)) + #both ij
                                               (int.joining.ref.j[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival

        int.joining.ref.j[[gonotrophic]]  = ((int.joining.ref.j[[gonotrophic-1]] * (1-dispersal.rate))+
                                               (int.staying.int.j[[gonotrophic-1]] * female.exposure * dispersal.rate * mean.survival.int.i * coverage.i)+
                                               (int.staying.int.j[[gonotrophic-1]] * female.exposure * coverage.ij  *  mean.survival.int.i.mix * survival.probability.int.j * coverage * dispersal.rate) +
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


    return(list(final.int.mean.i, final.ref.mean.i, final.int.mean.j, final.ref.mean.j))
  }

}


wrapper_run_mixture_micromosaic_simulation_sd_scaled = function(exposure.scaling.factor = 10,
                                                      female.fitness.cost = 0,
                                                      male.fitness.cost = 0,
                                                      female.exposure = 0.7,
                                                      male.exposure = 0.7,
                                                      heritability = 0.3,
                                                      dispersal.rate = 0.3,
                                                      coverage = 0.8,
                                                      standard.deviation = 50,
                                                      vector.length = 1000,
                                                      maximum.bioassay.survival.proportion = 1,
                                                      michaelis.menten.slope = 1,
                                                      regression.coefficient = 0.48,
                                                      regression.intercept = 0.15,
                                                      maximum.generations = 60,
                                                      half.population.bioassay.survival.resistance = 900,
                                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                      return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                      deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                      maximum.resistance.value = 25000,
                                                      starting.refugia.resistance.score = 0,
                                                      starting.intervention.resistance.score = 0,
                                                      applied.insecticide.dose = 1,
                                                      recommended.insecticide.dose = 1,
                                                      threshold.generations = 15,
                                                      base.efficacy.decay.rate = 0.015,
                                                      rapid.decay.rate = 0.08,
                                                      n.cycles = 10,
                                                      standard.llin.coverage = 0.4,
                                                      mixture.llin.coverage = 0.4,
                                                      z.sd.intercept = 24.800904,
                                                      z.sd.coefficient = 0.396678,
                                                      min.cross.selection,
                                                      max.cross.selection,
                                                      gonotrophic.cycle.length = 3,
                                                      natural.daily.survival = 1,
                                                      mixture.dosing.j = 0.5,
                                                      mixture.dosing.i = 0.5,
                                                      number.of.insecticides){

  cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = number.of.insecticides,
                                                       min.cross.selection = min.cross.selection,
                                                       max.cross.selection = max.cross.selection)



  sim.array = create_starting_array(n.insecticides = 2,
                                    maximum.generations = maximum.generations)

  #Set the starting resistance scores for the insecticides:
  sim.array = set_starting_resistance_scores(sim.array = sim.array,
                                             starting.refugia.resistance.score = starting.refugia.resistance.score,
                                             starting.intervention.resistance.score = starting.intervention.resistance.score,
                                             number.of.insecticides = 2)


  #Make a dataframe of the insecticide parameters:
  #Note, solo deployment with number.of.insecticides = 1 behaves weird if dataframe is not with 2 insecticides. But has no impact on
  #the simulation being run.
  insecticide.parameters.df = create_insecticide_parameters_dataframe_advanced(number.of.insecticides = number.of.insecticides,
                                                                               applied.insecticide.dose = applied.insecticide.dose,
                                                                               recommended.insecticide.dose = recommended.insecticide.dose,
                                                                               threshold.generation = threshold.generations,
                                                                               base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                                               rapid.decay.rate = rapid.decay.rate,
                                                                               heritability = heritability,
                                                                               female.fitness.cost = female.fitness.cost,
                                                                               male.fitness.cost = male.fitness.cost)
  #between gonotrophic cycle survival:::
  between.gonotrophic.survival = between_gonotrophic_cycle_survival(gonotrophic.cycle.length = gonotrophic.cycle.length,
                                                                    natural.daily.survival = natural.daily.survival)



  #Starting conditions





for(generation in 2:maximum.generations){

      cross.selection.i.j = cross.selection.matrix[1, 2]
      cross.selection.j.i = cross.selection.matrix[2, 1]

      male.insecticide.intervention.selection.differentials =  perform_male_mixture_micromosaic_differential_smooth_sd_scaled(coverage = coverage,
                                                                                                        coverage.i = standard.llin.coverage,
                                                                                                        coverage.ij = mixture.llin.coverage,
                                                                                                        trait.mean.1 = sim.array['intervention', 1, generation-1],
                                                                                                        trait.mean.2 = sim.array['intervention', 2, generation-1],
                                                                                                        z.sd.intercept = z.sd.intercept,
                                                                                                        z.sd.coefficient = z.sd.coefficient,
                                                                                                        vector.length = vector.length,
                                                                                                        female.exposure = female.exposure,
                                                                                                        male.exposure = male.exposure,
                                                                                                        current.insecticide.efficacy.1 = 1,
                                                                                                        current.insecticide.efficacy.2 = mixture.dosing.j,
                                                                                                        current.insecticide.efficacy.i.mix = mixture.dosing.i,
                                                                                                        regression.coefficient = regression.coefficient,
                                                                                                        regression.intercept = regression.intercept,
                                                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                        michaelis.menten.slope = michaelis.menten.slope,
                                                                                                        maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)

      male.differential.i = male.insecticide.intervention.selection.differentials[[1]]
      male.differential.j = male.insecticide.intervention.selection.differentials[[2]]



      tracked = multiple_gonotrophic_cycles_mixture_micromosaic_dispersal_sd_scaled(intervention.trait.mean.i = sim.array['intervention', 1, generation-1],
                                                                            intervention.trait.mean.j = sim.array['intervention', 2, generation-1],
                                                                            refugia.trait.mean.i = sim.array['refugia', 1, generation-1],
                                                                            refugia.trait.mean.j = sim.array['refugia', 2, generation-1],
                                                                            z.sd.intercept = z.sd.intercept,
                                                                            z.sd.coefficient = z.sd.coefficient,
                                                                            vector.length = vector.length,
                                                                            female.exposure = female.exposure,
                                                                            exposure.scaling.factor = exposure.scaling.factor,
                                                                            coverage = coverage,
                                                                            dispersal.rate = dispersal.rate,
                                                                            male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.differential.i,
                                                                                                                                                                         exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                         male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', 1, generation-1],
                                                                                                                                                                                                                                  z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                                  z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[1])),
                                                                            male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential =  male.differential.j, # male.insecticide.intervention.j,
                                                                                                                                                                         exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                         male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', 2, generation-1],
                                                                                                                                                                                                                                  z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                                  z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[2])),

                                                                            male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', 1, generation-1],
                                                                                                                                                      male.fitness.cost =(sd_changes_with_z(current.z = sim.array['refugia', 1, generation-1],
                                                                                                                                                                                            z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                            z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[1])),
                                                                            male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', 2, generation-1],
                                                                                                                                                      male.fitness.cost = (sd_changes_with_z(current.z = sim.array['refugia', 2, generation-1],
                                                                                                                                                                                             z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                             z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[2])),
                                                                            female.fitness.cost.i = insecticide.parameters.df$female.fitness.cost[1],
                                                                            female.fitness.cost.j = insecticide.parameters.df$female.fitness.cost[2],
                                                                            heritability.i = insecticide.parameters.df$heritability[1],
                                                                            heritability.j = insecticide.parameters.df$heritability[2],
                                                                            n.cycles = n.cycles,
                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                            maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                            regression.coefficient = regression.coefficient,
                                                                            regression.intercept = regression.intercept,
                                                                            current.insecticide.efficacy.i = 1,
                                                                            current.insecticide.efficacy.i.mix = mixture.dosing.i,
                                                                            current.insecticide.efficacy.j = mixture.dosing.j,
                                                                            coverage.i = standard.llin.coverage,
                                                                            coverage.ij = mixture.llin.coverage,
                                                                            cross.selection.i.j = cross.selection.i.j,
                                                                            cross.selection.j.i = cross.selection.j.i,
                                                                            between.gonotrophic.survival = between.gonotrophic.survival)


      sim.array['intervention', 1, generation] = tracked[[1]]
      sim.array['refugia', 1, generation] = tracked[[2]]
      sim.array['intervention', 2, generation] = tracked[[3]]
      sim.array['refugia', 2, generation] = tracked[[4]]


    }
return(sim.array)

}

convert_output_to_dataframe_mixture_micromosaic = function(simulation.results,
                                                           maximum.generations,
                                                           number.of.insecticides,
                                                           maximum.bioassay.survival.proportion,
                                                           michaelis.menten.slope,
                                                           half.population.bioassay.survival.resistance){

  data.list = list()


  sim.duration = maximum.generations

  for(insecticide in 1:number.of.insecticides){

    insecticide.tracked = as.character(rep(insecticide, times = (2 * maximum.generations))) # 2* as refugia and treatment

    generation.sequence = seq(1, maximum.generations, by = 1)
    time.in.generations = rep(generation.sequence, times = 2) # 2* as refugia and treatment

    resistance.intensity.refugia = simulation.results["refugia", insecticide, ]
    resistance.intensity.refugia = head(resistance.intensity.refugia, n=maximum.generations)
    resistance.intensity.treatment = simulation.results["intervention", insecticide, ]
    resistance.intensity.treatment = head(resistance.intensity.treatment, n=maximum.generations)
    resistance.score = c(resistance.intensity.refugia, resistance.intensity.treatment)

    site.refugia = rep("refugia", times = maximum.generations)
    site.treatment = rep("intervention", times = maximum.generations)
    site = c(site.refugia, site.treatment)

    bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                      michaelis.menten.slope = michaelis.menten.slope,
                                                                      trait.mean = resistance.score,
                                                                      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance)


    data.list[[insecticide]]= data.frame(insecticide.tracked,
                                         resistance.score,
                                         bioassay.survival,
                                         site,
                                         time.in.generations)

  }

  final.df = do.call(rbind, data.list)
  return(final.df)
}




