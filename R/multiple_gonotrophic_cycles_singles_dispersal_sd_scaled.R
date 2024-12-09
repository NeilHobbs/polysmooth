multiple_gonotrophic_cycles_singles_dispersal_sd_scaled = function(intervention.trait.mean.i,
                                                                   refugia.trait.mean.i,
                                                                   vector.length,
                                                                   female.exposure,
                                                                   exposure.scaling.factor,
                                                                   coverage,
                                                                   dispersal.rate,
                                                                   male.differential.intervention.i,
                                                                   male.differential.refugia.i,
                                                                   female.fitness.cost.i,
                                                                   heritability.i,
                                                                   n.cycles,
                                                                   half.population.bioassay.survival.resistance,
                                                                   michaelis.menten.slope,
                                                                   maximum.bioassay.survival.proportion ,
                                                                   regression.coefficient,
                                                                   regression.intercept,
                                                                   current.insecticide.efficacy.i,
                                                                   z.sd.intercept,
                                                                   z.sd.coefficient,
                                                                   between.gonotrophic.survival){

  calculate.female.fitness.cost.refugia  = female.fitness.cost.i * (sd_changes_with_z(current.z = refugia.trait.mean.i,
                                                                                            z.sd.intercept = z.sd.intercept,
                                                                                            z.sd.coefficient = z.sd.coefficient))

  calculate.female.fitness.cost.intervention = female.fitness.cost.i * (sd_changes_with_z(current.z = intervention.trait.mean.i,
                                                                                                    z.sd.intercept = z.sd.intercept,
                                                                                                    z.sd.coefficient = z.sd.coefficient))

  #Step 1: create the Normal Distributions:::
  #Insecticide i: emerge in intervention:
  intervention.normal.distribution.i = create_normal_distribution(vector.length = vector.length,
                                                                  trait.mean = intervention.trait.mean.i,
                                                                  standard.deviation = (sd_changes_with_z(current.z = intervention.trait.mean.i,
                                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                                          z.sd.coefficient = z.sd.coefficient)))



  #Insecticide i: emerge in refugia:
  refugia.normal.distribution.i = create_normal_distribution(vector.length = vector.length,
                                                             trait.mean = refugia.trait.mean.i,
                                                             standard.deviation = (sd_changes_with_z(current.z = refugia.trait.mean.i,
                                                                                                     z.sd.intercept = z.sd.intercept,
                                                                                                     z.sd.coefficient = z.sd.coefficient)))



  #Step 2: Create initial relative frequencies of the normal distibution:
  initial.intervention.densities.i = calculate_density_of_trait_values(vector.length = vector.length,
                                                                       trait.mean = intervention.trait.mean.i,
                                                                       standard.deviation = (sd_changes_with_z(current.z = intervention.trait.mean.i,
                                                                                                               z.sd.intercept = z.sd.intercept,
                                                                                                               z.sd.coefficient = z.sd.coefficient)))


  #Insecticide i: emerge in refugia:
  initial.refugia.densities.i = calculate_density_of_trait_values(vector.length = vector.length,
                                                                  trait.mean = refugia.trait.mean.i,
                                                                  standard.deviation = (sd_changes_with_z(current.z = refugia.trait.mean.i,
                                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                                          z.sd.coefficient = z.sd.coefficient)))


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




  if(coverage < 1){

        ref.staying.ref.i[[1]] = initial.refugia.densities.i * (1-coverage)*(1-dispersal.rate)
        ref.joining.int.i[[1]]  = initial.refugia.densities.i * (1-coverage) * dispersal.rate

        int.staying.int.i[[1]]  = (initial.intervention.densities.i * female.exposure * survival.probability.int.i *  coverage * (1-dispersal.rate)) + (initial.intervention.densities.i * (1-female.exposure) * coverage * (1-dispersal.rate))
        int.joining.ref.i[[1]]  = (initial.intervention.densities.i * female.exposure * survival.probability.int.i *  coverage * dispersal.rate) + (initial.intervention.densities.i * (1-female.exposure) * coverage * dispersal.rate)



        ref.number.in.ref.i[[1]]  = sum(ref.staying.ref.i[[1]])
        int.number.in.ref.i[[1]]  = sum(int.joining.ref.i[[1]])

        int.number.in.int.i[[1]]  = sum(int.staying.int.i[[1]])
        ref.number.in.int.i[[1]]  = sum(ref.joining.int.i[[1]])

        int.in.int.differential.i[[1]]  = exposure.scaling.factor * ((sum(int.staying.int.i[[1]] *intervention.normal.distribution.i) / int.number.in.int.i[[1]] ) - intervention.trait.mean.i) - calculate.female.fitness.cost.intervention
        int.in.ref.differential.i[[1]]  = exposure.scaling.factor * ((sum(int.joining.ref.i[[1]] *intervention.normal.distribution.i) / int.number.in.ref.i[[1]] ) - intervention.trait.mean.i) - calculate.female.fitness.cost.intervention

        ref.in.ref.differential.i[[1]]  = exposure.scaling.factor * ((sum(ref.staying.ref.i[[1]] *refugia.normal.distribution.i) / ref.number.in.ref.i[[1]] ) - refugia.trait.mean.i) - calculate.female.fitness.cost.refugia
        ref.in.int.differential.i[[1]]  = exposure.scaling.factor * ((sum(ref.joining.int.i[[1]] *refugia.normal.distribution.i) / ref.number.in.int.i[[1]] ) - refugia.trait.mean.i) - calculate.female.fitness.cost.refugia

        ref.response.in.ref.i[[1]]  = heritability.i * ((ref.in.ref.differential.i[[1]]  + male.differential.refugia.i) / 2)
        ref.response.in.int.i[[1]]  = heritability.i * ((ref.in.int.differential.i[[1]]  + male.differential.refugia.i) / 2)
        int.response.in.int.i[[1]]  = heritability.i * ((int.in.int.differential.i[[1]]  + male.differential.intervention.i) / 2)
        int.response.in.ref.i[[1]]  = heritability.i * ((int.in.ref.differential.i[[1]]  + male.differential.intervention.i) / 2)


        if(n.cycles > 1){
      for(gonotrophic in 2:n.cycles){

        ref.staying.ref.i[[gonotrophic]] = ((ref.staying.ref.i[[gonotrophic-1]] *(1-dispersal.rate)) +
          (ref.joining.int.i[[gonotrophic-1]] * (1-female.exposure)*dispersal.rate) +
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * dispersal.rate *  survival.probability.ref.i))*between.gonotrophic.survival

        ref.joining.int.i[[gonotrophic]]  = ((ref.joining.int.i[[gonotrophic-1]] * (1-female.exposure)*(1-dispersal.rate)) +
          (ref.joining.int.i[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) *  survival.probability.ref.i) +
          (ref.staying.ref.i[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival


        int.staying.int.i[[gonotrophic]]  = ((int.staying.int.i[[gonotrophic-1]] * (1-female.exposure) * (1-dispersal.rate)) +
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * (1-dispersal.rate) *  survival.probability.int.i)+
          (int.joining.ref.i[[gonotrophic-1]] * dispersal.rate))*between.gonotrophic.survival

        int.joining.ref.i[[gonotrophic]]  = ((int.joining.ref.i[[gonotrophic-1]] * (1-dispersal.rate))+
          (int.staying.int.i[[gonotrophic-1]] * female.exposure * dispersal.rate *  survival.probability.int.i)+
          (int.staying.int.i[[gonotrophic-1]] * (1-female.exposure) * dispersal.rate))*between.gonotrophic.survival



        ref.number.in.ref.i[[gonotrophic]]  = sum(ref.staying.ref.i[[gonotrophic]] )
        int.number.in.ref.i[[gonotrophic]]  = sum(int.joining.ref.i[[gonotrophic]] )

        int.number.in.int.i[[gonotrophic]]  = sum(int.staying.int.i[[gonotrophic]] )
        ref.number.in.int.i[[gonotrophic]]  = sum(ref.joining.int.i[[gonotrophic]] )

        int.in.int.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.i[[gonotrophic]] *intervention.normal.distribution.i) / int.number.in.int.i[[gonotrophic]] ) - intervention.trait.mean.i) - calculate.female.fitness.cost.intervention
        int.in.ref.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.joining.ref.i[[gonotrophic]] *intervention.normal.distribution.i) / int.number.in.ref.i[[gonotrophic]] ) - intervention.trait.mean.i) - calculate.female.fitness.cost.intervention

        ref.in.ref.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.staying.ref.i[[gonotrophic]] *refugia.normal.distribution.i) / ref.number.in.ref.i[[gonotrophic]] ) - refugia.trait.mean.i) - calculate.female.fitness.cost.refugia
        ref.in.int.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(ref.joining.int.i[[gonotrophic]] *refugia.normal.distribution.i) / ref.number.in.int.i[[gonotrophic]] ) - refugia.trait.mean.i) - calculate.female.fitness.cost.refugia

        ref.response.in.ref.i[[gonotrophic]]  = heritability.i * ((ref.in.ref.differential.i[[gonotrophic]]  + male.differential.refugia.i) / 2)
        ref.response.in.int.i[[gonotrophic]]  = heritability.i * ((ref.in.int.differential.i[[gonotrophic]]  + male.differential.refugia.i) / 2)
        int.response.in.int.i[[gonotrophic]]  = heritability.i * ((int.in.int.differential.i[[gonotrophic]]  + male.differential.intervention.i) / 2)
        int.response.in.ref.i[[gonotrophic]]  = heritability.i * ((int.in.ref.differential.i[[gonotrophic]]  + male.differential.intervention.i) / 2)




      }#end cycles for loop
}
    #total eggs laid for Trait i in refugia
    N.total.ref.i = sum(unlist(ref.number.in.ref.i), unlist(int.number.in.ref.i))

    #of which from "refugia" females:
    N.ref.in.ref.i = sum(unlist(ref.number.in.ref.i))

    #of which from "intervention" females:
    N.int.in.ref.i = sum(unlist(int.number.in.ref.i))

    av.ref.in.ref.response.i = sum((unlist(ref.response.in.ref.i) * (unlist(ref.number.in.ref.i)/N.ref.in.ref.i)))
    av.int.in.ref.response.i = sum((unlist(int.response.in.ref.i) * (unlist(int.number.in.ref.i)/N.int.in.ref.i)))

    final.ref.mean.i = ((N.ref.in.ref.i * (refugia.trait.mean.i + av.ref.in.ref.response.i)) +
                          (N.int.in.ref.i * (intervention.trait.mean.i + av.int.in.ref.response.i)))/(N.total.ref.i)


    #total eggs laid for Trait i in intervention
    N.total.int.i = sum(unlist(int.number.in.int.i), unlist(ref.number.in.int.i))

    #of which from "refugia" females:
    N.ref.in.int.i = sum(unlist(ref.number.in.int.i))

    #of which from "intervention" females:
    N.int.in.int.i = sum(unlist(int.number.in.int.i))

    av.int.in.int.response.i = sum((unlist(int.response.in.int.i) * (unlist(int.number.in.int.i)/N.int.in.int.i)))
    av.ref.in.int.response.i = sum((unlist(ref.response.in.int.i) * (unlist(ref.number.in.int.i)/N.ref.in.int.i)))

    final.int.mean.i = ((N.int.in.int.i * (intervention.trait.mean.i + av.int.in.int.response.i)) +
                          (N.ref.in.int.i * (refugia.trait.mean.i + av.ref.in.int.response.i)))/(N.total.int.i)




    #prevent mean PRS values falling below 0:::

    final.ref.mean.i = ifelse(final.ref.mean.i < 0,
                              yes = 0,
                              no = final.ref.mean.i)

    final.int.mean.i = ifelse(final.int.mean.i < 0,
                              yes = 0,
                              no = final.int.mean.i)


    return(list(final.int.mean.i, final.ref.mean.i))
  }#end coverage < 1

  if(coverage == 1 | dispersal.rate == 0){ #calculation is weird if this is not put in

        int.staying.int.i[[1]]  = (initial.intervention.densities.i * female.exposure * survival.probability.int.i * coverage) + (initial.intervention.densities.i * (1-female.exposure) * coverage)

        int.number.in.int.i[[1]]  = sum(int.staying.int.i[[1]])

        int.in.int.differential.i[[1]]  = exposure.scaling.factor * ((sum(int.staying.int.i[[1]] *intervention.normal.distribution.i) / int.number.in.int.i[[1]] ) - intervention.trait.mean.i) - calculate.female.fitness.cost.intervention

        int.response.in.int.i[[1]]  = heritability.i * ((int.in.int.differential.i[[1]]  + male.differential.intervention.i) / 2)


        if(n.cycles > 1){
      for(gonotrophic in 2:n.cycles){



        int.staying.int.i[[gonotrophic]]  = ((int.staying.int.i[[gonotrophic-1]] * (1-female.exposure)) +
          (int.staying.int.i[[gonotrophic-1]] * female.exposure  *  survival.probability.int.i))*between.gonotrophic.survival


        int.number.in.int.i[[gonotrophic]]  = sum(int.staying.int.i[[gonotrophic]] )

        int.in.int.differential.i[[gonotrophic]]  = exposure.scaling.factor * ((sum(int.staying.int.i[[gonotrophic]] *intervention.normal.distribution.i) / int.number.in.int.i[[gonotrophic]] ) - intervention.trait.mean.i) - calculate.female.fitness.cost.intervention

        int.response.in.int.i[[gonotrophic]]  = heritability.i * ((int.in.int.differential.i[[gonotrophic]]  + male.differential.intervention.i) / 2)

      }#end cycles for loop
}

    final.ref.mean.i = 0

    #total eggs laid for Trait i in intervention
    N.total.int.i = sum(unlist(int.number.in.int.i))

    #of which from "intervention" females:
    N.int.in.int.i = sum(unlist(int.number.in.int.i))

    av.int.in.int.response.i = sum((unlist(int.response.in.int.i) * (unlist(int.number.in.int.i)/N.int.in.int.i)))

    final.int.mean.i = intervention.trait.mean.i + av.int.in.int.response.i

    final.int.mean.i = ifelse(final.int.mean.i < 0,
                              yes = 0,
                              no = final.int.mean.i)


    return(list(final.int.mean.i, final.ref.mean.i))
  }

}
