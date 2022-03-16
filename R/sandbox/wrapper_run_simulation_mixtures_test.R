wrapper_run_simulation_mixtures = function(insecticide.parameters.df,
                                           maximum.generations,
                                           sim.array,
                                           standard.deviation,
                                           vector.length,
                                           female.exposure,
                                           exposure.scaling.factor,
                                           coverage,
                                           dispersal.rate,
                                           male.exposure,
                                           maximum.bioassay.survival.proportion,
                                           michaelis.menten.slope,
                                           half.population.bioassay.survival.resistance,
                                           regression.coefficient,
                                           regression.intercept,
                                           n.cycles,
                                           irm.switch.strategy,
                                           deployment.frequency,
                                           number.of.insecticides,
                                           calc.withdrawal.threshold,
                                           calc.return.threshold,
                                           available.vector,
                                           withdrawn.vector,
                                           mixture.strategy){

  mixture.df = select_mixing_stategy(mixture.strategy = mixture.strategy,
                                     number.of.insecticides = number.of.insecticides)

  mixture.id = rep(mixture.df[1, 1], times = deployment.frequency)#first row, first column
  mixture.part.1 = rep(mixture.df[1, 2], times = deployment.frequency)#first row, second column
  mixture.part.2 = rep(mixture.df[1, 3], times = deployment.frequency)#first row, third column

  #The dataframe that holds the mixture deployment information
  deployed.mixture = data.frame(mixture.id, mixture.part.1, mixture.part.2)



  #create vectors to hold the insecticide efficacies
  deployed.mixture$insecticide.efficacy.vector.part.1 = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[deployed.mixture[1,2], 2],
                                                                                           recommended.insecticide.dose = insecticide.parameters.df[deployed.mixture[1, 2],3],
                                                                                           threshold.generations = insecticide.parameters.df[deployed.mixture[1,2], 4],
                                                                                           base.efficacy.decay.rate = insecticide.parameters.df[deployed.mixture[1,2], 5],
                                                                                           rapid.decay.rate = insecticide.parameters.df[deployed.mixture[1,2], 6],
                                                                                           deployment.frequency = deployment.frequency)

  deployed.mixture$insecticide.efficacy.vector.part.2 = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[deployed.mixture[1,3] ,2],
                                                                                           recommended.insecticide.dose = insecticide.parameters.df[deployed.mixture[1,3] ,3],
                                                                                           threshold.generations = insecticide.parameters.df[deployed.mixture[1,3] ,4],
                                                                                           base.efficacy.decay.rate = insecticide.parameters.df[deployed.mixture[1,3] ,5],
                                                                                           rapid.decay.rate = insecticide.parameters.df[deployed.mixture[1,3] ,6],
                                                                                           deployment.frequency = deployment.frequency)
  available.mixtures = mixture.df #at the start all mixtures will be available
  mixture.info = list(available.mixtures, available.vector, withdrawn.vector, deployed.mixture)


  for(generation in 2:maximum.generations){
    #Stop the simulation if there is no insecticide being deployed anymore.
    if(is.na(deployed.mixture$mixture.id[generation])){break}else{

      for(insecticide in 1:number.of.insecticides){

        #if insecticide is deployed::::
        if(insecticide == deployed.mixture$mixture.part.1[generation]|
           insecticide == deployed.mixture$mixture.part.2[generation]){


          tracked.insecticide.efficacy = ifelse(insecticide == deployed.mixture$mixture.part.1[generation],
                                                yes = deployed.mixture$insecticide.efficacy.vector.part.1[generation],
                                                no = deployed.mixture$insecticide.efficacy.vector.part.2[generation])


          #find out which other insecticide insecticide it is:::
          other.insecticide =  get_other_insecticide_mixture(tracked.insecticide = insecticide,
                                                             deployed.mixture = deployed.mixture,
                                                             generation = generation)

          other.insecticide.efficacy = ifelse(other.insecticide == deployed.mixture$mixture.part.1[generation],
                                              yes = deployed.mixture$insecticide.efficacy.vector.part.1[generation],
                                              no = deployed.mixture$insecticide.efficacy.vector.part.2[generation])



          tracked.resistance =   multiple_gonotrophic_cycles_mixture_dispersal(intervention.trait.mean.i = sim.array['intervention', insecticide, generation-1],
                                                                               intervention.trait.mean.j = sim.array['intervention', other.insecticide, generation-1],
                                                                               refugia.trait.mean.i = sim.array['refugia', insecticide, generation-1],
                                                                               refugia.trait.mean.j = sim.array['refugia', other.insecticide, generation-1],
                                                                               standard.deviation = standard.deviation,
                                                                               vector.length = vector.length,
                                                                               female.exposure = female.exposure,
                                                                               exposure.scaling.factor = exposure.scaling.factor,
                                                                               coverage = coverage,
                                                                               dispersal.rate = dispersal.rate,
                                                                               male.differential.intervention.i = wrapper_calculate_male_insecticide_fitness_selection_differential_mixtures(male.trait.mean = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                                                             female.insecticide.exposure = female.exposure,
                                                                                                                                                                                             male.insecticide.exposure = male.exposure,
                                                                                                                                                                                             standard.deviation = standard.deviation,
                                                                                                                                                                                             male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide],
                                                                                                                                                                                             vector.length = vector.length,
                                                                                                                                                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                             regression.coefficient = regression.coefficient,
                                                                                                                                                                                             regression.intercept = regression.intercept,
                                                                                                                                                                                             current.insecticide.efficacy = tracked.insecticide.efficacy,
                                                                                                                                                                                             exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                             survival.to.other.insecticide = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                                                                                                                                                           trait.mean = sim.array['intervention', 2, generation-1],
                                                                                                                                                                                                                                                                                                                                           michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                                                                                                                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                                                                                                                                                                                                                         regression.coefficient = regression.coefficient,
                                                                                                                                                                                                                                                                         regression.intercept = regression.intercept,
                                                                                                                                                                                                                                                                         current.insecticide.efficacy = other.insecticide.efficacy)),
                                                                               male.differential.intervention.j =  wrapper_calculate_male_insecticide_fitness_selection_differential_mixtures(male.trait.mean = sim.array['intervention', other.insecticide, generation-1],
                                                                                                                                                                                              female.insecticide.exposure = female.exposure,
                                                                                                                                                                                              male.insecticide.exposure = male.exposure,
                                                                                                                                                                                              standard.deviation = standard.deviation,
                                                                                                                                                                                              male.fitness.cost = insecticide.parameters.df$male.fitness.cost[other.insecticide],
                                                                                                                                                                                              vector.length = vector.length,
                                                                                                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                                                                                                              regression.intercept = regression.intercept,
                                                                                                                                                                                              current.insecticide.efficacy = other.insecticide.efficacy,
                                                                                                                                                                                              exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                              survival.to.other.insecticide = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                                                                                                                                                            trait.mean = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                                                                                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                                                                                                                                                                                                                          regression.coefficient = regression.coefficient,
                                                                                                                                                                                                                                                                          regression.intercept = regression.intercept,
                                                                                                                                                                                                                                                                          current.insecticide.efficacy = tracked.insecticide.efficacy)),
                                                                               male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                         male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                               male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', other.insecticide, generation-1],
                                                                                                                                                         male.fitness.cost = insecticide.parameters.df$male.fitness.cost[other.insecticide]),
                                                                               female.fitness.cost.i = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                               female.fitness.cost.j = insecticide.parameters.df$female.fitness.cost[other.insecticide],
                                                                               heritability.i = insecticide.parameters.df$heritability[insecticide],
                                                                               heritability.j = insecticide.parameters.df$heritability[other.insecticide],
                                                                               n.cycles = n.cycles,
                                                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                               michaelis.menten.slope = michaelis.menten.slope,
                                                                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                               regression.coefficient = regression.coefficient,
                                                                               regression.intercept = regression.intercept,
                                                                               current.insecticide.efficacy.i = tracked.insecticide.efficacy,
                                                                               current.insecticide.efficacy.j = other.insecticide.efficacy)

          sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
          sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]

        }
        if(insecticide != deployed.mixture$mixture.part.1[generation]&
           insecticide != deployed.mixture$mixture.part.2[generation]){


          tracked.resistance = multiple_gonotrophic_cycles_mixture_dispersal_not_deployed(intervention.trait.mean.i = sim.array['intervention', deployed.mixture$mixture.part.1[generation], generation - 1],
                                                                                          intervention.trait.mean.j = sim.array['intervention', deployed.mixture$mixture.part.2[generation], generation - 1],
                                                                                          intervention.trait.mean.tracked = sim.array['intervention', insecticide, generation - 1],
                                                                                          refugia.trait.mean.i = sim.array['refugia', deployed.mixture$mixture.part.1[generation], generation - 1],
                                                                                          refugia.trait.mean.j = sim.array['refugia', deployed.mixture$mixture.part.2[generation], generation - 1],
                                                                                          refugia.trait.mean.tracked = sim.array['refugia', insecticide, generation - 1],
                                                                                          standard.deviation = standard.deviation,
                                                                                          vector.length = vector.length,
                                                                                          female.exposure = female.exposure,
                                                                                          exposure.scaling.factor,
                                                                                          coverage = coverage,
                                                                                          dispersal.rate = dispersal.rate,
                                                                                          male.differential.intervention.i = wrapper_calculate_male_insecticide_fitness_selection_differential_mixtures(male.trait.mean = sim.array['intervention', deployed.mixture$mixture.part.1[generation], generation-1],
                                                                                                                                                                                                        female.insecticide.exposure = female.exposure,
                                                                                                                                                                                                        male.insecticide.exposure = male.exposure,
                                                                                                                                                                                                        standard.deviation = standard.deviation,
                                                                                                                                                                                                        male.fitness.cost = insecticide.parameters.df$male.fitness.cost[deployed.mixture$mixture.part.1[generation]],
                                                                                                                                                                                                        vector.length = vector.length,
                                                                                                                                                                                                        maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                        michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                                        regression.coefficient = regression.coefficient,
                                                                                                                                                                                                        regression.intercept = regression.intercept,
                                                                                                                                                                                                        current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.1[generation],
                                                                                                                                                                                                        exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                                        survival.to.other.insecticide = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                                                                                                                                                                      trait.mean = sim.array['intervention', deployed.mixture$mixture.part.2[generation], generation-1],
                                                                                                                                                                                                                                                                                                                                                      michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                                                                                                                                                                      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                                                                                                                                                                                                                                    regression.coefficient = regression.coefficient,
                                                                                                                                                                                                                                                                                    regression.intercept = regression.intercept,
                                                                                                                                                                                                                                                                                    current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.2[generation])),
                                                                                          male.differential.intervention.j = wrapper_calculate_male_insecticide_fitness_selection_differential_mixtures(male.trait.mean = sim.array['intervention', deployed.mixture$mixture.part.2[generation], generation-1],
                                                                                                                                                                                                          female.insecticide.exposure = female.exposure,
                                                                                                                                                                                                          male.insecticide.exposure = male.exposure,
                                                                                                                                                                                                          standard.deviation = standard.deviation,
                                                                                                                                                                                                          male.fitness.cost = insecticide.parameters.df$male.fitness.cost[deployed.mixture$mixture.part.2[generation]],
                                                                                                                                                                                                          vector.length = vector.length,
                                                                                                                                                                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                          michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                                          regression.coefficient = regression.coefficient,
                                                                                                                                                                                                          regression.intercept = regression.intercept,
                                                                                                                                                                                                          current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.2[generation],
                                                                                                                                                                                                          exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                                          survival.to.other.insecticide = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                                                                                                                                                                        trait.mean = sim.array['intervention', deployed.mixture$mixture.part.1[generation], generation-1],
                                                                                                                                                                                                                                                                                                                                                        michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                                                                                                                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                                                                                                                                                                                                                                      regression.coefficient = regression.coefficient,
                                                                                                                                                                                                                                                                                      regression.intercept = regression.intercept,
                                                                                                                                                                                                                                                                                      current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.1[generation])),
                                                                                          male.differential.intervention.tracked = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                                               male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                                          male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', deployed.mixture$mixture.part.1[generation], generation-1],
                                                                                                                                                                    male.fitness.cost = insecticide.parameters.df$male.fitness.cost[deployed.mixture$mixture.part.1[generation]]),
                                                                                          male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', deployed.mixture$mixture.part.2[generation], generation-1],
                                                                                                                                                                    male.fitness.cost = insecticide.parameters.df$male.fitness.cost[deployed.mixture$mixture.part.2[generation]]),
                                                                                          male.differential.refugia.tracked = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                                          male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                                          female.fitness.cost.i = insecticide.parameters.df$female.fitness.cost[deployed.mixture$mixture.part.1[generation]],
                                                                                          female.fitness.cost.j = insecticide.parameters.df$female.fitness.cost[deployed.mixture$mixture.part.2[generation]],
                                                                                          female.fitness.cost.tracked = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                                          heritability.i = insecticide.parameters.df$heritability[deployed.mixture$mixture.part.1[generation]],
                                                                                          heritability.j = insecticide.parameters.df$heritability[deployed.mixture$mixture.part.2[generation]],
                                                                                          heritability.tracked = insecticide.parameters.df$heritability[insecticide],
                                                                                          n.cycles = n.cycles,
                                                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                          michaelis.menten.slope =michaelis.menten.slope,
                                                                                          maximum.bioassay.survival.proportion =maximum.bioassay.survival.proportion,
                                                                                          regression.coefficient = regression.coefficient,
                                                                                          regression.intercept = regression.intercept,
                                                                                          current.insecticide.efficacy.i = deployed.mixture$insecticide.efficacy.vector.part.1[generation],
                                                                                          current.insecticide.efficacy.j = deployed.mixture$insecticide.efficacy.vector.part.2[generation])


          sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
          sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]

        }
      }}
    #Which irm.strategy is being used: sequence or rotation

    #May be worth making the following chunk of code into its own function as it is a bit chunky
    #at the moment.
    #Update insecticide each time the deployment.frequency is reached:
    if(generation < maximum.generations){
      if(generation %% deployment.frequency == 0){
        if(irm.switch.strategy == "rotation"){
          update.mixture.info = irm_strategy_rotation_mixture_decay(number.of.insecticides = number.of.insecticides,
                                              current.generation = generation,
                                              withdrawal.threshold = calc.withdrawal.threshold,
                                              return.threshold = calc.return.threshold,
                                              simulation.array = sim.array,
                                              available.vector = available.vector,
                                              withdrawn.vector = withdrawn.vector,
                                              mixture.df = mixture.df,
                                              current.mixture = deployed.mixture$mixture.id[generation],
                                              deployment.frequency = deployment.frequency,
                                              deployment.df = deployed.mixture,
                                              insecticide.parameters.df = insecticide.parameters.df)} else{
                                                if(irm.switch.strategy == "sequence"){
                                                  update.mixture.info = irm_strategy_sequence_mixture_decay(number.of.insecticides = number.of.insecticides,
                                                                                      current.generation = generation,
                                                                                      withdrawal.threshold = calc.withdrawal.threshold,
                                                                                      return.threshold = calc.return.threshold,
                                                                                      simulation.array = sim.array,
                                                                                      available.vector = available.vector,
                                                                                      withdrawn.vector = withdrawn.vector,
                                                                                      mixture.df = mixture.df,
                                                                                      current.mixture = deployed.mixture$mixture.id[generation],
                                                                                      deployment.frequency = deployment.frequency,
                                                                                      deployment.df = deployed.mixture,
                                                                                      insecticide.parameters.df = insecticide.parameters.df)
                                                }else{
                                                  if(irm.switch.strategy == "novel.rotation"){
                                                    update.mixture.info = decision_on_insecticide_1_only_rotation(number.of.insecticides = number.of.insecticides,
                                                                                   current.generation = generation,
                                                                                   withdrawal.threshold = calc.withdrawal.threshold,
                                                                                   return.threshold = calc.return.threshold,
                                                                                   simulation.array = sim.array,
                                                                                   available.vector = available.vector,
                                                                                   withdrawn.vector = withdrawn.vector,
                                                                                   mixture.df = mixture.df,
                                                                                   current.mixture = deployed.mixture$mixture.id[generation],
                                                                                   deployment.frequency = deployment.frequency,
                                                                                   deployment.df = deployed.mixture,
                                                                                   insecticide.parameters.df = insecticide.parameters.df)
                                                  }else{
                                                    if(irm.switch.strategy == "novel.sequence"){
                                                      update.mixture.info = decision_on_insecticide_1_only_sequence(number.of.insecticides = number.of.insecticides,
                                                                                                                    current.generation = generation,
                                                                                                                    withdrawal.threshold = calc.withdrawal.threshold,
                                                                                                                    return.threshold = calc.return.threshold,
                                                                                                                    simulation.array = sim.array,
                                                                                                                    available.vector = available.vector,
                                                                                                                    withdrawn.vector = withdrawn.vector,
                                                                                                                    mixture.df = mixture.df,
                                                                                                                    current.mixture = deployed.mixture$mixture.id[generation],
                                                                                                                    deployment.frequency = deployment.frequency,
                                                                                                                    deployment.df = deployed.mixture,
                                                                                                                    insecticide.parameters.df = insecticide.parameters.df)
                                                  }}
                                                }
                                              }

        # mixture.info = list(available.mixtures, available.vector, withdrawn.vector, deployed.mixture)
      }
      if(generation %% deployment.frequency == 0){available.mixtures = update.mixture.info[[1]]}
      if(generation %% deployment.frequency == 0){available.vector = update.mixture.info[[2]]}
      if(generation %% deployment.frequency == 0){withdrawn.vector = update.mixture.info[[3]]}
      if(generation %% deployment.frequency == 0){deployed.mixture = update.mixture.info[[4]]

      }
    }
  }



  return(list(sim.array, deployed.mixture))


}
