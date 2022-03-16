wrapper_run_simulation_mixtures_sd_scaled = function(insecticide.parameters.df,
                                           maximum.generations,
                                           sim.array,
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
                                           z.sd.coefficient,
                                           z.sd.intercept){

  mixture.df = select_mixing_stategy(mixture.strategy = "mix.sequential.discrete",
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

    ##It should be noted that this model currently only works with mixtures for 2-insecticide simulations. It does not currently work for the inclusion of
    #multiple mixtures.

    #Stop the simulation if there is no insecticide being deployed anymore.
    if(is.na(deployed.mixture$mixture.id[generation])){break}else{

      tracked.resistance =   multiple_gonotrophic_cycles_mixture_dispersal_sd_scaled(intervention.trait.mean.i = sim.array['intervention', 1, generation-1],
                                                                                     intervention.trait.mean.j = sim.array['intervention', 2, generation-1],
                                                                                     refugia.trait.mean.i = sim.array['refugia', 1, generation-1],
                                                                                     refugia.trait.mean.j = sim.array['refugia', 2, generation-1],
                                                                                     z.sd.coefficient = z.sd.coefficient,
                                                                                     z.sd.intercept = z.sd.intercept,
                                                                                     vector.length = vector.length,
                                                                                     female.exposure = female.exposure,
                                                                                     exposure.scaling.factor = exposure.scaling.factor,
                                                                                     coverage = coverage,
                                                                                     dispersal.rate = dispersal.rate,
                                                                                     male.differential.intervention.i = wrapper_calculate_male_insecticide_fitness_selection_differential_mixtures_sd_scaled(male.trait.mean = sim.array['intervention', 1, generation-1],
                                                                                                                                                                                                             female.insecticide.exposure = female.exposure,
                                                                                                                                                                                                             male.insecticide.exposure = male.exposure,
                                                                                                                                                                                                             z.sd.coefficient = z.sd.coefficient,
                                                                                                                                                                                                             z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                             male.fitness.cost = insecticide.parameters.df$male.fitness.cost[1],
                                                                                                                                                                                                             vector.length = vector.length,
                                                                                                                                                                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                                             regression.coefficient = regression.coefficient,
                                                                                                                                                                                                             regression.intercept = regression.intercept,
                                                                                                                                                                                                             current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.1[generation],
                                                                                                                                                                                                             exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                                             survival.to.other.insecticide = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                                                                                                                                                                           trait.mean = sim.array['intervention', 2, generation-1],
                                                                                                                                                                                                                                                                                                                                                           michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                                                                                                                                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                                                                                                                                                                                                                                         regression.coefficient = regression.coefficient,
                                                                                                                                                                                                                                                                                         regression.intercept = regression.intercept,
                                                                                                                                                                                                                                                                                         current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.2[generation])),
                                                                                     male.differential.intervention.j =  wrapper_calculate_male_insecticide_fitness_selection_differential_mixtures_sd_scaled(male.trait.mean = sim.array['intervention', 2, generation-1],
                                                                                                                                                                                                              female.insecticide.exposure = female.exposure,
                                                                                                                                                                                                              male.insecticide.exposure = male.exposure,
                                                                                                                                                                                                              z.sd.coefficient = z.sd.coefficient,
                                                                                                                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                              male.fitness.cost = insecticide.parameters.df$male.fitness.cost[2],
                                                                                                                                                                                                              vector.length = vector.length,
                                                                                                                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                                                                                                                              regression.intercept = regression.intercept,
                                                                                                                                                                                                              current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.2[generation],
                                                                                                                                                                                                              exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                                              survival.to.other.insecticide = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                                                                                                                                                                            trait.mean = sim.array['intervention', 1, generation-1],
                                                                                                                                                                                                                                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                                                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                                                                                                                                                                                                                                          regression.coefficient = regression.coefficient,
                                                                                                                                                                                                                                                                                          regression.intercept = regression.intercept,
                                                                                                                                                                                                                                                                                          current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.1[generation])),
                                                                                     male.differential.refugia.i = (sd_changes_with_z(current.z = sim.array['refugia', 1, generation-1],
                                                                                                                                      z.sd.intercept = z.sd.intercept,
                                                                                                                                      z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[1]),
                                                                                     male.differential.refugia.j = (sd_changes_with_z(current.z = sim.array['refugia', 2, generation-1],
                                                                                                                                      z.sd.intercept = z.sd.intercept,
                                                                                                                                      z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[2]),
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
                                                                                     current.insecticide.efficacy.i = deployed.mixture$insecticide.efficacy.vector.part.1[generation],
                                                                                     current.insecticide.efficacy.j = deployed.mixture$insecticide.efficacy.vector.part.2[generation])

      sim.array['intervention', 1, generation] = tracked.resistance[[1]]
      sim.array['refugia', 1, generation] = tracked.resistance[[2]]
      sim.array['intervention', 2, generation] = tracked.resistance[[3]]
      sim.array['refugia', 2, generation] = tracked.resistance[[4]]

    }
    #Which irm.strategy is being used: sequence or rotation

    #May be worth making the following chunk of code into its own function as it is a bit chunky
    #at the moment.
    #Update insecticide each time the deployment.frequency is reached:
    if(generation < maximum.generations){
      update.mixture.info = if(generation %% deployment.frequency == 0){
        if(irm.switch.strategy == "rotation"){
          irm_strategy_rotation_mixture_decay(number.of.insecticides = number.of.insecticides,
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
                                                  irm_strategy_sequence_mixture_decay(number.of.insecticides = number.of.insecticides,
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
                                                  if(irm.switch.strategy == "insecticide.1"){
                                                    decision_on_insecticide_1_only(number.of.insecticides = number.of.insecticides,
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
                                                  }
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
