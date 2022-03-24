wrapper_run_simulation_micromosaics = function(insecticide.parameters.df,
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
                                               deployment.frequency,
                                               number.of.insecticides,
                                               intervention.coverage.1,
                                               intervention.coverage.2,
                                               withdrawal.threshold,
                                               return.threshold,
                                               available.vector,
                                               withdrawn.vector,
                                               irm.switch.strategy,
                                               min.cross.selection,
                                               max.cross.selection){

  cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = number.of.insecticides,
                                                       min.cross.selection = min.cross.selection,
                                                       max.cross.selection = max.cross.selection)

  #starts with insecticide 1
  deployed.vector.1 = rep(1, deployment.frequency)


  #The first insecticide deployed is always insecticide 1 for coverage.i
  insecticide.efficacy.vector.1 = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[1,2],
                                                                     recommended.insecticide.dose = insecticide.parameters.df[1,3],
                                                                     threshold.generations = insecticide.parameters.df[1,4],
                                                                     base.efficacy.decay.rate = insecticide.parameters.df[1,5],
                                                                     rapid.decay.rate = insecticide.parameters.df[1,6],
                                                                     deployment.frequency = deployment.frequency)

  #starts with insecticide 2
  deployed.vector.2 = rep(2, deployment.frequency)

  #The first insecticide deployed is always insecticide 2 for coverage.j
  insecticide.efficacy.vector.2 = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[2,2],
                                                                     recommended.insecticide.dose = insecticide.parameters.df[2,3],
                                                                     threshold.generations = insecticide.parameters.df[2,4],
                                                                     base.efficacy.decay.rate = insecticide.parameters.df[2,5],
                                                                     rapid.decay.rate = insecticide.parameters.df[2,6],
                                                                     deployment.frequency = deployment.frequency)

  for(generation in 2:maximum.generations){
    for(insecticide in 1:number.of.insecticides){
      if(insecticide == deployed.vector.1[generation] |
         insecticide == deployed.vector.2[generation] ){

        if(insecticide == deployed.vector.1[generation]){other.insecticide = deployed.vector.2[generation]}
        if(insecticide == deployed.vector.2[generation]){other.insecticide = deployed.vector.1[generation]}

        #Figure out which coverages are correct::::::
        if(insecticide == deployed.vector.1[generation]){tracked.coverage = intervention.coverage.1}
        if(insecticide == deployed.vector.2[generation]){tracked.coverage = intervention.coverage.2}

        if(other.insecticide == deployed.vector.1[generation]){other.coverage = intervention.coverage.1}
        if(other.insecticide == deployed.vector.2[generation]){other.coverage = intervention.coverage.2}


        ##Figure out which is then the corresponding insecticide efficacies:::::
        if(insecticide == deployed.vector.1[generation]){tracked.efficacy = insecticide.efficacy.vector.1[generation]}
        if(insecticide == deployed.vector.2[generation]){tracked.efficacy = insecticide.efficacy.vector.2[generation]}

        if(other.insecticide == deployed.vector.1[generation]){other.insecticide.efficacy = insecticide.efficacy.vector.1[generation]}
        if(other.insecticide == deployed.vector.2[generation]){other.insecticide.efficacy = insecticide.efficacy.vector.2[generation]}

        cross.selection.i.j = cross.selection.matrix[insecticide, other.insecticide]
        cross.selection.j.i = cross.selection.matrix[other.insecticide, insecticide]

        male.selection.differentials = perform_male_micromosaic_smooth(insecticide.coverage.1 = tracked.coverage,
                                                                       insecticide.coverage.2 = other.coverage,
                                                                       trait.mean.1 = sim.array["intervention", insecticide, generation-1],
                                                                       trait.mean.2 = sim.array["intervention", other.insecticide, generation-1],
                                                                       standard.deviation = standard.deviation,
                                                                       vector.length = vector.length,
                                                                       female.exposure = female.exposure,
                                                                       male.exposure = male.exposure,
                                                                       current.insecticide.efficacy.1 = tracked.efficacy,
                                                                       current.insecticide.efficacy.2 = other.insecticide.efficacy,
                                                                       regression.coefficient = regression.coefficient,
                                                                       regression.intercept = regression.intercept,
                                                                       half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                       michaelis.menten.slope = michaelis.menten.slope,
                                                                       maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                       male.natural.survival.probability = 1)

        tracked.resistance = multiple_gonotrophic_cycles_micromosaic_dispersal(intervention.trait.mean.i = sim.array['intervention', insecticide, generation-1],
                                                                               intervention.trait.mean.j = sim.array['intervention', other.insecticide, generation-1],
                                                                               refugia.trait.mean.i = sim.array['refugia', insecticide, generation-1],
                                                                               refugia.trait.mean.j = sim.array['refugia', other.insecticide, generation-1],
                                                                               standard.deviation = standard.deviation,
                                                                               vector.length = vector.length,
                                                                               female.exposure = female.exposure,
                                                                               exposure.scaling.factor = exposure.scaling.factor,
                                                                               coverage = coverage,
                                                                               dispersal.rate = dispersal.rate,
                                                                               male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.selection.differentials[[1]],
                                                                                                                                                                            exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                            male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                               male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.selection.differentials[[2]],
                                                                                                                                                                            exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                            male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[other.insecticide]),
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
                                                                               current.insecticide.efficacy.i = tracked.efficacy,
                                                                               current.insecticide.efficacy.j = other.insecticide.efficacy,
                                                                               coverage.i = tracked.coverage,
                                                                               coverage.j = other.coverage,
                                                                               cross.selection.i.j = cross.selection.i.j,
                                                                               cross.selection.j.i = cross.selection.j.i)

        sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
        sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]

      } #end if deployed
      if(insecticide != deployed.vector.1[generation] &
         insecticide != deployed.vector.2[generation]){

        cross.selection.i.k = cross.selection.matrix[deployed.vector.1[generation], insecticide]
        cross.selection.j.k = cross.selection.matrix[deployed.vector.2[generation], insecticide]


        male.selection.differentials = perform_male_micromosaic_smooth(insecticide.coverage.1 = tracked.coverage,
                                                                       insecticide.coverage.2 = other.coverage,
                                                                       trait.mean.1 = sim.array["intervention", insecticide, generation-1],
                                                                       trait.mean.2 = sim.array["intervention", other.insecticide, generation-1],
                                                                       standard.deviation = standard.deviation,
                                                                       vector.length = vector.length,
                                                                       female.exposure = female.exposure,
                                                                       male.exposure = male.exposure,
                                                                       current.insecticide.efficacy.1 = tracked.efficacy,
                                                                       current.insecticide.efficacy.2 = other.insecticide.efficacy,
                                                                       regression.coefficient = regression.coefficient,
                                                                       regression.intercept = regression.intercept,
                                                                       half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                       michaelis.menten.slope = michaelis.menten.slope,
                                                                       maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                       male.natural.survival.probability = 1)


        tracked.resistance =   multiple_gonotrophic_cycles_micromosaics_dispersal_not_deployed(intervention.trait.mean.i = sim.array['intervention', deployed.vector.1[generation], generation-1],
                                                                                               intervention.trait.mean.j = sim.array['intervention', deployed.vector.2[generation], generation-1],
                                                                                               intervention.trait.mean.tracked = sim.array['intervention', insecticide, generation-1],
                                                                                               refugia.trait.mean.i = sim.array['refugia', deployed.vector.1[generation], generation-1],
                                                                                               refugia.trait.mean.j = sim.array['refugia', deployed.vector.2[generation], generation-1],
                                                                                               refugia.trait.mean.tracked = sim.array['refugia', insecticide, generation-1],
                                                                                               standard.deviation = standard.deviation,
                                                                                               vector.length = vector.length,
                                                                                               female.exposure = female.exposure,
                                                                                               exposure.scaling.factor = exposure.scaling.factor,
                                                                                               coverage = coverage,
                                                                                               dispersal.rate = dispersal.rate,
                                                                                               male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.selection.differentials[[1]],
                                                                                                                                                                                            exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                            male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[deployed.vector.1[generation]]),
                                                                                               male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.selection.differentials[[2]],
                                                                                                                                                                                            exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                            male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[deployed.vector.2[generation]]),
                                                                                               male.differential.intervention.tracked = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                                                    male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                                               male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', deployed.vector.1[generation], generation-1],
                                                                                                                                                                         male.fitness.cost = insecticide.parameters.df$male.fitness.cost[deployed.vector.1[generation]]),
                                                                                               male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', deployed.vector.2[generation], generation-1],
                                                                                                                                                                         male.fitness.cost = insecticide.parameters.df$male.fitness.cost[deployed.vector.2[generation]]),
                                                                                               male.differential.refugia.tracked = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                                                 male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                                               female.fitness.cost.i = insecticide.parameters.df$female.fitness.cost[deployed.vector.1[generation]],
                                                                                               female.fitness.cost.j = insecticide.parameters.df$female.fitness.cost[deployed.vector.2[generation]],
                                                                                               female.fitness.cost.tracked = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                                               heritability.i = insecticide.parameters.df$heritability[deployed.vector.1[generation]],
                                                                                               heritability.j =  insecticide.parameters.df$heritability[deployed.vector.2[generation]],
                                                                                               heritability.tracked =  insecticide.parameters.df$heritability[insecticide],
                                                                                               n.cycles = n.cycles,
                                                                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                               michaelis.menten.slope = michaelis.menten.slope ,
                                                                                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                               regression.coefficient = regression.coefficient,
                                                                                               regression.intercept = regression.intercept,
                                                                                               current.insecticide.efficacy.i = insecticide.efficacy.vector.1[generation],
                                                                                               current.insecticide.efficacy.j = insecticide.efficacy.vector.2[generation],
                                                                                               coverage.i = intervention.coverage.1,
                                                                                               coverage.j = intervention.coverage.2,
                                                                                               cross.selection.i.k = cross.selection.i.k,
                                                                                               cross.selection.j.k = cross.selection.j.k)



        sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
        sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]
      } #end if not deployed
    }#end insecticide loop

    ##Insecticide Deployment Switching Section::::::::
    if(generation < maximum.generations){
      if(generation %% deployment.frequency == 0){
        if(irm.switch.strategy == "partial.rotation"){
          update.deployment.info = irm_strategy_micromosaics_partial_rotation(number.of.insecticides = number.of.insecticides,
                                                                              current.generation = generation,
                                                                              withdrawal.threshold = withdrawal.threshold,
                                                                              return.threshold = return.threshold,
                                                                              simulation.array = sim.array,
                                                                              available.vector = available.vector,
                                                                              withdrawn.vector = withdrawn.vector,
                                                                              deployment.frequency = deployment.frequency,
                                                                              deployed.insecticide.i = deployed.vector.1[generation],
                                                                              deployed.insecticide.j = deployed.vector.2[generation],
                                                                              deployment.vector.i = deployed.vector.1,
                                                                              deployment.vector.j = deployed.vector.2)} else{
                                                                      if(irm.switch.strategy == "full.rotation"){
                                                                        update.deployment.info = irm_strategy_micromosaics_full_rotation(number.of.insecticides = number.of.insecticides,
                                                                                                                                            current.generation = generation,
                                                                                                                                            withdrawal.threshold = withdrawal.threshold,
                                                                                                                                            return.threshold = return.threshold,
                                                                                                                                            simulation.array = sim.array,
                                                                                                                                            available.vector = available.vector,
                                                                                                                                            withdrawn.vector = withdrawn.vector,
                                                                                                                                            deployment.frequency = deployment.frequency,
                                                                                                                                            deployed.insecticide.i = deployed.vector.1[generation],
                                                                                                                                            deployed.insecticide.j = deployed.vector.2[generation],
                                                                                                                                            deployment.vector.i = deployed.vector.1,
                                                                                                                                            deployment.vector.j = deployed.vector.2)
                                                                      }else{
                                                                        if(irm.switch.strategy == "rotate.expensive"){
                                                                          update.deployment.info = irm_strategy_micromosaics_rotate_expensive(number.of.insecticides = number.of.insecticides,
                                                                                                                                              current.generation = generation,
                                                                                                                                              withdrawal.threshold = withdrawal.threshold,
                                                                                                                                              return.threshold = return.threshold,
                                                                                                                                              simulation.array = sim.array,
                                                                                                                                              available.vector = available.vector,
                                                                                                                                              withdrawn.vector = withdrawn.vector,
                                                                                                                                              deployment.frequency = deployment.frequency,
                                                                                                                                              deployed.insecticide.i = deployed.vector.1[generation],
                                                                                                                                              deployed.insecticide.j = deployed.vector.2[generation],
                                                                                                                                              deployment.vector.i = deployed.vector.1,
                                                                                                                                              deployment.vector.j = deployed.vector.2)
                                                                        }else{
                                                                          if(irm.switch.strategy == "sequence"){
                                                                            update.deployment.info = irm_strategy_micromosaics_sequence(number.of.insecticides = number.of.insecticides,
                                                                                                                                        current.generation = generation,
                                                                                                                                        withdrawal.threshold = withdrawal.threshold,
                                                                                                                                        return.threshold = return.threshold,
                                                                                                                                        simulation.array = sim.array,
                                                                                                                                        available.vector = available.vector,
                                                                                                                                        withdrawn.vector = withdrawn.vector,
                                                                                                                                        deployment.frequency = deployment.frequency,
                                                                                                                                        deployed.insecticide.i = deployed.vector.1[generation],
                                                                                                                                        deployed.insecticide.j = deployed.vector.2[generation],
                                                                                                                                        deployment.vector.i = deployed.vector.1,
                                                                                                                                        deployment.vector.j = deployed.vector.2)
                                                                          }}
                                                                      }
                                                                    }

      }

  if(generation %% deployment.frequency == 0){available.vector = update.deployment.info[[1]]}
  if(generation %% deployment.frequency == 0){withdrawn.vector = update.deployment.info[[2]]}
  if(generation %% deployment.frequency == 0){deployed.vector.1 = update.deployment.info[[3]]}
  if(generation %% deployment.frequency == 0){deployed.vector.2 = update.deployment.info[[4]]}
  if(generation %% deployment.frequency == 0){insecticide.i = deployed.vector.1[generation+1]}
  if(generation %% deployment.frequency == 0){insecticide.j = deployed.vector.2[generation+1]}
  if(generation %% deployment.frequency == 0){insecticide.efficacy.vector.1 = c(insecticide.efficacy.vector.1,
                                                                              create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[insecticide.i, 2],
                                                                                                                 recommended.insecticide.dose = insecticide.parameters.df[insecticide.i, 3],
                                                                                                                 threshold.generations = insecticide.parameters.df[insecticide.i, 4],
                                                                                                                 base.efficacy.decay.rate = insecticide.parameters.df[insecticide.i, 5],
                                                                                                                 rapid.decay.rate = insecticide.parameters.df[insecticide.i, 6],
                                                                                                                 deployment.frequency = deployment.frequency))}
  if(generation %% deployment.frequency == 0){insecticide.efficacy.vector.2 = c(insecticide.efficacy.vector.2,
                                                                                create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[insecticide.j, 2],
                                                                                                                   recommended.insecticide.dose = insecticide.parameters.df[insecticide.j, 3],
                                                                                                                   threshold.generations = insecticide.parameters.df[insecticide.j, 4],
                                                                                                                   base.efficacy.decay.rate = insecticide.parameters.df[insecticide.j, 5],
                                                                                                                   rapid.decay.rate = insecticide.parameters.df[insecticide.j, 6],
                                                                                                                   deployment.frequency = deployment.frequency))}



}


}#end generation loop
    return(list(sim.array, deployed.vector.1, deployed.vector.2, insecticide.efficacy.vector.1, insecticide.efficacy.vector.2))

  }
