wrapper_run_simulation_singles_sd_scaled = function(insecticide.parameters.df,
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
                                                    z.sd.intercept,
                                                    z.sd.coefficient,
                                                    min.cross.selection,
                                                    max.cross.selection,
                                                    between.gonotrophic.survival){




  deployed.insecticide = rep(1, times = deployment.frequency)#Always start with insecticide 1.
  #This is fine as all insecticides have equivalent properties.



  #The first insecticide deployed is always insecticide 1
  insecticide.efficacy.vector = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[1,2],
                                                                   recommended.insecticide.dose = insecticide.parameters.df[1,3],
                                                                   threshold.generations = insecticide.parameters.df[1,4],
                                                                   base.efficacy.decay.rate = insecticide.parameters.df[1,5],
                                                                   rapid.decay.rate = insecticide.parameters.df[1,6],
                                                                   deployment.frequency = deployment.frequency)


  insecticide.info = list(available.vector, withdrawn.vector, deployed.insecticide)

  cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = number.of.insecticides,
                                                       min.cross.selection = min.cross.selection,
                                                       max.cross.selection = max.cross.selection)

  #Also worth considering turning the for generation and for insecticide loops into functions,
  #as the code is other wise very large and chunky and therefore complicated to edit and adapt.
  #start at generation 2, as generation 1 has intensities set at 0.
  for(generation in 2:maximum.generations){

    #Stop the simulation if there is no insecticide being deployed anymore.
    if(is.na(deployed.insecticide[generation])){break}else{

      for(insecticide in 1:number.of.insecticides){ #track the resistance intensity for each insecticide
        ##                                                   #ask whether insecticide is the same as deployed insecticide


        if(insecticide == deployed.insecticide[generation]){

          #The inclusion of the population suppression as a result of insecticidal deployment is causing some weird problems - appears to be restting to 0 after each new deployment.

          tracked.resistance = multiple_gonotrophic_cycles_singles_dispersal_sd_scaled(intervention.trait.mean.i = sim.array['intervention', insecticide, generation-1],
                                                                                       refugia.trait.mean.i = sim.array['refugia', insecticide, generation-1],
                                                                                       exposure.scaling.factor = exposure.scaling.factor,
                                                                                       female.exposure = female.exposure,
                                                                                       vector.length = vector.length,
                                                                                       coverage = coverage,
                                                                                       dispersal.rate = dispersal.rate,
                                                                                       male.differential.intervention.i = wrapper_calculate_male_insecticide_fitness_selection_differential_sd_scaled(male.trait.mean = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                                                                      female.insecticide.exposure = female.exposure,
                                                                                                                                                                                                      male.insecticide.exposure = male.exposure,
                                                                                                                                                                                                      z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                      z.sd.coefficient = z.sd.coefficient,
                                                                                                                                                                                                      male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide],

                                                                                                                                                                                                      vector.length = vector.length,
                                                                                                                                                                                                      maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                      michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                      half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                                      regression.coefficient = regression.coefficient,
                                                                                                                                                                                                      regression.intercept = regression.intercept,
                                                                                                                                                                                                      current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                                                                                                                                                                      exposure.scaling.factor = exposure.scaling.factor),
                                                                                       male.differential.refugia.i = (sd_changes_with_z(current.z = sim.array['refugia', insecticide, generation-1],
                                                                                                                                        z.sd.intercept = z.sd.intercept,
                                                                                                                                        z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                                       female.fitness.cost.i = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                                       heritability.i = insecticide.parameters.df$heritability[insecticide],
                                                                                       n.cycles = n.cycles,
                                                                                       half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                       michaelis.menten.slope = michaelis.menten.slope,
                                                                                       maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                       regression.coefficient = regression.coefficient,
                                                                                       regression.intercept = regression.intercept,
                                                                                       current.insecticide.efficacy.i = insecticide.efficacy.vector[generation],
                                                                                       z.sd.intercept = z.sd.intercept,
                                                                                       z.sd.coefficient = z.sd.coefficient,
                                                                                       between.gonotrophic.survival = between.gonotrophic.survival)


          sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
          sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]

        }

        if(insecticide != deployed.insecticide[generation]){

          cross.selection.j.i = cross.selection.matrix[deployed.insecticide[generation], insecticide]

          tracked.resistance = multiple_gonotrophic_cycles_singles_dispersal_not_deployed_sd_scaled(intervention.trait.mean.i = sim.array['intervention', insecticide, generation-1],
                                                                                                    intervention.trait.mean.j = sim.array['intervention', deployed.insecticide[generation], generation-1],
                                                                                                    refugia.trait.mean.i = sim.array['refugia', insecticide, generation-1],
                                                                                                    refugia.trait.mean.j =  sim.array['refugia', deployed.insecticide[generation], generation-1],
                                                                                                    vector.length = vector.length,
                                                                                                    female.exposure = female.exposure,
                                                                                                    exposure.scaling.factor = exposure.scaling.factor,
                                                                                                    coverage = coverage,
                                                                                                    dispersal.rate = dispersal.rate,
                                                                                                    male.differential.intervention.i = (sd_changes_with_z(current.z = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                                                                                          z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                                                    male.differential.intervention.j = wrapper_calculate_male_insecticide_fitness_selection_differential_sd_scaled(male.trait.mean = sim.array['intervention', deployed.insecticide[generation], generation-1],
                                                                                                                                                                                                                   female.insecticide.exposure = female.exposure,
                                                                                                                                                                                                                   male.insecticide.exposure = male.exposure,
                                                                                                                                                                                                                   z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                   z.sd.coefficient = z.sd.coefficient,
                                                                                                                                                                                                                   male.fitness.cost = insecticide.parameters.df$male.fitness.cost[deployed.insecticide[generation]],
                                                                                                                                                                                                                   vector.length = vector.length,
                                                                                                                                                                                                                   maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                                   michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                                   half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                                                   regression.coefficient = regression.coefficient,
                                                                                                                                                                                                                   regression.intercept = regression.intercept,
                                                                                                                                                                                                                   current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                                                                                                                                                                                   exposure.scaling.factor = exposure.scaling.factor),
                                                                                                    male.differential.refugia.i = (sd_changes_with_z(current.z = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                     z.sd.intercept = z.sd.intercept,
                                                                                                                                                     z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                                                    male.differential.refugia.j = (sd_changes_with_z(current.z = sim.array['refugia', deployed.insecticide[generation], generation-1],
                                                                                                                                                     z.sd.intercept = z.sd.intercept,
                                                                                                                                                     z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[deployed.insecticide[insecticide]]),

                                                                                                    female.fitness.cost.i = insecticide.parameters.df$male.fitness.cost[insecticide],
                                                                                                    female.fitness.cost.j = insecticide.parameters.df$male.fitness.cost[deployed.insecticide[generation]],
                                                                                                    heritability.i = insecticide.parameters.df$heritability[insecticide],
                                                                                                    heritability.j = insecticide.parameters.df$heritability[deployed.insecticide[generation]],
                                                                                                    n.cycles = n.cycles,
                                                                                                    half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                    michaelis.menten.slope = michaelis.menten.slope,
                                                                                                    maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                    regression.coefficient = regression.coefficient,
                                                                                                    regression.intercept = regression.intercept,
                                                                                                    current.insecticide.efficacy.j = insecticide.efficacy.vector[generation],
                                                                                                    z.sd.coefficient = z.sd.coefficient,
                                                                                                    z.sd.intercept = z.sd.intercept,
                                                                                                    cross.selection.j.i = cross.selection.j.i,
                                                                                                    between.gonotrophic.survival = between.gonotrophic.survival)
          sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
          sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]


          #end insecticide not deployed
        }
        #NEED TO FIGURE OUT A WAY TO MAKE ALL THIS LESS MESSY AND MORE READABLE!!!!!!!!!!!
      }#end of for insecticide loop

      if(generation < maximum.generations){
        update.insecticide.info = if(generation %% deployment.frequency == 0){
          if(irm.switch.strategy == "rotation"){
            irm_strategy_rotation(
              number.of.insecticides = number.of.insecticides,
              current.generation = generation,
              withdrawal.threshold = calc.withdrawal.threshold,
              return.threshold = calc.return.threshold,
              simulation.array = sim.array,
              available.vector = available.vector,
              withdrawn.vector = withdrawn.vector,
              current.insecticide = deployed.insecticide[generation],
              deployment.frequency = deployment.frequency,
              deployment.vector = deployed.insecticide)} else{
                if(irm.switch.strategy == "sequence"){
                  irm_strategy_sequence(
                    number.of.insecticides = number.of.insecticides,
                    current.generation = generation,
                    withdrawal.threshold = calc.withdrawal.threshold,
                    return.threshold = calc.return.threshold,
                    simulation.array = sim.array,
                    available.vector = available.vector,
                    withdrawn.vector = withdrawn.vector,
                    current.insecticide = deployed.insecticide[generation],
                    deployment.frequency = deployment.frequency,
                    deployment.vector = deployed.insecticide)

                }else{ if(irm.switch.strategy == "adaptive.rotation"){
                  irm_strategy_adaptive_rotations(
                    number.of.insecticides = number.of.insecticides,
                    current.generation = generation,
                    withdrawal.threshold = calc.withdrawal.threshold,
                    return.threshold = calc.return.threshold,
                    simulation.array = sim.array,
                    available.vector = available.vector,
                    withdrawn.vector = withdrawn.vector,
                    current.insecticide = deployed.insecticide[generation],
                    deployment.frequency = deployment.frequency,
                    deployment.vector = deployed.insecticide)

                }}
              }

          #update.insectide.info[[1]] is the vector of the available insecticides
          #update.insecticide.info[[2]] is the vector of the withdrawn insecticides
          #update.insecticide.info[[3]] is the vector of the whole deployment =c(previous.deployment, new.deployment)
        }
        if(generation %% deployment.frequency == 0){available.vector = update.insecticide.info[[1]]}
        if(generation %% deployment.frequency == 0){withdrawn.vector = update.insecticide.info[[2]]}
        if(generation %% deployment.frequency == 0){deployed.insecticide = update.insecticide.info[[3]]}
        if(generation %% deployment.frequency == 0){currently.deployed.insecticide = deployed.insecticide[generation+1]}

        if(generation %% deployment.frequency == 0){insecticide.efficacy.vector = c(insecticide.efficacy.vector,
                                                                                    create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[currently.deployed.insecticide, 2],
                                                                                                                       recommended.insecticide.dose = insecticide.parameters.df[currently.deployed.insecticide, 3],
                                                                                                                       threshold.generations = insecticide.parameters.df[currently.deployed.insecticide, 4],
                                                                                                                       base.efficacy.decay.rate = insecticide.parameters.df[currently.deployed.insecticide, 5],
                                                                                                                       rapid.decay.rate = insecticide.parameters.df[currently.deployed.insecticide, 6],
                                                                                                                       deployment.frequency = deployment.frequency))}


        #A break point to stop simuation if there is no insecticide deployed
        #if(is.na(deployed.insecticide[generation])){break}

      }
    }
  }#end of for(generation) loop

  return(list(sim.array, deployed.insecticide, insecticide.efficacy.vector))

}
