# insecticide.parameters.df,
# maximum.generations,
# sim.array,
# standard.deviation,
# vector.length,
# female.exposure,
# exposure.scaling.factor,
# coverage,
# dispersal.rate,
# male.exposure ,
# maximum.bioassay.survival.proportion,
# michaelis.menten.slope ,
# half.population.bioassay.survival.resistance,
# regression.coefficient,
# regression.intercept,
# n.cycles,
# deployment.frequency,
# deployment.interval.llin,
# deployment.interval.irs,
# probability.only.i.male, i is the llin insecticide
# probability.only.j.male, j is the irs insecticide
# probability.both.i.j.male,
# probability.only.i.female,
# probability.only.j.female,
# probability.both.i.j.female,
# intervention.coverage.1,
# intervention.coverage.2,
# intervention.coverage.1.2,
# number.of.insecticides,
# llin.insecticides = a vector containing the numbers associated with insecticides deployed as LLINs (eg. c(1, 3))
# irs.insecticides = a vector containing the numbers associated with insecticides deployed as IRS (e.g. c(2, 4, 5))


wrapper_run_simulation_combinations = function(insecticide.parameters.df,
                                               maximum.generations,
                                               sim.array,
                                               standard.deviation,
                                               vector.length,
                                               female.exposure,
                                               exposure.scaling.factor,
                                               coverage,
                                               dispersal.rate,
                                               male.exposure ,
                                               maximum.bioassay.survival.proportion,
                                               michaelis.menten.slope ,
                                               half.population.bioassay.survival.resistance,
                                               regression.coefficient,
                                               regression.intercept,
                                               n.cycles,
                                               deployment.frequency,
                                               deployment.interval.llin,
                                               deployment.interval.irs,
                                               probability.only.i.male,
                                               probability.only.j.male,
                                               probability.both.i.j.male,
                                               probability.only.i.female,
                                               probability.only.j.female,
                                               probability.both.i.j.female,
                                               intervention.coverage.llin,
                                               intervention.coverage.irs,
                                               intervention.coverage.llin.irs,
                                               number.of.insecticides,
                                               llin.insecticides,
                                               irs.insecticides,
                                               irm.switch.strategy,
                                               withdrawal.threshold,
                                               return.threshold,
                                               available.vector,
                                               withdrawn.vector){


  #Starting conditions

  #The first insecticide deployed is always insecticide 1

        #This would be the LLIN
  insecticide.efficacy.vector.llin = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df$applied.insecticide.doses[1],
                                                                         recommended.insecticide.dose = insecticide.parameters.df$recommended.insecticide.doses[1],
                                                                         threshold.generations = insecticide.parameters.df$threshold.generations[1],
                                                                         base.efficacy.decay.rate = insecticide.parameters.df$base.efficacy.decay.rates[1],
                                                                         rapid.decay.rate = insecticide.parameters.df$rapid.decay.rates[1],
                                                                         deployment.frequency = deployment.interval.llin)

      #This would be the IRS
  #The first insecticide deployed is always insecticide 2
  insecticide.efficacy.vector.irs = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df$applied.insecticide.doses[2],
                                                                         recommended.insecticide.dose = insecticide.parameters.df$recommended.insecticide.doses[2],
                                                                         threshold.generations = insecticide.parameters.df$threshold.generations[2],
                                                                         base.efficacy.decay.rate = insecticide.parameters.df$base.efficacy.decay.rates[2],
                                                                         rapid.decay.rate = insecticide.parameters.df$rapid.decay.rates[2],
                                                                         deployment.frequency = deployment.interval.irs)


  which.insecticide.is.llin = rep(1, deployment.interval.llin)
  which.insecticide.is.irs = rep(2, deployment.interval.irs)

  for(generation in 2:maximum.generations){
    for(insecticide in 1:number.of.insecticides){

    if(insecticide == which.insecticide.is.llin[generation]){#if the insecticide tracked is the LLIN insecticide

    insecticide.j = which.insecticide.is.irs[generation]

    male.insecticide.intervention.i =  perform_male_combination_insecticide_selection_differential_smooth(coverage = coverage,
                                                                                                          coverage.i = intervention.coverage.llin,
                                                                                                          coverage.j = intervention.coverage.irs,
                                                                                                          coverage.ij = intervention.coverage.llin.irs,
                                                                                                          probability.only.i = probability.only.i.male,
                                                                                                          probability.only.j = probability.only.j.male,
                                                                                                          probability.both.i.j = probability.both.i.j.male,
                                                                                                          trait.mean.1 = sim.array['intervention', insecticide, generation-1],
                                                                                                          trait.mean.2 = sim.array['intervention', insecticide.j, generation-1],
                                                                                                          standard.deviation = standard.deviation,
                                                                                                          vector.length = vector.length,
                                                                                                          female.exposure = female.exposure,
                                                                                                          male.exposure = male.exposure,
                                                                                                          current.insecticide.efficacy.1 = insecticide.efficacy.vector.llin[generation],
                                                                                                          current.insecticide.efficacy.2 = insecticide.efficacy.vector.irs[generation],
                                                                                                          regression.coefficient = regression.coefficient,
                                                                                                          regression.intercept = regression.intercept,
                                                                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                          michaelis.menten.slope = michaelis.menten.slope,
                                                                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[1]]

    male.insecticide.intervention.j =  perform_male_combination_insecticide_selection_differential_smooth(coverage = coverage,
                                                                                                          coverage.i = intervention.coverage.llin,
                                                                                                          coverage.j = intervention.coverage.irs,
                                                                                                          coverage.ij = intervention.coverage.llin.irs,
                                                                                                          probability.only.i = probability.only.i.male,
                                                                                                          probability.only.j = probability.only.j.male,
                                                                                                          probability.both.i.j = probability.both.i.j.male,
                                                                                                          trait.mean.1 = sim.array['intervention', insecticide, generation-1],
                                                                                                          trait.mean.2 = sim.array['intervention', insecticide.j, generation-1],
                                                                                                          standard.deviation = standard.deviation,
                                                                                                          vector.length = vector.length,
                                                                                                          female.exposure = female.exposure,
                                                                                                          male.exposure = male.exposure,
                                                                                                          current.insecticide.efficacy.1 = insecticide.efficacy.vector.llin[generation],
                                                                                                          current.insecticide.efficacy.2 = insecticide.efficacy.vector.irs[generation],
                                                                                                          regression.coefficient = regression.coefficient,
                                                                                                          regression.intercept = regression.intercept,
                                                                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                          michaelis.menten.slope = michaelis.menten.slope,
                                                                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[2]]



    tracked = multiple_gonotrophic_cycles_combination_dispersal(intervention.trait.mean.i = sim.array['intervention', insecticide, generation-1],
                                                                intervention.trait.mean.j = sim.array['intervention', insecticide.j, generation-1],
                                                                refugia.trait.mean.i = sim.array['refugia', insecticide, generation-1],
                                                                refugia.trait.mean.j = sim.array['refugia', insecticide.j, generation-1],
                                                                standard.deviation = standard.deviation,
                                                                vector.length = vector.length,
                                                                female.exposure = female.exposure,
                                                                exposure.scaling.factor = exposure.scaling.factor,
                                                                coverage = coverage,
                                                                dispersal.rate = dispersal.rate,
                                                                male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.intervention.i,
                                                                                                                                                             exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                             male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential =  male.insecticide.intervention.j, # male.insecticide.intervention.j,
                                                                                                                                                             exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                             male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[insecticide.j]),

                                                                male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                                                                          male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]
                                                                ),
                                                                male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide.j, generation-1],
                                                                                                                                          male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide.j]
                                                                ),
                                                                female.fitness.cost.i = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                female.fitness.cost.j = insecticide.parameters.df$female.fitness.cost[insecticide.j],
                                                                heritability.i = insecticide.parameters.df$heritability[insecticide],
                                                                heritability.j = insecticide.parameters.df$heritability[insecticide.j],
                                                                n.cycles = n.cycles,
                                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                michaelis.menten.slope = michaelis.menten.slope,
                                                                maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                regression.coefficient = regression.coefficient,
                                                                regression.intercept = regression.intercept,
                                                                current.insecticide.efficacy.i = insecticide.efficacy.vector.llin[generation],
                                                                current.insecticide.efficacy.j = insecticide.efficacy.vector.irs[generation],
                                                                coverage.i = intervention.coverage.llin,
                                                                coverage.j = intervention.coverage.irs,
                                                                coverage.ij = intervention.coverage.llin.irs,
                                                                probability.only.i = probability.only.i.female,
                                                                probability.only.j = probability.only.j.female,
                                                                probability.both.i.j = probability.both.i.j.female)


    sim.array['intervention', insecticide, generation] = tracked[[1]]
    sim.array['refugia', insecticide, generation] = tracked[[2]]
    }
      if(insecticide == which.insecticide.is.irs[generation]){#if the insecticide tracked is the IRS insecticide

        insecticide.i = which.insecticide.is.llin[generation]

        male.insecticide.intervention.i =  perform_male_combination_insecticide_selection_differential_smooth(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.llin,
                                                                                                              coverage.j = intervention.coverage.irs,
                                                                                                              coverage.ij = intervention.coverage.llin.irs,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', insecticide.i, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', insecticide, generation-1],
                                                                                                              standard.deviation = standard.deviation,
                                                                                                              vector.length = vector.length,
                                                                                                              female.exposure = female.exposure,
                                                                                                              male.exposure = male.exposure,
                                                                                                              current.insecticide.efficacy.1 = insecticide.efficacy.vector.llin[generation],
                                                                                                              current.insecticide.efficacy.2 = insecticide.efficacy.vector.irs[generation],
                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                              regression.intercept = regression.intercept,
                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[1]]

        male.insecticide.intervention.j =  perform_male_combination_insecticide_selection_differential_smooth(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.llin,
                                                                                                              coverage.j = intervention.coverage.irs,
                                                                                                              coverage.ij = intervention.coverage.llin.irs,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', insecticide.i, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', insecticide, generation-1],
                                                                                                              standard.deviation = standard.deviation,
                                                                                                              vector.length = vector.length,
                                                                                                              female.exposure = female.exposure,
                                                                                                              male.exposure = male.exposure,
                                                                                                              current.insecticide.efficacy.1 = insecticide.efficacy.vector.llin[generation],
                                                                                                              current.insecticide.efficacy.2 = insecticide.efficacy.vector.irs[generation],
                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                              regression.intercept = regression.intercept,
                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[2]]



        tracked = multiple_gonotrophic_cycles_combination_dispersal(intervention.trait.mean.i = sim.array['intervention', insecticide.i, generation-1],
                                                                    intervention.trait.mean.j = sim.array['intervention', insecticide, generation-1],
                                                                    refugia.trait.mean.i = sim.array['refugia', insecticide.i, generation-1],
                                                                    refugia.trait.mean.j = sim.array['refugia', insecticide, generation-1],
                                                                    standard.deviation = standard.deviation,
                                                                    vector.length = vector.length,
                                                                    female.exposure = female.exposure,
                                                                    exposure.scaling.factor = exposure.scaling.factor,
                                                                    coverage = coverage,
                                                                    dispersal.rate = dispersal.rate,
                                                                    male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.intervention.i,
                                                                                                                                                                 exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                 male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[insecticide.i]),
                                                                    male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential =  male.insecticide.intervention.j, # male.insecticide.intervention.j,
                                                                                                                                                                 exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                 male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[insecticide]),

                                                                    male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide.i, generation-1],
                                                                                                                                              male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide.i]
                                                                    ),
                                                                    male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                                                                              male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]
                                                                    ),
                                                                    female.fitness.cost.i = insecticide.parameters.df$female.fitness.cost[insecticide.i],
                                                                    female.fitness.cost.j = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                    heritability.i = insecticide.parameters.df$heritability[insecticide.i],
                                                                    heritability.j = insecticide.parameters.df$heritability[insecticide],
                                                                    n.cycles = n.cycles,
                                                                    half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                    michaelis.menten.slope = michaelis.menten.slope,
                                                                    maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                    regression.coefficient = regression.coefficient,
                                                                    regression.intercept = regression.intercept,
                                                                    current.insecticide.efficacy.i = insecticide.efficacy.vector.llin[generation],
                                                                    current.insecticide.efficacy.j = insecticide.efficacy.vector.irs[generation],
                                                                    coverage.i = intervention.coverage.llin,
                                                                    coverage.j = intervention.coverage.irs,
                                                                    coverage.ij = intervention.coverage.llin.irs,
                                                                    probability.only.i = probability.only.i.female,
                                                                    probability.only.j = probability.only.j.female,
                                                                    probability.both.i.j = probability.both.i.j.female)


        sim.array['intervention', insecticide, generation] = tracked[[3]]
        sim.array['refugia', insecticide, generation] = tracked[[4]]
      }

      if(insecticide != which.insecticide.is.irs[generation] &
         insecticide != which.insecticide.is.llin[generation]){#if the insecticide tracked is not deployed

        insecticide.i = which.insecticide.is.llin[generation]
        insecticide.j = which.insecticide.is.irs[generation]

        male.insecticide.intervention.i =  perform_male_combination_insecticide_selection_differential_smooth(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.llin,
                                                                                                              coverage.j = intervention.coverage.irs,
                                                                                                              coverage.ij = intervention.coverage.llin.irs,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', insecticide.i, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', insecticide.j, generation-1],
                                                                                                              standard.deviation = standard.deviation,
                                                                                                              vector.length = vector.length,
                                                                                                              female.exposure = female.exposure,
                                                                                                              male.exposure = male.exposure,
                                                                                                              current.insecticide.efficacy.1 = insecticide.efficacy.vector.llin[generation],
                                                                                                              current.insecticide.efficacy.2 = insecticide.efficacy.vector.irs[generation],
                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                              regression.intercept = regression.intercept,
                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[1]]

        male.insecticide.intervention.j =  perform_male_combination_insecticide_selection_differential_smooth(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.llin,
                                                                                                              coverage.j = intervention.coverage.irs,
                                                                                                              coverage.ij = intervention.coverage.llin.irs,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', insecticide.i, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', insecticide.j, generation-1],
                                                                                                              standard.deviation = standard.deviation,
                                                                                                              vector.length = vector.length,
                                                                                                              female.exposure = female.exposure,
                                                                                                              male.exposure = male.exposure,
                                                                                                              current.insecticide.efficacy.1 = insecticide.efficacy.vector.llin[generation],
                                                                                                              current.insecticide.efficacy.2 = insecticide.efficacy.vector.irs[generation],
                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                              regression.intercept = regression.intercept,
                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[2]]



        tracked = multiple_gonotrophic_cycles_combinations_dispersal_not_deployed(intervention.trait.mean.i = sim.array['intervention', insecticide.i, generation-1],
                                                                    intervention.trait.mean.j = sim.array['intervention', insecticide.j, generation-1],
                                                                    refugia.trait.mean.i = sim.array['refugia', insecticide.i, generation-1],
                                                                    refugia.trait.mean.j = sim.array['refugia', insecticide.j, generation-1],
                                                                    standard.deviation = standard.deviation,
                                                                    vector.length = vector.length,
                                                                    female.exposure = female.exposure,
                                                                    exposure.scaling.factor = exposure.scaling.factor,
                                                                    coverage = coverage,
                                                                    dispersal.rate = dispersal.rate,
                                                                    male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.intervention.i,
                                                                                                                                                                 exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                 male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[insecticide.i]),
                                                                    male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential =  male.insecticide.intervention.j, # male.insecticide.intervention.j,
                                                                                                                                                                 exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                 male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[insecticide.j]),

                                                                    male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide.i, generation-1],
                                                                                                                                              male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide.i]
                                                                    ),
                                                                    male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide.j, generation-1],
                                                                                                                                              male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide.j]
                                                                    ),
                                                                    female.fitness.cost.i = insecticide.parameters.df$female.fitness.cost[insecticide.i],
                                                                    female.fitness.cost.j = insecticide.parameters.df$female.fitness.cost[insecticide.j],
                                                                    heritability.i = insecticide.parameters.df$heritability[insecticide.i],
                                                                    heritability.j = insecticide.parameters.df$heritability[insecticide.j],
                                                                    n.cycles = n.cycles,
                                                                    half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                    michaelis.menten.slope = michaelis.menten.slope,
                                                                    maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                    regression.coefficient = regression.coefficient,
                                                                    regression.intercept = regression.intercept,
                                                                    current.insecticide.efficacy.i = insecticide.efficacy.vector.llin[generation],
                                                                    current.insecticide.efficacy.j = insecticide.efficacy.vector.irs[generation],
                                                                    coverage.i = intervention.coverage.llin,
                                                                    coverage.j = intervention.coverage.irs,
                                                                    coverage.ij = intervention.coverage.llin.irs,
                                                                    probability.only.i = probability.only.i.female,
                                                                    probability.only.j = probability.only.j.female,
                                                                    probability.both.i.j = probability.both.i.j.female,
                                                                    intervention.trait.mean.tracked = sim.array['intervention', insecticide, generation-1],
                                                                    refugia.trait.mean.tracked = sim.array['refugia', insecticide, generation-1],
                                                                    male.differential.intervention.tracked = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                         male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]
                                                                    ),
                                                                    male.differential.refugia.tracked = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                    male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]
                                                                    ),
                                                                    female.fitness.cost.tracked = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                    heritability.tracked = insecticide.parameters.df$heritability[insecticide]
        )


        sim.array['intervention', insecticide, generation] = tracked[[1]]
        sim.array['refugia', insecticide, generation] = tracked[[2]]
      }


    }

    if(generation < maximum.generations){
      if(generation %% deployment.interval.irs == 0){
        if(irm.switch.strategy == "irs.rotation"){
          update.insecticide.info = irm_strategy_combinations_rotate_irs(
            number.of.insecticides = number.of.insecticides,
            current.generation = generation,
            withdrawal.threshold = calc.withdrawal.threshold,
            return.threshold = calc.return.threshold,
            simulation.array = sim.array,
            available.vector = available.vector,
            withdrawn.vector = withdrawn.vector,
            current.irs.insecticide = which.insecticide.is.irs[generation],
            deployment.vector = which.insecticide.is.irs,
            irs.insecticides = irs.insecticides,
            deployment.interval.irs = deployment.interval.irs)

        #update.insectide.info[[1]] is the vector of the available insecticides
        #update.insecticide.info[[2]] is the vector of the withdrawn insecticides
        #update.insecticide.info[[3]] is the vector of the whole deployment =c(previous.deployment, new.deployment)
      }
      if(generation %% deployment.interval.irs == 0){available.vector = update.insecticide.info[[1]]}
      if(generation %% deployment.interval.irs == 0){withdrawn.vector = update.insecticide.info[[2]]}
      if(generation %% deployment.interval.irs == 0){which.insecticide.is.irs = update.insecticide.info[[3]]}
      if(generation %% deployment.interval.irs == 0){current.irs = which.insecticide.is.irs[generation+1]}
      if(generation %% deployment.interval.irs == 0){insecticide.efficacy.vector.irs = c(insecticide.efficacy.vector.irs,
                                                                                  create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[current.irs, 2],
                                                                                                                     recommended.insecticide.dose = insecticide.parameters.df[current.irs, 3],
                                                                                                                     threshold.generations = insecticide.parameters.df[current.irs, 4],
                                                                                                                     base.efficacy.decay.rate = insecticide.parameters.df[current.irs, 5],
                                                                                                                     rapid.decay.rate = insecticide.parameters.df[current.irs, 6],
                                                                                                                     deployment.frequency = deployment.interval.irs))



      if(generation %% deployment.interval.llin == 0){insecticide.efficacy.vector.llin = c(insecticide.efficacy.vector.llin,
                                                                                      create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[1, 2],
                                                                                                                         recommended.insecticide.dose = insecticide.parameters.df[1, 3],
                                                                                                                         threshold.generations = insecticide.parameters.df[1, 4],
                                                                                                                         base.efficacy.decay.rate = insecticide.parameters.df[1, 5],
                                                                                                                         rapid.decay.rate = insecticide.parameters.df[1, 6],
                                                                                                                         deployment.frequency = deployment.interval.llin))}
      }


      #A break point to stop simuation if there is no insecticide deployed
      #if(is.na(deployed.insecticide[generation])){break}
}
    }



  }


  return(list(sim.array, insecticide.efficacy.vector.llin, insecticide.efficacy.vector.irs, which.insecticide.is.irs, which.insecticide.is.llin))

}
