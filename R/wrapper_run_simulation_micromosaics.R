wrapper_run_simulation_micromosaics = function(insecticide.parameters.df = insecticide.parameters.df,
                                               maximum.generations = maximum.generations,
                                               sim.array = sim.array,
                                               standard.deviation = standard.deviation,
                                               vector.length = vector.length,
                                               female.exposure = female.exposure,
                                               exposure.scaling.factor = exposure.scaling.factor,
                                               coverage = coverage,
                                               dispersal.rate = dispersal.rate,
                                               male.exposure = male.exposure,
                                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                               michaelis.menten.slope = michaelis.menten.slope,
                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                               regression.coefficient = regression.coefficient,
                                               regression.intercept = regression.intercept,
                                               n.cycles = n.cycles,
                                               deployment.frequency = deployment.frequency,
                                               number.of.insecticides = number.of.insecticides,
                                               available.vector = available.vector,
                                               withdrawn.vector = withdrawn.vector,
                                               intervention.coverage.1,
                                               intervention.coverage.2){

  #The first insecticide deployed is always insecticide 1
  insecticide.efficacy.vector.1 = rep(create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[1,2],
                                                                         recommended.insecticide.dose = insecticide.parameters.df[1,3],
                                                                         threshold.generations = insecticide.parameters.df[1,4],
                                                                         base.efficacy.decay.rate = insecticide.parameters.df[1,5],
                                                                         rapid.decay.rate = insecticide.parameters.df[1,6],
                                                                         deployment.frequency = deployment.frequency), (maximum.generations/deployment.frequency))


  #The first insecticide deployed is always insecticide 2
  insecticide.efficacy.vector.2 = rep(create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[2,2],
                                                                         recommended.insecticide.dose = insecticide.parameters.df[2,3],
                                                                         threshold.generations = insecticide.parameters.df[2,4],
                                                                         base.efficacy.decay.rate = insecticide.parameters.df[2,5],
                                                                         rapid.decay.rate = insecticide.parameters.df[2,6],
                                                                         deployment.frequency = deployment.frequency), (maximum.generations/deployment.frequency))

  for(generation in 2:maximum.generations){

    male.selection.differentials = perform_male_micromosaic_smooth(insecticide.coverage.1 = intervention.coverage.1,
                                                                   insecticide.coverage.2 = intervention.coverage.2,
                                                                   trait.mean.1 = sim.array["intervention", 1, generation-1],
                                                                   trait.mean.2 = sim.array["intervention", 2, generation-1],
                                                                   standard.deviation = standard.deviation,
                                                                   vector.length = vector.length,
                                                                   female.exposure = female.exposure,
                                                                   male.exposure = male.exposure,
                                                                   current.insecticide.efficacy.1 = insecticide.efficacy.vector.1[generation],
                                                                   current.insecticide.efficacy.2 = insecticide.efficacy.vector.2[generation],
                                                                   regression.coefficient = regression.coefficient,
                                                                   regression.intercept = regression.intercept,
                                                                   half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                   michaelis.menten.slope = michaelis.menten.slope,
                                                                   maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                   male.natural.survival.probability = 1)

    tracked.resistance = multiple_gonotrophic_cycles_micromosaic_dispersal(intervention.trait.mean.i = sim.array['intervention', 1, generation-1],
                                                                           intervention.trait.mean.j = sim.array['intervention', 2, generation-1],
                                                                           refugia.trait.mean.i = sim.array['refugia', 1, generation-1],
                                                                           refugia.trait.mean.j = sim.array['refugia', 2, generation-1],
                                                                           standard.deviation = standard.deviation,
                                                                           vector.length = vector.length,
                                                                           female.exposure = female.exposure,
                                                                           exposure.scaling.factor = exposure.scaling.factor,
                                                                           coverage = coverage,
                                                                           dispersal.rate = dispersal.rate,
                                                                           male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.selection.differentials[[1]],
                                                                                                                                                                        exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                        male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[1]),
                                                                           male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.selection.differentials[[2]],
                                                                                                                                                                        exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                        male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[2]),
                                                                           male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', 1, generation-1],
                                                                                                                                                     male.fitness.cost = insecticide.parameters.df$male.fitness.cost[1]),
                                                                           male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', 2, generation-1],
                                                                                                                                                     male.fitness.cost = insecticide.parameters.df$male.fitness.cost[2]),
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
                                                                           current.insecticide.efficacy.i = insecticide.efficacy.vector.1[generation],
                                                                           current.insecticide.efficacy.j = insecticide.efficacy.vector.2[generation],
                                                                           coverage.i = intervention.coverage.1,
                                                                           coverage.j = intervention.coverage.2)

    sim.array['intervention', 1, generation] = tracked.resistance[[1]]
    sim.array['refugia', 1, generation] = tracked.resistance[[2]]
    sim.array['intervention', 2, generation] = tracked.resistance[[3]]
    sim.array['refugia', 2, generation] = tracked.resistance[[4]]
  }


  return(list(sim.array, insecticide.efficacy.vector.1, insecticide.efficacy.vector.2))

}
