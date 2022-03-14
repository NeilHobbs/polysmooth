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
                                               intervention.coverage.1,
                                               intervention.coverage.2,
                                               intervention.coverage.1.2){


  #The first insecticide deployed is always insecticide 1
  insecticide.efficacy.vector.1 = rep(create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df$applied.insecticide.doses[1],
                                                                         recommended.insecticide.dose = insecticide.parameters.df$recommended.insecticide.doses[1],
                                                                         threshold.generations = insecticide.parameters.df$threshold.generations[1],
                                                                         base.efficacy.decay.rate = insecticide.parameters.df$base.efficacy.decay.rates[1],
                                                                         rapid.decay.rate = insecticide.parameters.df$rapid.decay.rates[1],
                                                                         deployment.frequency = deployment.interval.llin), (maximum.generations/deployment.interval.llin))


  #The first insecticide deployed is always insecticide 2
  insecticide.efficacy.vector.2 = rep(create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df$applied.insecticide.doses[2],
                                                                         recommended.insecticide.dose = insecticide.parameters.df$recommended.insecticide.doses[2],
                                                                         threshold.generations = insecticide.parameters.df$threshold.generations[2],
                                                                         base.efficacy.decay.rate = insecticide.parameters.df$base.efficacy.decay.rates[2],
                                                                         rapid.decay.rate = insecticide.parameters.df$rapid.decay.rates[2],
                                                                         deployment.frequency = deployment.interval.irs), (maximum.generations/deployment.interval.irs))

  for(generation in 2:maximum.generations){

    male.insecticide.intervention.i =  perform_male_combination_insecticide_selection_differential_smooth(coverage = coverage,
                                                                                                          coverage.i = intervention.coverage.1,
                                                                                                          coverage.j = intervention.coverage.1,
                                                                                                          coverage.ij = intervention.coverage.1.2,
                                                                                                          probability.only.i = probability.only.i.male,
                                                                                                          probability.only.j = probability.only.j.male,
                                                                                                          probability.both.i.j = probability.both.i.j.male,
                                                                                                          trait.mean.1 = sim.array['intervention', 1, generation-1],
                                                                                                          trait.mean.2 = sim.array['intervention', 2, generation-1],
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
                                                                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[1]]

    male.insecticide.intervention.j =  perform_male_combination_insecticide_selection_differential_smooth(coverage = coverage,
                                                                                                          coverage.i = intervention.coverage.1,
                                                                                                          coverage.j = intervention.coverage.1,
                                                                                                          coverage.ij = intervention.coverage.1.2,
                                                                                                          probability.only.i = probability.only.i.male,
                                                                                                          probability.only.j = probability.only.j.male,
                                                                                                          probability.both.i.j = probability.both.i.j.male,
                                                                                                          trait.mean.1 = sim.array['intervention', 1, generation-1],
                                                                                                          trait.mean.2 = sim.array['intervention', 2, generation-1],
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
                                                                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[2]]



    tracked = multiple_gonotrophic_cycles_combination_dispersal(intervention.trait.mean.i = sim.array['intervention', 1, generation-1],
                                                                intervention.trait.mean.j = sim.array['intervention', 2, generation-1],
                                                                refugia.trait.mean.i = sim.array['refugia', 1, generation-1],
                                                                refugia.trait.mean.j = sim.array['refugia', 2, generation-1],
                                                                standard.deviation = standard.deviation,
                                                                vector.length = vector.length,
                                                                female.exposure = female.exposure,
                                                                exposure.scaling.factor = exposure.scaling.factor,
                                                                coverage = coverage,
                                                                dispersal.rate = dispersal.rate,
                                                                male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.intervention.i,
                                                                                                                                                             exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                             male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[1]),
                                                                male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential =  male.insecticide.intervention.j, # male.insecticide.intervention.j,
                                                                                                                                                             exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                             male.fitness.selection.differential = insecticide.parameters.df$male.fitness.cost[2]),

                                                                male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', 1, generation-1],
                                                                                                                                          male.fitness.cost = insecticide.parameters.df$male.fitness.cost[1]
                                                                ),
                                                                male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', 2, generation-1],
                                                                                                                                          male.fitness.cost = insecticide.parameters.df$male.fitness.cost[2]
                                                                ),
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
                                                                coverage.j = intervention.coverage.2,
                                                                coverage.ij = intervention.coverage.1.2,
                                                                probability.only.i = probability.only.i.female,
                                                                probability.only.j = probability.only.j.female,
                                                                probability.both.i.j = probability.both.i.j.female)


    sim.array['intervention', 1, generation] = tracked[[1]]
    sim.array['refugia', 1, generation] = tracked[[2]]
    sim.array['intervention', 2, generation] = tracked[[3]]
    sim.array['refugia', 2, generation] = tracked[[4]]


  }


  return(list(sim.array, insecticide.efficacy.vector.1, insecticide.efficacy.vector.2))

}
