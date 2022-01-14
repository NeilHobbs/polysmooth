##This is the development version


run_simulation_micro_mosaics_dispersal_v2 = function(coverage = 0.8,
                                                  natural.survival = 0.7,
                                                  dispersal.rate = 0.99,
                                                  female.exposure = 0.7,
                                                  heritability = 0.3,
                                                  starting.intervention.resistance.score = 10,
                                                  starting.refugia.resistance.score = 0,
                                                  n.cycles = 1,
                                                  male.selection.differential.intervention = 0,
                                                  male.selection.differential.refugia = 0,
                                                  exposure.scaling.factor = 10,
                                                  vector.length = 1000,
                                                  standard.deviation = 30,
                                                  half.population.bioassay.survival.resistance = 900,
                                                  michaelis.menten.slope = 1,
                                                  maximum.bioassay.survival.proportion = 1,
                                                  regression.coefficient = 0.48,
                                                  regression.intercept = 0.15,
                                                  current.insecticide.efficacy = 1,
                                                  maximum.generations = 500){

  #Start by creating an array (calls the array_named function):
  #dimension 1: site = c("refugia", "intervention"), which hold resistance scores
  #Easier to include both, but refugia won't happen if no dispersal
  #dimension 2: insectide to which the resistance intensity corresponds to
  #dimension 3: generation.
  sim.array = create_starting_array(n.insecticides = 1,
                                    maximum.generations = maximum.generations)

  #Set the starting resistance scores for the insecticides:
  sim.array = set_starting_resistance_scores(sim.array = sim.array,
                                             starting.refugia.resistance.score = starting.refugia.resistance.score,
                                             starting.intervention.resistance.score = starting.intervention.resistance.score,
                                             number.of.insecticides = 1)


  for(generation in 2:maximum.generations){


   update.means =  multiple_gonotrophic_cycles_with_dispersal_v2(coverage = coverage,
                                                             natural.survival = natural.survival,
                                                             dispersal.rate = dispersal.rate,
                                                             female.exposure = female.exposure,
                                                             refugia.trait.mean = sim.array["refugia", 1, generation-1],
                                                             heritability = heritability,
                                                             intervention.trait.mean = sim.array["intervention", 1, generation-1],
                                                             n.cycles = n.cycles,
                                                             male.selection.differential.intervention = male.selection.differential.intervention,
                                                             male.selection.differential.refugia = male.selection.differential.refugia,
                                                             exposure.scaling.factor = exposure.scaling.factor,
                                                             vector.length = vector.length,
                                                             standard.deviation = standard.deviation,
                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                             regression.coefficient = regression.coefficient,
                                                             regression.intercept = regression.intercept,
                                                             current.insecticide.efficacy = current.insecticide.efficacy)

    sim.array["intervention", 1, generation] = update.means[[1]]
    sim.array["refugia", 1, generation] = update.means[[2]]
  }



  #ensure the simulation array is return after running
  #need to develop an quick and easy way to turn array into dataframes for plotting purposes
  return(sim.array)

}






