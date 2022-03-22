

run_simulation_advanced = function(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                   irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                   number.of.insecticides = 2,
                                   sd.scaled = FALSE, ##TRUE or FALSE
                                   exposure.scaling.factor = 10,
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
                                   cross.selection = 0,
                                   deployment.interval.llin = 30, #only for combinations
                                   deployment.interval.irs = 10, #only for combinations
                                   probability.only.i.male = 0.7, #only for combinations
                                   probability.only.j.male = 0.2, #only for combinations
                                   probability.both.i.j.male = 0.1, #only for combinations
                                   probability.only.i.female = 0.4, #only for combinations
                                   probability.only.j.female = 0.2, #only for combinations
                                   probability.both.i.j.female = 0.4, #only for combinations
                                   n.cycles = 10,
                                   intervention.coverage.1 = 0.4,
                                   intervention.coverage.2 = 0.4,
                                   intervention.coverage.1.2 = 0.2,
                                   z.sd.intercept = 24.800904,
                                   z.sd.coefficient = 0.396678,
                                   mixture.strategy = "pyrethroid.plus",
                                   llin.insecticides,
                                   irs.insecticides,
                                   min.cross.selection,
                                   max.cross.selection){


  check_for_errors_and_warnings(coverage = coverage,
                                female.exposure = female.exposure,
                                male.exposure = male.exposure,
                                heritability = heritability,
                                dispersal.rate = dispersal.rate,
                                maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                michaelis.menten.slope = michaelis.menten.slope,
                                applied.insecticide.dose = applied.insecticide.dose,
                                recommended.insecticide.dose = recommended.insecticide.dose,
                                starting.refugia.resistance.score = starting.refugia.resistance.score,
                                starting.intervention.resistance.score = starting.intervention.resistance.score,
                                irm.deployment.strategy = irm.deployment.strategy,
                                intervention.coverage.1 = intervention.coverage.1,
                                intervention.coverage.2 = intervention.coverage.2,
                                intervention.coverage.1.2 = intervention.coverage.1.2,
                                probability.only.i.male = probability.only.i.male,
                                probability.only.j.male = probability.only.j.male,
                                probability.both.i.j.male = probability.both.i.j.male,
                                probability.only.i.female = probability.only.i.female,
                                probability.only.j.female = probability.only.j.female,
                                probability.both.i.j.female = probability.both.i.j.female,
                                vector.length = vector.length,
                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance)



  #Start by creating an array (calls the array_named function):
  #dimension 1: site = c("refugia", "intervention"), which hold resistance scores
  #Easier to include both, but refugia won't happen if no dispersal
  #dimension 2: insectide to which the resistance intensity corresponds to
  #dimension 3: generation.

  sim.array = create_starting_array(n.insecticides = number.of.insecticides,
                                    maximum.generations = maximum.generations)

  #Set the starting resistance scores for the insecticides:
  sim.array = set_starting_resistance_scores(sim.array = sim.array,
                                             starting.refugia.resistance.score = starting.refugia.resistance.score,
                                             starting.intervention.resistance.score = starting.intervention.resistance.score,
                                             number.of.insecticides = number.of.insecticides)


  #Make a vector of the available insecticides@
  available.vector = seq(1, number.of.insecticides, by = 1)#Creates a vector of the insecticides that are available for deployment.
  #At the beginning all insecticides are available for deployment.
  withdrawn.vector = c() #creates an empty vector to hold the withdrawn insecticides.

  #Set the withdrawal and return bioassay survival thresholds.
  calc.withdrawal.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                            michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                            bioassay.survival = withdrawal.threshold.value,
                                                                            estimate.precision = 0.0001,
                                                                            minimum.resistance.value = 0,
                                                                            maximum.resistance.value = maximum.resistance.value)

  calc.return.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                        michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                        bioassay.survival = return.threshold.value,
                                                                        estimate.precision = 0.0001,
                                                                        minimum.resistance.value = 0,
                                                                        maximum.resistance.value = maximum.resistance.value)

  #Make a dataframe of the insecticide parameters:
  insecticide.parameters.df = create_insecticide_parameters_dataframe_advanced(number.of.insecticides = number.of.insecticides,
                                                                               applied.insecticide.dose = applied.insecticide.dose,
                                                                               recommended.insecticide.dose = recommended.insecticide.dose,
                                                                               threshold.generation = threshold.generations,
                                                                               base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                                               rapid.decay.rate = rapid.decay.rate,
                                                                               heritability = heritability,
                                                                               female.fitness.cost = female.fitness.cost,
                                                                               male.fitness.cost = male.fitness.cost)



  if(sd.scaled == FALSE){

    if(irm.deployment.strategy == "singles"){

      simulation.results =   wrapper_run_simulation_singles(insecticide.parameters.df = insecticide.parameters.df,
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
                                                            irm.switch.strategy = irm.switch.strategy,
                                                            deployment.frequency = deployment.frequency,
                                                            number.of.insecticides = number.of.insecticides,
                                                            calc.withdrawal.threshold = calc.withdrawal.threshold,
                                                            calc.return.threshold = calc.return.threshold,
                                                            available.vector = available.vector,
                                                            withdrawn.vector = withdrawn.vector,
                                                            min.cross.selection = min.cross.selection,
                                                            max.cross.selection = max.cross.selection)

      #convert to dataframe::

      simulation.df = convert_output_to_dataframe_singles(simulation.results = simulation.results,
                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                          michaelis.menten.slope = michaelis.menten.slope,
                                                          maximum.generations = maximum.generations,
                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                          number.of.insecticides = number.of.insecticides)


    }#end of singles if statement

    if(irm.deployment.strategy == "mixtures"){

      simulation.results =  wrapper_run_simulation_mixtures(insecticide.parameters.df = insecticide.parameters.df,
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
                                                            irm.switch.strategy = irm.switch.strategy,
                                                            deployment.frequency = deployment.frequency,
                                                            number.of.insecticides = number.of.insecticides,
                                                            calc.withdrawal.threshold = calc.withdrawal.threshold,
                                                            calc.return.threshold = calc.return.threshold,
                                                            available.vector = available.vector,
                                                            withdrawn.vector = withdrawn.vector,
                                                            mixture.strategy = mixture.strategy)

      #convert to dataframe::

      simulation.df = convert_output_to_dataframe_mixtures(simulation.results = simulation.results,
                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                          michaelis.menten.slope = michaelis.menten.slope,
                                                          maximum.generations = maximum.generations,
                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                          number.of.insecticides = number.of.insecticides)

    }#end of mixtures if statement

    if(irm.deployment.strategy == "micromosaics"){

      simulation.results =wrapper_run_simulation_micromosaics(insecticide.parameters.df = insecticide.parameters.df,
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
                                                              intervention.coverage.1 = intervention.coverage.1,
                                                              intervention.coverage.2 = intervention.coverage.2,
                                                              irm.switch.strategy = irm.switch.strategy,
                                                              withdrawal.threshold = calc.withdrawal.threshold,
                                                              return.threshold = calc.return.threshold,
                                                              available.vector = available.vector,
                                                              withdrawn.vector = withdrawn.vector)

      #convert to dataframe::

      simulation.df = convert_output_to_dataframe_micromosaics(simulation.results = simulation.results,
                                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                               michaelis.menten.slope = michaelis.menten.slope,
                                                               maximum.generations = maximum.generations,
                                                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                               number.of.insecticides = number.of.insecticides)


    } #end of micromosaics if statement

    if(irm.deployment.strategy == "combinations"){
      simulation.results  = wrapper_run_simulation_combinations(insecticide.parameters.df = insecticide.parameters.df,
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
                                                                deployment.interval.llin = deployment.interval.llin,
                                                                deployment.interval.irs = deployment.interval.irs,
                                                                probability.only.i.male = probability.only.i.male,
                                                                probability.only.j.male = probability.only.j.male,
                                                                probability.both.i.j.male = probability.both.i.j.male,
                                                                probability.only.i.female = probability.only.i.female,
                                                                probability.only.j.female = probability.only.j.female,
                                                                probability.both.i.j.female = probability.both.i.j.female,
                                                                intervention.coverage.llin = intervention.coverage.1,
                                                                intervention.coverage.irs = intervention.coverage.2,
                                                                intervention.coverage.llin.irs = intervention.coverage.1.2,
                                                                number.of.insecticides = number.of.insecticides,
                                                                llin.insecticides = llin.insecticides,
                                                                irs.insecticides = irs.insecticides,
                                                                irm.switch.strategy = irm.switch.strategy,
                                                                withdrawal.threshold = calc.return.threshold,
                                                                return.threshold = calc.return.threshold,
                                                                available.vector = available.vector,
                                                                withdrawn.vector = withdrawn.vector)

      #convert to dataframe::

      simulation.df = convert_output_to_dataframe_combinations(simulation.results = simulation.results,
                                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                               michaelis.menten.slope = michaelis.menten.slope,
                                                               maximum.generations = maximum.generations,
                                                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                               number.of.insecticides = number.of.insecticides)


    } #end of combinations if statement



  }#sd.scaled = FALSE end

  if(sd.scaled == TRUE){

    if(irm.deployment.strategy == "singles"){
      simulation.results= wrapper_run_simulation_singles_sd_scaled(insecticide.parameters.df = insecticide.parameters.df,
                                                                   maximum.generations = maximum.generations,
                                                                   sim.array = sim.array,
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
                                                                   irm.switch.strategy = irm.switch.strategy,
                                                                   deployment.frequency = deployment.frequency,
                                                                   number.of.insecticides = number.of.insecticides,
                                                                   calc.withdrawal.threshold = calc.withdrawal.threshold,
                                                                   calc.return.threshold = calc.return.threshold,
                                                                   available.vector = available.vector,
                                                                   withdrawn.vector = withdrawn.vector,
                                                                   z.sd.intercept = z.sd.intercept,
                                                                   z.sd.coefficient = z.sd.coefficient,
                                                                   min.cross.selection = min.cross.selection,
                                                                   max.cross.selection = max.cross.selection)

      #convert to dataframe::

      simulation.df = convert_output_to_dataframe_singles(simulation.results = simulation.results,
                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                          michaelis.menten.slope = michaelis.menten.slope,
                                                          maximum.generations = maximum.generations,
                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                          number.of.insecticides = number.of.insecticides)

    }#end of singles if statement

    if(irm.deployment.strategy == "mixtures"){

      simulation.results =  wrapper_run_simulation_mixtures_sd_scaled(insecticide.parameters.df = insecticide.parameters.df,
                                                                      maximum.generations = maximum.generations,
                                                                      sim.array = sim.array,
                                                                      z.sd.intercept = z.sd.intercept,
                                                                      z.sd.coefficient = z.sd.coefficient,                                                            vector.length = vector.length,
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
                                                                      irm.switch.strategy = irm.switch.strategy,
                                                                      deployment.frequency = deployment.frequency,
                                                                      number.of.insecticides = number.of.insecticides,
                                                                      calc.withdrawal.threshold = calc.withdrawal.threshold,
                                                                      calc.return.threshold = calc.return.threshold,
                                                                      available.vector = available.vector,
                                                                      withdrawn.vector = withdrawn.vector,
                                                                      mixture.strategy = mixture.strategy)

      #convert to dataframe::

      simulation.df = convert_output_to_dataframe_mixtures(simulation.results = simulation.results,
                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                          michaelis.menten.slope = michaelis.menten.slope,
                                                          maximum.generations = maximum.generations,
                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                          number.of.insecticides = number.of.insecticides)

    }#end of mixtures if statement

    if(irm.deployment.strategy == "micromosaics"){

      simulation.results =wrapper_run_simulation_micromosaics_sd_scaled(insecticide.parameters.df = insecticide.parameters.df,
                                                                        maximum.generations = maximum.generations,
                                                                        sim.array = sim.array,
                                                                        z.sd.intercept = z.sd.intercept,
                                                                        z.sd.coefficient = z.sd.coefficient,
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
                                                                        intervention.coverage.1 = intervention.coverage.1,
                                                                        intervention.coverage.2 = intervention.coverage.2,
                                                                        irm.switch.strategy = irm.switch.strategy,
                                                                        withdrawal.threshold = calc.withdrawal.threshold,
                                                                        return.threshold = calc.return.threshold)


      #convert to dataframe::

      simulation.df = convert_output_to_dataframe_micromosaics(simulation.results = simulation.results,
                                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                               michaelis.menten.slope = michaelis.menten.slope,
                                                               maximum.generations = maximum.generations,
                                                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                               number.of.insecticides = number.of.insecticides)

    } #end of micromosaics if statement

    if(irm.deployment.strategy == "combinations"){
      simulation.results  = wrapper_run_simulation_combinations_sd_scaled(insecticide.parameters.df = insecticide.parameters.df,
                                                                          maximum.generations = maximum.generations,
                                                                          sim.array = sim.array,
                                                                          z.sd.coefficient = z.sd.coefficient,
                                                                          z.sd.intercept = z.sd.intercept,
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
                                                                          deployment.interval.llin = deployment.interval.llin,
                                                                          deployment.interval.irs = deployment.interval.irs,
                                                                          probability.only.i.male = probability.only.i.male,
                                                                          probability.only.j.male = probability.only.j.male,
                                                                          probability.both.i.j.male = probability.both.i.j.male,
                                                                          probability.only.i.female = probability.only.i.female,
                                                                          probability.only.j.female = probability.only.j.female,
                                                                          probability.both.i.j.female = probability.both.i.j.female,
                                                                          intervention.coverage.llin = intervention.coverage.1,
                                                                          intervention.coverage.irs = intervention.coverage.2,
                                                                          intervention.coverage.llin.irs = intervention.coverage.1.2,
                                                                          number.of.insecticides = number.of.insecticides,
                                                                          llin.insecticides = llin.insecticides,
                                                                          irs.insecticides = irs.insecticides,
                                                                          irm.switch.strategy = irm.switch.strategy,
                                                                          withdrawal.threshold = calc.return.threshold,
                                                                          return.threshold = calc.return.threshold,
                                                                          available.vector = available.vector,
                                                                          withdrawn.vector = withdrawn.vector)
      #convert to dataframe::

      simulation.df = convert_output_to_dataframe_combinations(simulation.results = simulation.results,
                                                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                               michaelis.menten.slope = michaelis.menten.slope,
                                                               maximum.generations = maximum.generations,
                                                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                               number.of.insecticides = number.of.insecticides)


    } #end of combinations if statement



  }#sd.scaled = TRUE end

  return(simulation.df)

}
