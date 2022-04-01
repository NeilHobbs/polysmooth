#' @title Run the Insecticide Resistance Management Simulation
#'
#' @description Overall wrapper function for the running of the polysmooth model. This is the
#' function that should be used for running any simulations regardless of strategy.
#'
#' @param maximum.generations = the maximum number of generations in the simulation
#' @param standard.deviation = the standard deviation of the Normal distribution of the polygenic resistance score
#' @param vector.length = the length of the vector which contains the values in the Normal distribution
#' @param female.exposure = The probability a female mosquito will encounter the insecticide
#' @param exposure.scaling.factor =  A factor which converts the exposure to the selection differential
#' @param coverage = The proportion of the area (and mosquito population) emerging in the intervention site
#' @param dispersal.rate = The proportion of female mosquitoes dispersing each gonotrophic cycle
#' @param male.exposure = The probability a male mosquito encounters the insecticide as a proportion of the female exposre
#' @param maximum.bioassay.survival.proportion = The maximum bioassay survival probability
#' @param michaelis.menten.slope = The slope of the Michaelis-Menten equation
#' @param half.population.bioassay.survival.resistance = The polygenic resistance score which gives 50% bioassay survival
#' @param regression.coefficient = The regression coefficient of a linear model between hut survival ~ bioassay survival
#' @param regression.intercept = the regression intercept of a linear model between hut survival ~ bioassay survival
#' @param n.cycles = The maximum number of gonotrophic cycles allow in the model
#' @param deployment.interval.llin = The deployment interval for the LLIN in mosquito generations
#' @param deployment.interval.irs = The deployment interval for the IRS in mosquito generations
#' @param probability.only.i.male  = The probability a male mosquito which enters a house with both LLIN and IRS only encounters the LLIN insecticide
#' @param probability.only.j.male = The probability a male mosquito which enters a house with both LLIN and IRS only encounters the IRS insecticide
#' @param probability.both.i.j.male = The probability a male mosquito which enters a house with both LLIN and IRS  encounters both the LLIN and IRS insecticides
#' @param probability.only.i.female = The probability a female mosquito which enters a house with both LLIN and IRS only encounters the LLIN insecticide
#' @param probability.only.j.female = The probability a female mosquito which enters a house with both LLIN and IRS only encounters the IRS insecticide
#' @param probability.both.i.j.female = The probability a female mosquito which enters a house with both LLIN and IRS  encounters both the LLIN and IRS insecticides
#' @param intervention.coverage.1 = The proportion of insecticide treated houses with only the LLIN insecticide
#' @param intervention.coverage.2 = The proportion of insecticide treated houses with only the IRS insecticide
#' @param intervention.coverage.1.2 = The proportion of insectiicde treated houses with both the LLIN and IRS insecticides
#' @param number.of.insecticides = The total number of insecticides in the simulation
#' @param llin.insecticides = a vector containing the numbers associated with insecticides deployed as LLINs (eg. c(1))
#' @param irs.insecticides = a vector containing the numbers associated with insecticides deployed as IRS (e.g. c(2, 3, 4, 5))
#' @param min.cross.selection = The minimum cross selection between insecticides
#' @param max.cross.selection = The maximim cross selection between insecticides
#' @param irm.switch.strategy = the strategy for how insecticides will be switched
#' @param withdrawal.threshold = The withdrawal threshold for when an insecticide can no longer be deployed
#' @param return.threshold = The return threshold for when a previously withdrawn insecticide can be made re-available
#' @param z.sd.intercept =
#' @param z.sd.coefficient =
#' @param mixture.strategy =
#' @param withdrawal.threshold.value =
#' @param return.threshold.value
#' @param deployment.frequency
#' @param maximum.resistance.value = 25000,
#' @param starting.refugia.resistance.score = 0,
#' @param starting.intervention.resistance.score = 0,
#' @param applied.insecticide.dose = 1,
#' @param recommended.insecticide.dose = 1,
#' @param threshold.generations = 15,
#' @param base.efficacy.decay.rate = 0.015,
#' @param rapid.decay.rate = 0.08,
#' @param female.fitness.cost = 0,
#' @param male.fitness.cost = 0,
#' @param irm.deployment.strategy = How the insecticides are deployed, one of "singles", "mixtures", "micromosaics" or "combinations"
#' @param irm.switch.strategy = How the insecticide switches are made and will depend on the irm.deployment.strategy
#' @param sd.scaled
#' @param heritability

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
                                                            mixture.strategy = mixture.strategy,
                                                            min.cross.selection = min.cross.selection,
                                                            max.cross.selection = max.cross.selection)

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
                                                              withdrawn.vector = withdrawn.vector,
                                                              min.cross.selection = min.cross.selection,
                                                              max.cross.selection = max.cross.selection)

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
                                                                withdrawn.vector = withdrawn.vector,
                                                                min.cross.selection = min.cross.selection,
                                                                max.cross.selection = max.cross.selection)

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
                                                                      mixture.strategy = mixture.strategy,
                                                                      min.cross.selection = min.cross.selection,
                                                                      max.cross.selection = max.cross.selection)

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
                                                                        return.threshold = calc.return.threshold,
                                                                        min.cross.selection = min.cross.selection,
                                                                        max.cross.selection = max.cross.selection)


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
                                                                          withdrawn.vector = withdrawn.vector,
                                                                          min.cross.selection = min.cross.selection,
                                                                          max.cross.selection = max.cross.selection)
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
