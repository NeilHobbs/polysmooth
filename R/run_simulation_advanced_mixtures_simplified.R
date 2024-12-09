#'@description This function is only for the simultion of scenarios with 2 insecticides, where the simulations are run
#' to completion. Such as when looking at the advantage/disadvantage of mixtures dosing/decay/resistance etc.



run_simulation_advanced_mixtures_simplified = function(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                                       irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                                       number.of.insecticides = 2,
                                                       sd.scaled = TRUE, ##TRUE or FALSE
                                                       exposure.scaling.factor = 10,
                                                       female.fitness.cost = 0,
                                                       male.fitness.cost = 0,
                                                       female.exposure = 0.7,
                                                       male.exposure = 0.7,
                                                       heritability = 0.3,
                                                       dispersal.rate = 0.3,
                                                       coverage = 0.8,
                                                       standard.deviation = 50,
                                                       vector.length = 200,
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
                                                       n.cycles = 1,
                                                       intervention.coverage.1 = 0.4,
                                                       intervention.coverage.2 = 0.4,
                                                       intervention.coverage.1.2 = 0.2,
                                                       z.sd.intercept = 24.800904,
                                                       z.sd.coefficient = 0.396678,
                                                       mixture.strategy = "pyrethroid.plus",
                                                       llin.insecticides,
                                                       irs.insecticides,
                                                       min.cross.selection = 0,
                                                       max.cross.selection = 0){


  # check_for_errors_and_warnings(coverage = coverage,
  #                               female.exposure = female.exposure,
  #                               male.exposure = male.exposure,
  #                               heritability = heritability,
  #                               dispersal.rate = dispersal.rate,
  #                               maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
  #                               michaelis.menten.slope = michaelis.menten.slope,
  #                               applied.insecticide.dose = applied.insecticide.dose,
  #                               recommended.insecticide.dose = recommended.insecticide.dose,
  #                               starting.refugia.resistance.score = starting.refugia.resistance.score,
  #                               starting.intervention.resistance.score = starting.intervention.resistance.score,
  #                               irm.deployment.strategy = irm.deployment.strategy,
  #                               intervention.coverage.1 = intervention.coverage.1,
  #                               intervention.coverage.2 = intervention.coverage.2,
  #                               intervention.coverage.1.2 = intervention.coverage.1.2,
  #                               probability.only.i.male = probability.only.i.male,
  #                               probability.only.j.male = probability.only.j.male,
  #                               probability.both.i.j.male = probability.both.i.j.male,
  #                               probability.only.i.female = probability.only.i.female,
  #                               probability.only.j.female = probability.only.j.female,
  #                               probability.both.i.j.female = probability.both.i.j.female,
  #                               vector.length = vector.length,
  #                               half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance)



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
  #Note, solo deployment with number.of.insecticides = 1 behaves weird if dataframe is not with 2 insecticides. But has no impact on
  #the simulation being run.
  insecticide.parameters.df = create_insecticide_parameters_dataframe_advanced(number.of.insecticides = ifelse(number.of.insecticides == 1,
                                                                                                               yes = 2,
                                                                                                               no = number.of.insecticides),
                                                                               applied.insecticide.dose = applied.insecticide.dose,
                                                                               recommended.insecticide.dose = recommended.insecticide.dose,
                                                                               threshold.generation = threshold.generations,
                                                                               base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                                               rapid.decay.rate = rapid.decay.rate,
                                                                               heritability = heritability,
                                                                               female.fitness.cost = female.fitness.cost,
                                                                               male.fitness.cost = male.fitness.cost)



      simulation.results =  wrapper_run_simulation_mixtures_sd_scaled(insecticide.parameters.df = insecticide.parameters.df,
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



      simulation.df = convert_output_to_dataframe_mixtures(simulation.results = simulation.results,
                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                           michaelis.menten.slope = michaelis.menten.slope,
                                                           maximum.generations = maximum.generations,
                                                           maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                           number.of.insecticides = number.of.insecticides)



      return(simulation.df)
      }
