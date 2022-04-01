#' @title Run the simulation with LLIN and IRS Combination

#' @description Run a simulation when the irm.deployment.strategy = "combinations". This branch
#' assumes the standard deviation of the polygenic resistance score remains constant.

#' @param insecticide.parameters.df = a dataframe containing the information for the insecticide efficacies and decay rates
#' @param maximum.generations = the maximum number of generations in the simulation
#' @param sim.array = the array which holds the simulation results
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
#' @param available.vector = A vector containing the currently available insecticides
#' @param withdrawn.vector = A vector containing the currently withdrawn insecticides

wrapper_run_simulation_combinations_sd_scaled = function(insecticide.parameters.df,
                                               maximum.generations,
                                               sim.array,
                                               z.sd.intercept,
                                               z.sd.coefficient,
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
                                               withdrawn.vector,
                                               min.cross.selection,
                                               max.cross.selection){

  cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = number.of.insecticides,
                                                       min.cross.selection = min.cross.selection,
                                                       max.cross.selection = max.cross.selection)
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


  which.insecticide.is.llin = rep(1, maximum.generations) #no changes made to LLIN so just have deployed continuously for now
  which.insecticide.is.irs = rep(2, deployment.interval.irs)

  for(generation in 2:maximum.generations){
    for(insecticide in 1:number.of.insecticides){

      if(insecticide == which.insecticide.is.llin[generation]){#if the insecticide tracked is the LLIN insecticide

        insecticide.j = which.insecticide.is.irs[generation]

        cross.selection.i.j = cross.selection.matrix[insecticide, insecticide.j]
        cross.selection.j.i = cross.selection.matrix[insecticide.j, insecticide]

        male.insecticide.intervention.i =  perform_male_combination_insecticide_selection_differential_smooth_sd_scaled(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.llin,
                                                                                                              coverage.j = intervention.coverage.irs,
                                                                                                              coverage.ij = intervention.coverage.llin.irs,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', insecticide, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', insecticide.j, generation-1],
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient,
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

        male.insecticide.intervention.j =  perform_male_combination_insecticide_selection_differential_smooth_sd_scaled(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.llin,
                                                                                                              coverage.j = intervention.coverage.irs,
                                                                                                              coverage.ij = intervention.coverage.llin.irs,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', insecticide, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', insecticide.j, generation-1],
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient,                                                                                                              vector.length = vector.length,
                                                                                                              female.exposure = female.exposure,
                                                                                                              male.exposure = male.exposure,
                                                                                                              current.insecticide.efficacy.1 = insecticide.efficacy.vector.llin[generation],
                                                                                                              current.insecticide.efficacy.2 = insecticide.efficacy.vector.irs[generation],
                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                              regression.intercept = regression.intercept,
                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[2]]



        tracked = multiple_gonotrophic_cycles_combination_dispersal_sd_scaled(intervention.trait.mean.i = sim.array['intervention', insecticide, generation-1],
                                                                    intervention.trait.mean.j = sim.array['intervention', insecticide.j, generation-1],
                                                                    refugia.trait.mean.i = sim.array['refugia', insecticide, generation-1],
                                                                    refugia.trait.mean.j = sim.array['refugia', insecticide.j, generation-1],
                                                                    z.sd.intercept = z.sd.intercept,
                                                                    z.sd.coefficient = z.sd.coefficient,                                                                    vector.length = vector.length,
                                                                    female.exposure = female.exposure,
                                                                    exposure.scaling.factor = exposure.scaling.factor,
                                                                    coverage = coverage,
                                                                    dispersal.rate = dispersal.rate,
                                                                    male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.intervention.i,
                                                                                                                                                                 exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                 male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                          z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide])),
                                                                    male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential =  male.insecticide.intervention.j, # male.insecticide.intervention.j,
                                                                                                                                                                 exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                 male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', insecticide.j, generation-1],
                                                                                                                                                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                          z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide.j])),

                                                                    male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                                                                              male.fitness.cost =(sd_changes_with_z(current.z = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                                                    z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                    z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide])),
                                                                    male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide.j, generation-1],
                                                                                                                                              male.fitness.cost = (sd_changes_with_z(current.z = sim.array['refugia', insecticide.j, generation-1],
                                                                                                                                                                                     z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                     z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide.j])),
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
                                                                    probability.both.i.j = probability.both.i.j.female,
                                                                    cross.selection.i.j = cross.selection.i.j,
                                                                    cross.selection.j.i = cross.selection.j.i)


        sim.array['intervention', insecticide, generation] = tracked[[1]]
        sim.array['refugia', insecticide, generation] = tracked[[2]]
      }
      if(insecticide == which.insecticide.is.irs[generation]){#if the insecticide tracked is the IRS insecticide

        insecticide.i = which.insecticide.is.llin[generation]

        cross.selection.i.j = cross.selection.matrix[insecticide.i, insecticide]
        cross.selection.j.i = cross.selection.matrix[insecticide, insecticide.i]

        male.insecticide.intervention.i =  perform_male_combination_insecticide_selection_differential_smooth_sd_scaled(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.llin,
                                                                                                              coverage.j = intervention.coverage.irs,
                                                                                                              coverage.ij = intervention.coverage.llin.irs,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', insecticide.i, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', insecticide, generation-1],
                                                                                                              vector.length = vector.length,
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient,
                                                                                                              female.exposure = female.exposure,
                                                                                                              male.exposure = male.exposure,
                                                                                                              current.insecticide.efficacy.1 = insecticide.efficacy.vector.llin[generation],
                                                                                                              current.insecticide.efficacy.2 = insecticide.efficacy.vector.irs[generation],
                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                              regression.intercept = regression.intercept,
                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[1]]

        male.insecticide.intervention.j =  perform_male_combination_insecticide_selection_differential_smooth_sd_scaled(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.llin,
                                                                                                              coverage.j = intervention.coverage.irs,
                                                                                                              coverage.ij = intervention.coverage.llin.irs,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', insecticide.i, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', insecticide, generation-1],
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient,                                                                                                              vector.length = vector.length,
                                                                                                              female.exposure = female.exposure,
                                                                                                              male.exposure = male.exposure,
                                                                                                              current.insecticide.efficacy.1 = insecticide.efficacy.vector.llin[generation],
                                                                                                              current.insecticide.efficacy.2 = insecticide.efficacy.vector.irs[generation],
                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                              regression.intercept = regression.intercept,
                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[2]]



        tracked = multiple_gonotrophic_cycles_combination_dispersal_sd_scaled(intervention.trait.mean.i = sim.array['intervention', insecticide.i, generation-1],
                                                                    intervention.trait.mean.j = sim.array['intervention', insecticide, generation-1],
                                                                    refugia.trait.mean.i = sim.array['refugia', insecticide.i, generation-1],
                                                                    refugia.trait.mean.j = sim.array['refugia', insecticide, generation-1],
                                                                    z.sd.intercept = z.sd.intercept,
                                                                    z.sd.coefficient = z.sd.coefficient,                                                                    vector.length = vector.length,
                                                                    female.exposure = female.exposure,
                                                                    exposure.scaling.factor = exposure.scaling.factor,
                                                                    coverage = coverage,
                                                                    dispersal.rate = dispersal.rate,
                                                                    male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.intervention.i,
                                                                                                                                                                 exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                 male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', insecticide.i, generation-1],
                                                                                                                                                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                          z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide.i])),
                                                                    male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.intervention.j,
                                                                                                                                                                 exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                 male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                          z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide])),

                                                                    male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide.i, generation-1],
                                                                                                                                              male.fitness.cost =(sd_changes_with_z(current.z = sim.array['refugia', insecticide.i, generation-1],
                                                                                                                                                                                    z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                    z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide.i])
                                                                    ),
                                                                    male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                                                                              male.fitness.cost = (sd_changes_with_z(current.z = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                                                     z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                     z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide])
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
                                                                    probability.both.i.j = probability.both.i.j.female,
                                                                    cross.selection.i.j = cross.selection.i.j,
                                                                    cross.selection.j.i = cross.selection.j.i)


        sim.array['intervention', insecticide, generation] = tracked[[3]]
        sim.array['refugia', insecticide, generation] = tracked[[4]]
      }

      if(insecticide != which.insecticide.is.irs[generation] &
         insecticide != which.insecticide.is.llin[generation]){#if the insecticide tracked is not deployed

        insecticide.i = which.insecticide.is.llin[generation]
        insecticide.j = which.insecticide.is.irs[generation]

        cross.selection.i.k = cross.selection.matrix[insecticide.i, insecticide]
        cross.selection.j.k = cross.selection.matrix[insecticide.j, insecticide]

        male.insecticide.intervention.i =  perform_male_combination_insecticide_selection_differential_smooth_sd_scaled(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.llin,
                                                                                                              coverage.j = intervention.coverage.irs,
                                                                                                              coverage.ij = intervention.coverage.llin.irs,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', insecticide.i, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', insecticide.j, generation-1],
                                                                                                              vector.length = vector.length,
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient,
                                                                                                              female.exposure = female.exposure,
                                                                                                              male.exposure = male.exposure,
                                                                                                              current.insecticide.efficacy.1 = insecticide.efficacy.vector.llin[generation],
                                                                                                              current.insecticide.efficacy.2 = insecticide.efficacy.vector.irs[generation],
                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                              regression.intercept = regression.intercept,
                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)[[1]]

        male.insecticide.intervention.j =  perform_male_combination_insecticide_selection_differential_smooth_sd_scaled(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.llin,
                                                                                                              coverage.j = intervention.coverage.irs,
                                                                                                              coverage.ij = intervention.coverage.llin.irs,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', insecticide.i, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', insecticide.j, generation-1],
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient,
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



        tracked = multiple_gonotrophic_cycles_combinations_dispersal_not_deployed_sd_scaled(intervention.trait.mean.i = sim.array['intervention', insecticide.i, generation-1],
                                                                                  intervention.trait.mean.j = sim.array['intervention', insecticide.j, generation-1],
                                                                                  refugia.trait.mean.i = sim.array['refugia', insecticide.i, generation-1],
                                                                                  refugia.trait.mean.j = sim.array['refugia', insecticide.j, generation-1],
                                                                                  z.sd.intercept = z.sd.intercept,
                                                                                  z.sd.coefficient = z.sd.coefficient,
                                                                                  vector.length = vector.length,
                                                                                  female.exposure = female.exposure,
                                                                                  exposure.scaling.factor = exposure.scaling.factor,
                                                                                  coverage = coverage,
                                                                                  dispersal.rate = dispersal.rate,
                                                                                  male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.intervention.i,
                                                                                                                                                                               exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                               male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', insecticide.i, generation-1],
                                                                                                                                                                                                                                        z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                                        z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide.i])),
                                                                                  male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.intervention.j,
                                                                                                                                                                               exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                               male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', insecticide.j, generation-1],
                                                                                                                                                                                                                                        z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                                        z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide.j])),

                                                                                  male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide.i, generation-1],
                                                                                                                                                            male.fitness.cost =(sd_changes_with_z(current.z = sim.array['refugia', insecticide.i, generation-1],
                                                                                                                                                                                                  z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                  z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide.i])
                                                                                  ),
                                                                                  male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide.j, generation-1],
                                                                                                                                                            male.fitness.cost =(sd_changes_with_z(current.z = sim.array['refugia', insecticide.j, generation-1],
                                                                                                                                                                                                  z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                  z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide.j])
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
                                                                                                                                                                       male.fitness.cost =(sd_changes_with_z(current.z = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                                                                             z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                             z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide])
                                                                                  ),
                                                                                  male.differential.refugia.tracked = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                                  male.fitness.cost =(sd_changes_with_z(current.z = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                                                                        z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                        z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide])
                                                                                  ),
                                                                                  female.fitness.cost.tracked = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                                  heritability.tracked = insecticide.parameters.df$heritability[insecticide],
                                                                                  cross.selection.i.k = cross.selection.i.k,
                                                                                  cross.selection.j.k = cross.selection.j.k
        )


        sim.array['intervention', insecticide, generation] = tracked[[1]]
        sim.array['refugia', insecticide, generation] = tracked[[2]]
      }


    }

    ##Insecticide Deployment Switching Section::::::::
    if(generation < maximum.generations){
      if(generation %% deployment.interval.irs == 0){
        if(irm.switch.strategy == "rotate.irs"){
          update.deployment.info = irm_strategy_combinations_rotate_irs(number.of.insecticides = number.of.insecticides,
                                                                        irs.insecticides = irs.insecticides,
                                                                        available.vector = available.vector,
                                                                        withdrawn.vector = withdrawn.vector,
                                                                        withdrawal.threshold = withdrawal.threshold,
                                                                        return.threshold = return.threshold,
                                                                        current.generation = generation,
                                                                        simulation.array = sim.array,
                                                                        current.irs.insecticide = which.insecticide.is.irs[generation],
                                                                        deployment.vector = which.insecticide.is.irs,
                                                                        deployment.interval.irs = deployment.interval.irs)} else{
                                                                          if(irm.switch.strategy == "sequence.irs"){
                                                                            update.deployment.info = irm_strategy_combinations_sequence_irs(number.of.insecticides = number.of.insecticides,
                                                                                                                                            irs.insecticides = irs.insecticides,
                                                                                                                                            available.vector = available.vector,
                                                                                                                                            withdrawn.vector = withdrawn.vector,
                                                                                                                                            withdrawal.threshold = withdrawal.threshold,
                                                                                                                                            return.threshold = return.threshold,
                                                                                                                                            current.generation = generation,
                                                                                                                                            simulation.array = sim.array,
                                                                                                                                            current.irs.insecticide = which.insecticide.is.irs[generation],
                                                                                                                                            deployment.vector = which.insecticide.is.irs,
                                                                                                                                            deployment.interval.irs = deployment.interval.irs)
                                                                          }
                                                                        }

        #update.insectide.info[[1]] is the vector of the available insecticides
        #update.insecticide.info[[2]] is the vector of the withdrawn insecticides
        #update.insecticide.info[[3]] is the vector of the whole deployment =c(previous.deployment, new.deployment)

        if(generation %% deployment.interval.irs == 0){available.vector = update.deployment.info[[1]]}
        if(generation %% deployment.interval.irs == 0){withdrawn.vector = update.deployment.info[[2]]}
        if(generation %% deployment.interval.irs == 0){which.insecticide.is.irs = update.deployment.info[[3]]}
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
                                                                                                                                deployment.frequency = deployment.interval.llin))
        }
        }


        #A break point to stop simuation if there is no insecticide deployed
        #if(is.na(deployed.insecticide[generation])){break}
      }
    }



  }


  return(list(sim.array, insecticide.efficacy.vector.llin, insecticide.efficacy.vector.irs, which.insecticide.is.irs, which.insecticide.is.llin))

}
