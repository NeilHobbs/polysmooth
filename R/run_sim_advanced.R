

run_simulation_advanced = function(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                   irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
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
                                   mixture.strategy, #only needed if deployment.type = "mixtures"
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
                                   intervention.coverage.1.2 = 0.2){


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

  number.of.insecticides = 2

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

              tracked.resistance = multiple_gonotrophic_cycles_single_dispersal(coverage = coverage,
                                                                                dispersal.rate = dispersal.rate,
                                                                                female.exposure = female.exposure,
                                                                                refugia.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                heritability = insecticide.parameters.df$heritability[insecticide],
                                                                                intervention.trait.mean = sim.array['intervention', insecticide, generation-1],
                                                                                n.cycles = n.cycles,
                                                                                male.selection.differential.intervention = wrapper_calculate_male_insecticide_fitness_selection_differential(male.trait.mean = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                                                             female.insecticide.exposure = female.exposure,
                                                                                                                                                                                             male.insecticide.exposure = male.exposure,
                                                                                                                                                                                             standard.deviation = standard.deviation,
                                                                                                                                                                                             male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide],
                                                                                                                                                                                             vector.length = vector.length,
                                                                                                                                                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                             regression.coefficient = regression.coefficient,
                                                                                                                                                                                             regression.intercept = regression.intercept,
                                                                                                                                                                                             current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                                                                                                                                                             exposure.scaling.factor = exposure.scaling.factor),
                                                                                male.selection.differential.refugia = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                                  male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                                female.fitness.cost.intervention = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                                female.fitness.cost.refugia = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                                exposure.scaling.factor = exposure.scaling.factor,
                                                                                vector.length = vector.length,
                                                                                standard.deviation = standard.deviation,
                                                                                half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                michaelis.menten.slope = michaelis.menten.slope,
                                                                                maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                regression.coefficient = regression.coefficient,
                                                                                regression.intercept = regression.intercept,
                                                                                current.insecticide.efficacy = insecticide.efficacy.vector[generation])

              sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
              sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]

            }

            if(insecticide != deployed.insecticide[generation]){

              tracked.resistance = multiple_gonotrophic_cycles_singles_dispersal_not_deployed(intervention.trait.mean.i = sim.array['intervention', insecticide, generation-1],
                                                                                              intervention.trait.mean.j = sim.array['intervention', deployed.insecticide[generation], generation-1],
                                                                                              refugia.trait.mean.i = sim.array['refugia', insecticide, generation-1],
                                                                                              refugia.trait.mean.j =  sim.array['refugia', deployed.insecticide[generation], generation-1],
                                                                                              standard.deviation = standard.deviation,
                                                                                              vector.length = vector.length,
                                                                                              female.exposure = female.exposure,
                                                                                              exposure.scaling.factor = exposure.scaling.factor,
                                                                                              coverage = coverage,
                                                                                              dispersal.rate = dispersal.rate,
                                                                                              male.differential.intervention.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                                             male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                                              male.differential.intervention.j = wrapper_calculate_male_insecticide_fitness_selection_differential(male.trait.mean = sim.array['intervention', deployed.insecticide[generation], generation-1],
                                                                                                                                                                                                   female.insecticide.exposure = female.exposure,
                                                                                                                                                                                                   male.insecticide.exposure = male.exposure,
                                                                                                                                                                                                   standard.deviation = standard.deviation,
                                                                                                                                                                                                   male.fitness.cost = insecticide.parameters.df$male.fitness.cost[deployed.insecticide[generation]],
                                                                                                                                                                                                   vector.length = vector.length,
                                                                                                                                                                                                   maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                   michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                   half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                                   regression.coefficient = regression.coefficient,
                                                                                                                                                                                                   regression.intercept = regression.intercept,
                                                                                                                                                                                                   current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                                                                                                                                                                   exposure.scaling.factor = exposure.scaling.factor),
                                                                                              male.differential.refugia.i = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                                        male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                                              male.differential.refugia.j = wrapper_male_fitness_selection_differential(male.trait.mean = sim.array['refugia', deployed.insecticide[generation], generation-1],
                                                                                                                                                                        male.fitness.cost = insecticide.parameters.df$male.fitness.cost[deployed.insecticide[insecticide]]),
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
                                                                                              current.insecticide.efficacy.j = insecticide.efficacy.vector[generation])
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

                    }
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
    }#end of singles if statement

    if(irm.deployment.strategy == "mixtures"){

      mixture.df = select_mixing_stategy(mixture.strategy = "mix.sequential.discrete",
                                         number.of.insecticides = number.of.insecticides)

      mixture.id = rep(mixture.df[1, 1], times = deployment.frequency)#first row, first column
      mixture.part.1 = rep(mixture.df[1, 2], times = deployment.frequency)#first row, second column
      mixture.part.2 = rep(mixture.df[1, 3], times = deployment.frequency)#first row, third column

      #The dataframe that holds the mixture deployment information
      deployed.mixture = data.frame(mixture.id, mixture.part.1, mixture.part.2)



      #create vectors to hold the insecticide efficacies
      deployed.mixture$insecticide.efficacy.vector.part.1 = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[deployed.mixture[1,2], 2],
                                                                                               recommended.insecticide.dose = insecticide.parameters.df[deployed.mixture[1, 2],3],
                                                                                               threshold.generations = insecticide.parameters.df[deployed.mixture[1,2], 4],
                                                                                               base.efficacy.decay.rate = insecticide.parameters.df[deployed.mixture[1,2], 5],
                                                                                               rapid.decay.rate = insecticide.parameters.df[deployed.mixture[1,2], 6],
                                                                                               deployment.frequency = deployment.frequency)

      deployed.mixture$insecticide.efficacy.vector.part.2 = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[deployed.mixture[1,3] ,2],
                                                                                               recommended.insecticide.dose = insecticide.parameters.df[deployed.mixture[1,3] ,3],
                                                                                               threshold.generations = insecticide.parameters.df[deployed.mixture[1,3] ,4],
                                                                                               base.efficacy.decay.rate = insecticide.parameters.df[deployed.mixture[1,3] ,5],
                                                                                               rapid.decay.rate = insecticide.parameters.df[deployed.mixture[1,3] ,6],
                                                                                               deployment.frequency = deployment.frequency)
      available.mixtures = mixture.df #at the start all mixtures will be available
      mixture.info = list(available.mixtures, available.vector, withdrawn.vector, deployed.mixture)


      for(generation in 2:maximum.generations){

        ##It should be noted that this model currently only works with mixtures for 2-insecticide simulations. It does not currently work for the inclusion of
        #multiple mixtures.

        #Stop the simulation if there is no insecticide being deployed anymore.
        if(is.na(deployed.mixture$mixture.id[generation])){break}else{

          tracked.resistance =   multiple_gonotrophic_cycles_mixture_dispersal(intervention.trait.mean.i = sim.array['intervention', 1, generation-1],
                                                                               intervention.trait.mean.j = sim.array['intervention', 1, generation-1],
                                                                               refugia.trait.mean.i = sim.array['refugia', 1, generation-1],
                                                                               refugia.trait.mean.j = sim.array['refugia', 1, generation-1],
                                                                               standard.deviation = standard.deviation,
                                                                               vector.length = vector.length,
                                                                               female.exposure = female.exposure,
                                                                               exposure.scaling.factor = exposure.scaling.factor,
                                                                               coverage = coverage,
                                                                               dispersal.rate = dispersal.rate,
                                                                               male.differential.intervention.i = wrapper_calculate_male_insecticide_fitness_selection_differential_mixtures(male.trait.mean = sim.array['intervention', 1, generation-1],
                                                                                                                                                                                             female.insecticide.exposure = female.exposure,
                                                                                                                                                                                             male.insecticide.exposure = male.exposure,
                                                                                                                                                                                             standard.deviation = standard.deviation,
                                                                                                                                                                                             male.fitness.cost = insecticide.parameters.df$male.fitness.cost[1],
                                                                                                                                                                                             vector.length = vector.length,
                                                                                                                                                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                             regression.coefficient = regression.coefficient,
                                                                                                                                                                                             regression.intercept = regression.intercept,
                                                                                                                                                                                             current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.1[generation],
                                                                                                                                                                                             exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                             survival.to.other.insecticide = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                                                                                                                                                           trait.mean = sim.array['intervention', 2, generation-1],
                                                                                                                                                                                                                                                                                                                                           michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                                                                                                                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                                                                                                                                                                                                                         regression.coefficient = regression.coefficient,
                                                                                                                                                                                                                                                                         regression.intercept = regression.intercept,
                                                                                                                                                                                                                                                                         current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.2[generation])),
                                                                               male.differential.intervention.j =  wrapper_calculate_male_insecticide_fitness_selection_differential_mixtures(male.trait.mean = sim.array['intervention', 2, generation-1],
                                                                                                                                                                                              female.insecticide.exposure = female.exposure,
                                                                                                                                                                                              male.insecticide.exposure = male.exposure,
                                                                                                                                                                                              standard.deviation = standard.deviation,
                                                                                                                                                                                              male.fitness.cost = insecticide.parameters.df$male.fitness.cost[2],
                                                                                                                                                                                              vector.length = vector.length,
                                                                                                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                                                                                                              regression.intercept = regression.intercept,
                                                                                                                                                                                              current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.2[generation],
                                                                                                                                                                                              exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                              survival.to.other.insecticide = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                                                                                                                                                            trait.mean = sim.array['intervention', 1, generation-1],
                                                                                                                                                                                                                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                                                                                                                                                                                                                          regression.coefficient = regression.coefficient,
                                                                                                                                                                                                                                                                          regression.intercept = regression.intercept,
                                                                                                                                                                                                                                                                          current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.1[generation])),
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
                                                                               current.insecticide.efficacy.i = deployed.mixture$insecticide.efficacy.vector.part.1[generation],
                                                                               current.insecticide.efficacy.j = deployed.mixture$insecticide.efficacy.vector.part.2[generation])

          sim.array['intervention', 1, generation] = tracked.resistance[[1]]
          sim.array['refugia', 1, generation] = tracked.resistance[[2]]
          sim.array['intervention', 2, generation] = tracked.resistance[[3]]
          sim.array['refugia', 2, generation] = tracked.resistance[[4]]

        }
        #Which irm.strategy is being used: sequence or rotation

        #May be worth making the following chunk of code into its own function as it is a bit chunky
        #at the moment.
        #Update insecticide each time the deployment.frequency is reached:
        if(generation < maximum.generations){
          update.mixture.info = if(generation %% deployment.frequency == 0){
            if(irm.switch.strategy == "rotation"){
              irm_strategy_rotation_mixture_decay(number.of.insecticides = number.of.insecticides,
                                                  current.generation = generation,
                                                  withdrawal.threshold = calc.withdrawal.threshold,
                                                  return.threshold = calc.return.threshold,
                                                  simulation.array = sim.array,
                                                  available.vector = available.vector,
                                                  withdrawn.vector = withdrawn.vector,
                                                  mixture.df = mixture.df,
                                                  current.mixture = deployed.mixture$mixture.id[generation],
                                                  deployment.frequency = deployment.frequency,
                                                  deployment.df = deployed.mixture,
                                                  insecticide.parameters.df = insecticide.parameters.df)} else{
                                                    if(irm.switch.strategy == "sequence"){
                                                      irm_strategy_sequence_mixture_decay(number.of.insecticides = number.of.insecticides,
                                                                                          current.generation = generation,
                                                                                          withdrawal.threshold = calc.withdrawal.threshold,
                                                                                          return.threshold = calc.return.threshold,
                                                                                          simulation.array = sim.array,
                                                                                          available.vector = available.vector,
                                                                                          withdrawn.vector = withdrawn.vector,
                                                                                          mixture.df = mixture.df,
                                                                                          current.mixture = deployed.mixture$mixture.id[generation],
                                                                                          deployment.frequency = deployment.frequency,
                                                                                          deployment.df = deployed.mixture,
                                                                                          insecticide.parameters.df = insecticide.parameters.df)
                                                    }else{
                                                      if(irm.switch.strategy == "insecticide.1"){
                                                        decision_on_insecticide_1_only(number.of.insecticides = number.of.insecticides,
                                                                                       current.generation = generation,
                                                                                       withdrawal.threshold = calc.withdrawal.threshold,
                                                                                       return.threshold = calc.return.threshold,
                                                                                       simulation.array = sim.array,
                                                                                       available.vector = available.vector,
                                                                                       withdrawn.vector = withdrawn.vector,
                                                                                       mixture.df = mixture.df,
                                                                                       current.mixture = deployed.mixture$mixture.id[generation],
                                                                                       deployment.frequency = deployment.frequency,
                                                                                       deployment.df = deployed.mixture,
                                                                                       insecticide.parameters.df = insecticide.parameters.df)
                                                      }
                                                    }
                                                  }

            # mixture.info = list(available.mixtures, available.vector, withdrawn.vector, deployed.mixture)
          }
          if(generation %% deployment.frequency == 0){available.mixtures = update.mixture.info[[1]]}
          if(generation %% deployment.frequency == 0){available.vector = update.mixture.info[[2]]}
          if(generation %% deployment.frequency == 0){withdrawn.vector = update.mixture.info[[3]]}
          if(generation %% deployment.frequency == 0){deployed.mixture = update.mixture.info[[4]]

          }
        }
      }}#end of mixtures if statement

    if(irm.deployment.strategy == "micromosaics"){

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


    } #end of micromosaics if statement

    if(irm.deployment.strategy == "combinations"){
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





    } #end of combinations if statement



  }#sd.scaled = FALSE end

  if(sd.scaled == TRUE){

    if(irm.deployment.strategy == "singles"){
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

              tracked.resistance = multiple_gonotrophic_cycles_single_dispersal_sd_scaled(coverage = coverage,
                                                                                          dispersal.rate = dispersal.rate,
                                                                                          female.exposure = female.exposure,
                                                                                          refugia.trait.mean = sim.array['refugia', insecticide, generation-1],
                                                                                          heritability = insecticide.parameters.df$heritability[insecticide],
                                                                                          intervention.trait.mean = sim.array['intervention', insecticide, generation-1],
                                                                                          n.cycles = n.cycles,
                                                                                          male.selection.differential.intervention = wrapper_calculate_male_insecticide_fitness_selection_differential_sd_scaled(male.trait.mean = sim.array['intervention', insecticide, generation-1],
                                                                                                                                                                                                       female.insecticide.exposure = female.exposure,
                                                                                                                                                                                                       male.insecticide.exposure = male.exposure,
                                                                                                                                                                                                       z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                       z.sd.coefficient = z.sd.coefficient,                                                                                                                                                                                                       male.fitness.cost = insecticide.parameters.df$male.fitness.cost[insecticide],
                                                                                                                                                                                                       vector.length = vector.length,
                                                                                                                                                                                                       maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                       michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                       half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                                       regression.coefficient = regression.coefficient,
                                                                                                                                                                                                       regression.intercept = regression.intercept,
                                                                                                                                                                                                       current.insecticide.efficacy = insecticide.efficacy.vector[generation],
                                                                                                                                                                                                       exposure.scaling.factor = exposure.scaling.factor),
                                                                                          male.selection.differential.refugia = (sd_changes_with_z(current.z = sim.array['refugia', insecticide, generation-1],
                                                                                                                                                  z.sd.intercept = z.sd.intercept,
                                                                                                                                                  z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[insecticide]),
                                                                                          female.fitness.cost.intervention = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                                          female.fitness.cost.refugia = insecticide.parameters.df$female.fitness.cost[insecticide],
                                                                                          exposure.scaling.factor = exposure.scaling.factor,
                                                                                          vector.length = vector.length,
                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                          z.sd.coefficient = z.sd.coefficient,
                                                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                          michaelis.menten.slope = michaelis.menten.slope,
                                                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                          regression.coefficient = regression.coefficient,
                                                                                          regression.intercept = regression.intercept,
                                                                                          current.insecticide.efficacy = insecticide.efficacy.vector[generation])

              sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
              sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]

            }

            if(insecticide != deployed.insecticide[generation]){

              tracked.resistance = multiple_gonotrophic_cycles_singles_dispersal_not_deployed_sd_scaled(intervention.trait.mean.i = sim.array['intervention', insecticide, generation-1],
                                                                                              intervention.trait.mean.j = sim.array['intervention', deployed.insecticide[generation], generation-1],
                                                                                              refugia.trait.mean.i = sim.array['refugia', insecticide, generation-1],
                                                                                              refugia.trait.mean.j =  sim.array['refugia', deployed.insecticide[generation], generation-1],
                                                                                              standard.deviation = standard.deviation,
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
                                                                                                                                                                                                   z.sd.coefficient = z.sd.coefficient,                                                                                                                                                                                                      male.fitness.cost = insecticide.parameters.df$male.fitness.cost[deployed.insecticide[generation]],
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
                                                                                              current.insecticide.efficacy.j = insecticide.efficacy.vector[generation])
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

                    }
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
    }#end of singles if statement

    if(irm.deployment.strategy == "mixtures"){

      mixture.df = select_mixing_stategy(mixture.strategy = "mix.sequential.discrete",
                                         number.of.insecticides = number.of.insecticides)

      mixture.id = rep(mixture.df[1, 1], times = deployment.frequency)#first row, first column
      mixture.part.1 = rep(mixture.df[1, 2], times = deployment.frequency)#first row, second column
      mixture.part.2 = rep(mixture.df[1, 3], times = deployment.frequency)#first row, third column

      #The dataframe that holds the mixture deployment information
      deployed.mixture = data.frame(mixture.id, mixture.part.1, mixture.part.2)



      #create vectors to hold the insecticide efficacies
      deployed.mixture$insecticide.efficacy.vector.part.1 = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[deployed.mixture[1,2], 2],
                                                                                               recommended.insecticide.dose = insecticide.parameters.df[deployed.mixture[1, 2],3],
                                                                                               threshold.generations = insecticide.parameters.df[deployed.mixture[1,2], 4],
                                                                                               base.efficacy.decay.rate = insecticide.parameters.df[deployed.mixture[1,2], 5],
                                                                                               rapid.decay.rate = insecticide.parameters.df[deployed.mixture[1,2], 6],
                                                                                               deployment.frequency = deployment.frequency)

      deployed.mixture$insecticide.efficacy.vector.part.2 = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[deployed.mixture[1,3] ,2],
                                                                                               recommended.insecticide.dose = insecticide.parameters.df[deployed.mixture[1,3] ,3],
                                                                                               threshold.generations = insecticide.parameters.df[deployed.mixture[1,3] ,4],
                                                                                               base.efficacy.decay.rate = insecticide.parameters.df[deployed.mixture[1,3] ,5],
                                                                                               rapid.decay.rate = insecticide.parameters.df[deployed.mixture[1,3] ,6],
                                                                                               deployment.frequency = deployment.frequency)
      available.mixtures = mixture.df #at the start all mixtures will be available
      mixture.info = list(available.mixtures, available.vector, withdrawn.vector, deployed.mixture)


      for(generation in 2:maximum.generations){

        ##It should be noted that this model currently only works with mixtures for 2-insecticide simulations. It does not currently work for the inclusion of
        #multiple mixtures.

        #Stop the simulation if there is no insecticide being deployed anymore.
        if(is.na(deployed.mixture$mixture.id[generation])){break}else{

          tracked.resistance =   multiple_gonotrophic_cycles_mixture_dispersal_sd_scaled(intervention.trait.mean.i = sim.array['intervention', 1, generation-1],
                                                                               intervention.trait.mean.j = sim.array['intervention', 1, generation-1],
                                                                               refugia.trait.mean.i = sim.array['refugia', 1, generation-1],
                                                                               refugia.trait.mean.j = sim.array['refugia', 1, generation-1],
                                                                               z.sd.coefficient = z.sd.coefficient,
                                                                               z.sd.intercept = z.sd.intercept,
                                                                               vector.length = vector.length,
                                                                               female.exposure = female.exposure,
                                                                               exposure.scaling.factor = exposure.scaling.factor,
                                                                               coverage = coverage,
                                                                               dispersal.rate = dispersal.rate,
                                                                               male.differential.intervention.i = wrapper_calculate_male_insecticide_fitness_selection_differential_mixtures_sd_scaled(male.trait.mean = sim.array['intervention', 1, generation-1],
                                                                                                                                                                                             female.insecticide.exposure = female.exposure,
                                                                                                                                                                                             male.insecticide.exposure = male.exposure,
                                                                                                                                                                                             z.sd.coefficient = z.sd.coefficient,
                                                                                                                                                                                             z.sd.intercept = z.sd.intercept,                                                                                                                                                                                             male.fitness.cost = insecticide.parameters.df$male.fitness.cost[1],
                                                                                                                                                                                             vector.length = vector.length,
                                                                                                                                                                                             maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                             michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                             regression.coefficient = regression.coefficient,
                                                                                                                                                                                             regression.intercept = regression.intercept,
                                                                                                                                                                                             current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.1[generation],
                                                                                                                                                                                             exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                             survival.to.other.insecticide = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                                                                                                                                                           trait.mean = sim.array['intervention', 2, generation-1],
                                                                                                                                                                                                                                                                                                                                           michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                                                                                                                                                           half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                                                                                                                                                                                                                         regression.coefficient = regression.coefficient,
                                                                                                                                                                                                                                                                         regression.intercept = regression.intercept,
                                                                                                                                                                                                                                                                         current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.2[generation])),
                                                                               male.differential.intervention.j =  wrapper_calculate_male_insecticide_fitness_selection_differential_mixtures(male.trait.mean = sim.array['intervention', 2, generation-1],
                                                                                                                                                                                              female.insecticide.exposure = female.exposure,
                                                                                                                                                                                              male.insecticide.exposure = male.exposure,
                                                                                                                                                                                              z.sd.coefficient = z.sd.coefficient,
                                                                                                                                                                                              z.sd.intercept = z.sd.intercept,                                                                                                                                                                                              male.fitness.cost = insecticide.parameters.df$male.fitness.cost[2],
                                                                                                                                                                                              vector.length = vector.length,
                                                                                                                                                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                                                              regression.coefficient = regression.coefficient,
                                                                                                                                                                                              regression.intercept = regression.intercept,
                                                                                                                                                                                              current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.2[generation],
                                                                                                                                                                                              exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                                              survival.to.other.insecticide = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                                                                                                                                                                                                            trait.mean = sim.array['intervention', 1, generation-1],
                                                                                                                                                                                                                                                                                                                                            michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                                                                                                                                                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance),
                                                                                                                                                                                                                                                                          regression.coefficient = regression.coefficient,
                                                                                                                                                                                                                                                                          regression.intercept = regression.intercept,
                                                                                                                                                                                                                                                                          current.insecticide.efficacy = deployed.mixture$insecticide.efficacy.vector.part.1[generation])),
                                                                               male.differential.refugia.i = (sd_changes_with_z(current.z = sim.array['refugia', 1, generation-1],
                                                                                                                               z.sd.intercept = z.sd.intercept,
                                                                                                                               z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[1]),
                                                                               male.differential.refugia.j = (sd_changes_with_z(current.z = sim.array['refugia', 2, generation-1],
                                                                                                                                z.sd.intercept = z.sd.intercept,
                                                                                                                                z.sd.coefficient = z.sd.coefficient) * insecticide.parameters.df$male.fitness.cost[2]),
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
                                                                               current.insecticide.efficacy.i = deployed.mixture$insecticide.efficacy.vector.part.1[generation],
                                                                               current.insecticide.efficacy.j = deployed.mixture$insecticide.efficacy.vector.part.2[generation])

          sim.array['intervention', 1, generation] = tracked.resistance[[1]]
          sim.array['refugia', 1, generation] = tracked.resistance[[2]]
          sim.array['intervention', 2, generation] = tracked.resistance[[3]]
          sim.array['refugia', 2, generation] = tracked.resistance[[4]]

        }
        #Which irm.strategy is being used: sequence or rotation

        #May be worth making the following chunk of code into its own function as it is a bit chunky
        #at the moment.
        #Update insecticide each time the deployment.frequency is reached:
        if(generation < maximum.generations){
          update.mixture.info = if(generation %% deployment.frequency == 0){
            if(irm.switch.strategy == "rotation"){
              irm_strategy_rotation_mixture_decay(number.of.insecticides = number.of.insecticides,
                                                  current.generation = generation,
                                                  withdrawal.threshold = calc.withdrawal.threshold,
                                                  return.threshold = calc.return.threshold,
                                                  simulation.array = sim.array,
                                                  available.vector = available.vector,
                                                  withdrawn.vector = withdrawn.vector,
                                                  mixture.df = mixture.df,
                                                  current.mixture = deployed.mixture$mixture.id[generation],
                                                  deployment.frequency = deployment.frequency,
                                                  deployment.df = deployed.mixture,
                                                  insecticide.parameters.df = insecticide.parameters.df)} else{
                                                    if(irm.switch.strategy == "sequence"){
                                                      irm_strategy_sequence_mixture_decay(number.of.insecticides = number.of.insecticides,
                                                                                          current.generation = generation,
                                                                                          withdrawal.threshold = calc.withdrawal.threshold,
                                                                                          return.threshold = calc.return.threshold,
                                                                                          simulation.array = sim.array,
                                                                                          available.vector = available.vector,
                                                                                          withdrawn.vector = withdrawn.vector,
                                                                                          mixture.df = mixture.df,
                                                                                          current.mixture = deployed.mixture$mixture.id[generation],
                                                                                          deployment.frequency = deployment.frequency,
                                                                                          deployment.df = deployed.mixture,
                                                                                          insecticide.parameters.df = insecticide.parameters.df)
                                                    }else{
                                                      if(irm.switch.strategy == "insecticide.1"){
                                                        decision_on_insecticide_1_only(number.of.insecticides = number.of.insecticides,
                                                                                       current.generation = generation,
                                                                                       withdrawal.threshold = calc.withdrawal.threshold,
                                                                                       return.threshold = calc.return.threshold,
                                                                                       simulation.array = sim.array,
                                                                                       available.vector = available.vector,
                                                                                       withdrawn.vector = withdrawn.vector,
                                                                                       mixture.df = mixture.df,
                                                                                       current.mixture = deployed.mixture$mixture.id[generation],
                                                                                       deployment.frequency = deployment.frequency,
                                                                                       deployment.df = deployed.mixture,
                                                                                       insecticide.parameters.df = insecticide.parameters.df)
                                                      }
                                                    }
                                                  }

            # mixture.info = list(available.mixtures, available.vector, withdrawn.vector, deployed.mixture)
          }
          if(generation %% deployment.frequency == 0){available.mixtures = update.mixture.info[[1]]}
          if(generation %% deployment.frequency == 0){available.vector = update.mixture.info[[2]]}
          if(generation %% deployment.frequency == 0){withdrawn.vector = update.mixture.info[[3]]}
          if(generation %% deployment.frequency == 0){deployed.mixture = update.mixture.info[[4]]

          }
        }
      }}#end of mixtures if statement

    if(irm.deployment.strategy == "micromosaics"){

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

        male.selection.differentials = perform_male_micromosaic_smooth_sd_scaled(insecticide.coverage.1 = intervention.coverage.1,
                                                                                 insecticide.coverage.2 = intervention.coverage.2,
                                                                                 trait.mean.1 = sim.array["intervention", 1, generation-1],
                                                                                 trait.mean.2 = sim.array["intervention", 2, generation-1],
                                                                                 z.sd.intercept = z.sd.intercept,
                                                                                 z.sd.coefficient = z.sd.coefficient,
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

        tracked.resistance = multiple_gonotrophic_cycles_micromosaic_dispersal_sd_scaled(intervention.trait.mean.i = sim.array['intervention', 1, generation-1],
                                                                               intervention.trait.mean.j = sim.array['intervention', 2, generation-1],
                                                                               refugia.trait.mean.i = sim.array['refugia', 1, generation-1],
                                                                               refugia.trait.mean.j = sim.array['refugia', 2, generation-1],
                                                                               z.sd.intercept = z.sd.intercept,
                                                                               z.sd.coefficient = z.sd.coefficient,
                                                                               vector.length = vector.length,                                                                               vector.length = vector.length,
                                                                               female.exposure = female.exposure,
                                                                               exposure.scaling.factor = exposure.scaling.factor,
                                                                               coverage = coverage,
                                                                               dispersal.rate = dispersal.rate,
                                                                               male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.selection.differentials[[1]],
                                                                                                                                                                            exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                            male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', 1, generation-1],
                                                                                                                                                                                                                                               z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                                               z.sd.coefficient = z.sd.coefficient) *insecticide.parameters.df$male.fitness.cost[1])),
                                                                               male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.selection.differentials[[2]],
                                                                                                                                                                            exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                            male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', 2, generation-1],
                                                                                                                                                                                                                                     z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                                     z.sd.coefficient = z.sd.coefficient) *insecticide.parameters.df$male.fitness.cost[2])),
                                                                               male.differential.refugia.i =(sd_changes_with_z(current.z = sim.array['refugia', 1, generation-1],
                                                                                                                               z.sd.intercept = z.sd.intercept,
                                                                                                                               z.sd.coefficient = z.sd.coefficient) *insecticide.parameters.df$male.fitness.cost[1]),
                                                                               male.differential.refugia.j = (sd_changes_with_z(current.z = sim.array['refugia', 2, generation-1],
                                                                                                                                z.sd.intercept = z.sd.intercept,
                                                                                                                                z.sd.coefficient = z.sd.coefficient) *insecticide.parameters.df$male.fitness.cost[2]),
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


    } #end of micromosaics if statement

    if(irm.deployment.strategy == "combinations"){
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

        male.insecticide.intervention.i =  perform_male_combination_insecticide_selection_differential_smooth_sd_scaled(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.1,
                                                                                                              coverage.j = intervention.coverage.1,
                                                                                                              coverage.ij = intervention.coverage.1.2,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', 1, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', 2, generation-1],
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient,
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

        male.insecticide.intervention.j =  perform_male_combination_insecticide_selection_differential_smooth_sd_scaled(coverage = coverage,
                                                                                                              coverage.i = intervention.coverage.1,
                                                                                                              coverage.j = intervention.coverage.1,
                                                                                                              coverage.ij = intervention.coverage.1.2,
                                                                                                              probability.only.i = probability.only.i.male,
                                                                                                              probability.only.j = probability.only.j.male,
                                                                                                              probability.both.i.j = probability.both.i.j.male,
                                                                                                              trait.mean.1 = sim.array['intervention', 1, generation-1],
                                                                                                              trait.mean.2 = sim.array['intervention', 2, generation-1],
                                                                                                              z.sd.intercept = z.sd.intercept,
                                                                                                              z.sd.coefficient = z.sd.coefficient,
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



        tracked = multiple_gonotrophic_cycles_combination_dispersal_sd_scaled(intervention.trait.mean.i = sim.array['intervention', 1, generation-1],
                                                                    intervention.trait.mean.j = sim.array['intervention', 2, generation-1],
                                                                    refugia.trait.mean.i = sim.array['refugia', 1, generation-1],
                                                                    refugia.trait.mean.j = sim.array['refugia', 2, generation-1],
                                                                    z.sd.intercept = z.sd.intercept,
                                                                    z.sd.coefficient = z.sd.coefficient,
                                                                    vector.length = vector.length,
                                                                    female.exposure = female.exposure,
                                                                    exposure.scaling.factor = exposure.scaling.factor,
                                                                    coverage = coverage,
                                                                    dispersal.rate = dispersal.rate,
                                                                    male.differential.intervention.i = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential = male.insecticide.intervention.i,
                                                                                                                                                                 exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                 male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', 1, generation-1],
                                                                                                                                                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                          z.sd.coefficient = z.sd.coefficient) *insecticide.parameters.df$male.fitness.cost[1])),
                                                                    male.differential.intervention.j = calculate_male_insecticide_fitness_selection_differential(male.insecticide.selection.differential =  male.insecticide.intervention.j, # male.insecticide.intervention.j,
                                                                                                                                                                 exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                                                                 male.fitness.selection.differential = (sd_changes_with_z(current.z = sim.array['intervention', 2, generation-1],
                                                                                                                                                                                                                          z.sd.intercept = z.sd.intercept,
                                                                                                                                                                                                                          z.sd.coefficient = z.sd.coefficient) *insecticide.parameters.df$male.fitness.cost[2])),

                                                                    male.differential.refugia.i = (sd_changes_with_z(current.z = sim.array['refugia', 1, generation-1],
                                                                                                                     z.sd.intercept = z.sd.intercept,
                                                                                                                     z.sd.coefficient = z.sd.coefficient) *insecticide.parameters.df$male.fitness.cost[1]),
                                                                    male.differential.refugia.j = (sd_changes_with_z(current.z = sim.array['refugia', 2, generation-1],
                                                                                                                     z.sd.intercept = z.sd.intercept,
                                                                                                                     z.sd.coefficient = z.sd.coefficient) *insecticide.parameters.df$male.fitness.cost[2]),
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





    } #end of combinations if statement



  }#sd.scaled = TRUE end

  return(sim.array)

}
