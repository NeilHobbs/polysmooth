
run_simulation_temp = function(number.of.insecticides = 2,
                               exposure.scaling.factor = 10,
                               nsim = 1000,
                               female.fitness.cost,
                               male.fitness.cost,
                               female.insecticide.exposure,
                               male.insecticide.exposure,
                               standard.deviation,
                               vector.length,
                               maximum.bioassay.survival.proportion,
                               michaelis.menten.slope,
                               regression.coefficient,
                               regression.intercept,
                               maximum.generations = 500,
                               irm.strategy = "sequence", #will be sequence or rotation (plus mixture later on),
                               half.population.bioassay.survival.resistance = 900,
                               withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                               return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                               deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                               maximum.resistance.value = 25000,
                               starting.refugia.resistance.score = 0,
                               starting.intervention.resistance.score = 0){

  #Start by creating an array (calls the array_named function):
  #dimension 1: site = c("refugia", "treatment"), which hold resistance intensities.
  #Easier to include both, but refugia won't happen if no dispersal
  #dimension 2: insectide to which the resistance intensity corresponds to
  #dimension 3: generation.
  #dim 4: which insecticide is currently deployed. Need to decide if it easier/better to have the deployed insecticide in the
  #array; or just as a separate vector as is currently used. I think as a separate vector as it is currently working through this method.
  #refugia is the refugia. treatment is the place where insecticides are the intervention site where insecticides are deployed.

  sim.array = create_starting_array(n.insecticides = number.of.insecticides,
                                    maximum.generations = maximum.generations)


  #Maybe create a separate function: set_starting_conditions() for the following chunk of code. In doing so;
  #be able to set each insecticide having a unique starting intensity. And would set the insecticide.info
  #and calculating the withdrawal and return thresholds.

  if(length(starting.refugia.resistance.score) == 1){
    sim.array['refugia', , 1] = starting.refugia.resistance.score

  }  else(

    #Set starting resistance intensities (fills in only the first row/generation). The other generations are set to NAs.
    for(i in 1:number.of.insecticides){
      sim.array['refugia', i , 1] = starting.refugia.resistance.score[i]
    })

  if(length(starting.intervention.resistance.score) == 1){
    sim.array['intervention', , 1] = starting.intervention.resistance.score
  }else(

    #treatment site starting resistance intensity (where the insecticide can be deployed)
    for(i in 1:number.of.insecticides){
      sim.array['intervention', i , 1] = starting.intervention.resistance.score[i]
    })

  available.vector = seq(1, number.of.insecticides, by = 1)#Creates a vector of the insecticides that are available for deployment.
  #At the beginning all insecticides are available for deployment.
  withdrawn.vector = c() #creates an empty vector to hold the withdrawn insecticides.

  deployed.insecticide = rep(1, times = deployment.frequency)#Always start with insecticide 1.
  #This is fine as all insecticides have equivalent properties.


  insecticide.info = list(available.vector, withdrawn.vector, deployed.insecticide)
  #Set the withdrawal and return thresholds: requires inputting the desired proportion of survival as input parameters. These will require
  #the user to input the half.population.bioassay.survival.resistance; the required thresholds; and maximum.resistance.value [this will be incase,
  #a user decides to use a high Z50 value]. But we should recommend the Z50 to be 900.


  calc.withdrawal.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                            michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                            half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                            bioassay.survival = withdrawal.threshold.value,
                                                                            estimate.precision = 0.001,
                                                                            minimum.resistance.value = 0,
                                                                            maximum.resistance.value = maximum.resistance.value)

  calc.return.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                        michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                        bioassay.survival = return.threshold.value,
                                                                        estimate.precision = 0.001,
                                                                        minimum.resistance.value = 0,
                                                                        maximum.resistance.value = maximum.resistance.value)


  #Also worth considering turning the for generation and for insecticide loops into functions,
  #as the code is other wise very large and chunky and therefore complicated to edit and adapt.
  #start at generation 2, as generation 1 has intensities set at 0.
  for(generation in 2:maximum.generations){

    #Stop the simulation if there is no insecticide being deployed anymore.
    if(is.na(deployed.insecticide[generation])){break}else{

      for(insecticide in 1:number.of.insecticides){ #track the resistance intensity for each insecticide
        ##                                                   #ask whether insecticide is the same as deployed insecticide


        if(insecticide == deployed.insecticide[generation]){

          tracked.resistance = wrapper_intervention_site_after_selection_deployed(insecticide.population.suppression = insecticide.population.suppression,
                                                                                  intervention.before.selection = sim.array['intervention', insecticide, generation-1],
                                                                                  female.fitness.cost = female.fitness.cost,
                                                                                  male.fitness.cost = male.fitness.cost,
                                                                                  female.insecticide.exposure = female.insecticide.exposure,
                                                                                  male.insecticide.exposure = male.insecticide.exposure,
                                                                                  standard.deviation = standard.deviation,
                                                                                  vector.length = vector.length,
                                                                                  maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                  michaelis.menten.slope = michaelis.menten.slope,
                                                                                  half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                  regression.coefficient = regression.coefficient,
                                                                                  regression.intercept = regression.intercept,
                                                                                  current.insecticide.efficacy = current.insecticide.efficacy,
                                                                                  exposure.scaling.factor = exposure.scaling.factor,
                                                                                  heritability = heritability,
                                                                                  refugia.before.selection = sim.array['refugia', insecticide, generation-1],
                                                                                  dispersal.rate = dispersal.rate,
                                                                                  intervention.coverage = intervention.coverage)

          sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
          sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]

        }

        if(insecticide != deployed.insecticide[generation]){

          tracked.resistance = wrapper_intervention_site_not_deployed_dispersal(insecticide.population.suppression = insecticide.population.suppression,
                                                                                intervention.before.selection = sim.array['intervention', insecticide, generation-1],
                                                                                female.fitness.cost = female.fitness.cost,
                                                                                male.fitness.cost = male.fitness.cost,
                                                                                heritability = heritability,
                                                                                refugia.before.selection = sim.array['refugia', insecticide, generation-1],
                                                                                dispersal.rate = dispersal.rate,
                                                                                intervention.coverage = intervention.coverage)

          sim.array['intervention', insecticide, generation] = tracked.resistance[[1]]
          sim.array['refugia', insecticide, generation] = tracked.resistance[[2]]


        #end insecticide not deployed
      }

        }#end of for insecticide loop

      #returns the mean population insecticide resistance each generation.


      #Which irm.strategy is being used: sequence or rotation

      #May be worth making the following chunk of code into its own function as it is a bit chunky
      #at the moment.
      #Update insecticide each time the deployment.frequency is reached:
      if(generation < maximum.generations){
        update.insecticide.info = if(generation %% deployment.frequency == 0){
          if(irm.strategy == "rotation"){
            irm_strategy_rotation(number.of.insecticides = number.of.insecticides,
              current.generation = generation,
              withdrawal.threshold = calc.withdrawal.threshold,
              return.threshold = calc.return.threshold,
              simulation.array = sim.array,
              available.vector = available.vector,
              withdrawn.vector = withdrawn.vector,
              current.insecticide = deployed.insecticide[generation],
              deployment.frequency = deployment.frequency,
              deployment.vector = deployed.insecticide)}

          if(irm.strategy == "sequence"){
                  irm_strategy_sequence(number.of.insecticides = number.of.insecticides,
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

      #A break point to stop simuation if there is no insecticide deployed
      #if(is.na(deployed.insecticide[generation])){break}

    }
      }#end of for(generation) loop

    #ensure the simulation array is return after running
    #need to develop an quick and easy way to turn array into dataframes for plotting purposes
    return(list(sim.array, deployed.insecticide))
  }






