# ##This is the development version
##Superceded by run_sim_advanced methodology

#
# run_simulation_multiple_gonotrophic_cycles_dispersal= function(coverage = 0.8,
#                                                                natural.survival = 0.7,
#                                                                dispersal.rate = 0.99,
#                                                                female.insecticide.exposure = 0.7,
#                                                                male.insecticide.exposure = 0.7,
#                                                                heritability = 0.3,
#                                                                starting.intervention.resistance.score = 10,
#                                                                starting.refugia.resistance.score = 0,
#                                                                n.cycles = 1,
#                                                                male.selection.differential.intervention = 0,
#                                                                male.selection.differential.refugia = 0,
#                                                                exposure.scaling.factor = 10,
#                                                                vector.length = 1000,
#                                                                standard.deviation = 30,
#                                                                half.population.bioassay.survival.resistance = 900,
#                                                                michaelis.menten.slope = 1,
#                                                                maximum.bioassay.survival.proportion = 1,
#                                                                regression.coefficient = 0.48,
#                                                                regression.intercept = 0.15,
#                                                                current.insecticide.efficacy = 1,
#                                                                maximum.generations = 500,
#                                                                number.of.insecticides = 2,
#                                                                min.cross.selection,
#                                                                max.cross.selection,
#                                                                withdrawal.threshold.value,
#                                                                maximum.resistance.value,
#                                                                deployment.frequency,
#                                                                applied.insecticide.dose,
#                                                                recommended.insecticide.dose,
#                                                                threshold.generation,
#                                                                base.efficacy.decay.rate,
#                                                                rapid.decay.rate,
#                                                                ){
#
#   #Start by creating an array (calls the array_named function):
#   #dimension 1: site = c("refugia", "intervention"), which hold resistance scores
#   #Easier to include both, but refugia won't happen if no dispersal
#   #dimension 2: insectide to which the resistance intensity corresponds to
#   #dimension 3: generation.
#   sim.array = create_starting_array(n.insecticides = number.of.insecticides,
#                                     maximum.generations = maximum.generations)
#
#   #Set the starting resistance scores for the insecticides:
#   sim.array = set_starting_resistance_scores(sim.array = sim.array,
#                                              starting.refugia.resistance.score = starting.refugia.resistance.score,
#                                              starting.intervention.resistance.score = starting.intervention.resistance.score,
#                                              number.of.insecticides = number.of.insecticides)
#
#
#   cross.selection.matrix = make_cross_selection_matrix(number.of.insecticides = number.of.insecticides,
#                                                        min.cross.selection = min.cross.selection,
#                                                        max.cross.selection = max.cross.selection)
#
#   #Make a vector of the available insecticides@
#   available.vector = seq(1, number.of.insecticides, by = 1)#Creates a vector of the insecticides that are available for deployment.
#   #At the beginning all insecticides are available for deployment.
#   withdrawn.vector = c() #creates an empty vector to hold the withdrawn insecticides.
#
#   #Set the withdrawal and return bioassay survival thresholds.
#   calc.withdrawal.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
#                                                                             michaelis.menten.slope = 1, #must be set to 1 to work properly
#                                                                             half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
#                                                                             bioassay.survival = withdrawal.threshold.value,
#                                                                             estimate.precision = 0.001,
#                                                                             minimum.resistance.value = 0,
#                                                                             maximum.resistance.value = maximum.resistance.value)
#
#   calc.return.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
#                                                                         michaelis.menten.slope = 1, #must be set to 1 to work properly
#                                                                         half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
#                                                                         bioassay.survival = return.threshold.value,
#                                                                         estimate.precision = 0.001,
#                                                                         minimum.resistance.value = 0,
#                                                                         maximum.resistance.value = maximum.resistance.value)
#
#   #Make a dataframe of the insecticide parameters:
#   insecticide.parameters.df = create_insecticide_parameters_dataframe(number.of.insecticides = number.of.insecticides,
#                                                                       applied.insecticide.dose = applied.insecticide.dose,
#                                                                       recommended.insecticide.dose = recommended.insecticide.dose,
#                                                                       threshold.generation = threshold.generations,
#                                                                       base.efficacy.decay.rate = base.efficacy.decay.rate,
#                                                                       rapid.decay.rate = rapid.decay.rate,
#                                                                       heritability = heritability)
#
#
#
#
#
#   #The first insecticide deployed is always insecticide 1
#   insecticide.efficacy.vector = create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[1,2],
#                                                                    recommended.insecticide.dose = insecticide.parameters.df[1,3],
#                                                                    threshold.generations = insecticide.parameters.df[1,4],
#                                                                    base.efficacy.decay.rate = insecticide.parameters.df[1,5],
#                                                                    rapid.decay.rate = insecticide.parameters.df[1,6],
#                                                                    deployment.frequency = deployment.frequency)
#
#
#   deployed.insecticide = rep(1, deployment.frequency)
#
#   insecticide.info = list(available.vector, withdrawn.vector, deployed.insecticide)
#
#
#
#
#   for(generation in 2:maximum.generations){
#
#     #Stop the simulation if there is no insecticide being deployed anymore.
#     if(is.na(deployed.insecticide[generation])){break}else{
#
#       for(insecticide in 1:number.of.insecticides){ #track the resistance intensity for each insecticide
#         ##                                                   #ask whether insecticide is the same as deployed insecticide
#
#
#         if(insecticide == deployed.insecticide[generation]){
#
#           update.means =  multiple_gonotrophic_cycles_with_dispersal_v2(coverage = coverage,
#                                                                         natural.survival = natural.survival,
#                                                                         dispersal.rate = dispersal.rate,
#                                                                         female.exposure = female.insecticide.exposure,
#                                                                         refugia.trait.mean = sim.array["refugia", 1, generation-1],
#                                                                         heritability = heritability,
#                                                                         intervention.trait.mean = sim.array["intervention", 1, generation-1],
#                                                                         n.cycles = n.cycles,
#                                                                         male.selection.differential.intervention = 0,
#                                                                         male.selection.differential.refugia = 0,
#                                                                         exposure.scaling.factor = exposure.scaling.factor,
#                                                                         vector.length = vector.length,
#                                                                         standard.deviation = standard.deviation,
#                                                                         half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
#                                                                         michaelis.menten.slope = michaelis.menten.slope,
#                                                                         maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
#                                                                         regression.coefficient = regression.coefficient,
#                                                                         regression.intercept = regression.intercept,
#                                                                         current.insecticide.efficacy = current.insecticide.efficacy)
#
#           sim.array["intervention", insecticide, generation] = update.means[[1]]
#           sim.array["refugia", insecticide, generation] = update.means[[2]]
#           if(insecticide != deployed.insecticide[generation]){
#
#             sim.array["intervention", insecticide, generation] = sim.array["intervention", insecticide, generation - 1]
#             sim.array["refugia", insecticide, generation] = sim.array["intervention", insecticide, generation - 1]
#
#           }
#
#           #end insecticide not deployed
#         }
#         #NEED TO FIGURE OUT A WAY TO MAKE ALL THIS LESS MESSY AND MORE READABLE!!!!!!!!!!!
#       }#end of for insecticide loop
#
#       if(generation < maximum.generations){
#         update.insecticide.info = if(generation %% deployment.frequency == 0){
#           if(irm.strategy == "rotation"){
#             irm_strategy_rotation(
#               number.of.insecticides = number.of.insecticides,
#               current.generation = generation,
#               withdrawal.threshold = calc.withdrawal.threshold,
#               return.threshold = calc.return.threshold,
#               simulation.array = sim.array,
#               available.vector = available.vector,
#               withdrawn.vector = withdrawn.vector,
#               current.insecticide = deployed.insecticide[generation],
#               deployment.frequency = deployment.frequency,
#               deployment.vector = deployed.insecticide)} else{
#                 if(irm.strategy == "sequence"){
#                   irm_strategy_sequence(
#                     number.of.insecticides = number.of.insecticides,
#                     current.generation = generation,
#                     withdrawal.threshold = calc.withdrawal.threshold,
#                     return.threshold = calc.return.threshold,
#                     simulation.array = sim.array,
#                     available.vector = available.vector,
#                     withdrawn.vector = withdrawn.vector,
#                     current.insecticide = deployed.insecticide[generation],
#                     deployment.frequency = deployment.frequency,
#                     deployment.vector = deployed.insecticide)
#
#                 }
#               }
#
#           #update.insectide.info[[1]] is the vector of the available insecticides
#           #update.insecticide.info[[2]] is the vector of the withdrawn insecticides
#           #update.insecticide.info[[3]] is the vector of the whole deployment =c(previous.deployment, new.deployment)
#         }
#         if(generation %% deployment.frequency == 0){available.vector = update.insecticide.info[[1]]}
#         if(generation %% deployment.frequency == 0){withdrawn.vector = update.insecticide.info[[2]]}
#         if(generation %% deployment.frequency == 0){deployed.insecticide = update.insecticide.info[[3]]}
#         if(generation %% deployment.frequency == 0){currently.deployed.insecticide = deployed.insecticide[generation+1]}
#
#         if(generation %% deployment.frequency == 0){insecticide.efficacy.vector = c(insecticide.efficacy.vector,
#                                                                                     create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[currently.deployed.insecticide, 2],
#                                                                                                                        recommended.insecticide.dose = insecticide.parameters.df[currently.deployed.insecticide, 3],
#                                                                                                                        threshold.generations = insecticide.parameters.df[currently.deployed.insecticide, 4],
#                                                                                                                        base.efficacy.decay.rate = insecticide.parameters.df[currently.deployed.insecticide, 5],
#                                                                                                                        rapid.decay.rate = insecticide.parameters.df[currently.deployed.insecticide, 6],
#                                                                                                                        deployment.frequency = deployment.frequency))}
#
#
#         #A break point to stop simuation if there is no insecticide deployed
#         #if(is.na(deployed.insecticide[generation])){break}
#
#       }
#     }
#   }#end of for(generation) loop
#
#
#   #ensure the simulation array is return after running
#   #need to develop an quick and easy way to turn array into dataframes for plotting purposes
#   return(sim.array)
#
# }
#
#
#
#
#
#
