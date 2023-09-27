#'@description There may be occassions where one of the insecticides is a lot cheaper than the other insecticides. Under
#'these conditions it would be prudent that this insecticide was preferentially deployed to reduce the overall cost of
#'the IRM strategy. This could for example be with doing micromosaics of a pythrethroid while rotating the expensive options
#'of the carbamate and organophosphate micromosaic portion. For the sake of simplicity the "cheap" insecticide is insecticide 1


irm_strategy_micromosaics_rotate_expensive = function(number.of.insecticides,
                                                   current.generation,
                                                   withdrawal.threshold,
                                                   return.threshold,
                                                   simulation.array,
                                                   available.vector,
                                                   withdrawn.vector,
                                                   deployment.frequency,
                                                   deployed.insecticide.i,
                                                   deployed.insecticide.j,
                                                   deployment.vector.i,
                                                   deployment.vector.j){

  #First withdrawal and return of insecticides:::
  #Step 1: confirm which insecticides are/are not available for deployment
  list.available.withdrawn =  return_and_withdrawal_of_insecticides_from_arsenal(number.of.insecticides = number.of.insecticides,
                                                                                 current.generation = current.generation,
                                                                                 withdrawal.threshold = withdrawal.threshold,
                                                                                 return.threshold = return.threshold,
                                                                                 simulation.array = simulation.array,
                                                                                 available.vector = available.vector,
                                                                                 withdrawn.vector = withdrawn.vector)
  available.to.deploy = list.available.withdrawn[[1]]
  unavailable.to.deploy = list.available.withdrawn[[2]]


  #If the "cheap" insecticide is not available stop the simulation ---> assume there is
    #not sufficient money to deploy two expensive insecticides at once.
  if(1 %in% unavailable.to.deploy == TRUE){
    deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.i)
    deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.j)

    return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated.i, deployment.vector.updated.j))
    }

  #If there is 1 or less insecticide left micro-mosaics no longer possible --> set conditions to terminate simulation
  if(length(available.to.deploy) <= 1){#set deployments to NA to stop the simulation
    deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.i)
    deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.j)

    return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated.i, deployment.vector.updated.j))
     }


  #Temporarily make the deployed.insecticide.j insecticide unavailable; this means it cannot be a new candidate (and
  # immediately redeployed --> there has to be a rotation),
  if(deployed.insecticide.j %in% available.to.deploy == TRUE){
    available.to.deploy.temp = available.to.deploy[!available.to.deploy %in% deployed.insecticide.j]}
  #do the same for the "cheap" insecticide
  if(1 %in% available.to.deploy == TRUE){
    available.to.deploy.temp = available.to.deploy.temp[!available.to.deploy.temp %in% 1]}

  #If there is 1 or less insecticide left micro-mosaics no longer possible --> set conditions to terminate simulation
  if(length(available.to.deploy.temp) <= 1){#set deployments to NA to stop the simulation
    deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.i)
    deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.j)
  }


  #Next need to find the two candidate insecticides for deployment

  #Choose the first candidate insecticide
  candidate.insecticide.1 = choose_the_next_insecticide(previous.insecticide = deployed.insecticide.j,
                                                        available.insecticides = available.to.deploy.temp,
                                                        number.of.insecticides = number.of.insecticides)

  #remove candidate.1
  available.to.deploy.temp = available.to.deploy.temp[!available.to.deploy.temp %in% candidate.insecticide.1]




  deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = 1,
                                                              deployment.frequency = deployment.frequency,
                                                              deployment.vector = deployment.vector.i)

  deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = candidate.insecticide.1,
                                                              deployment.frequency = deployment.frequency,
                                                              deployment.vector = deployment.vector.j)



  return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated.i, deployment.vector.updated.j))

}
