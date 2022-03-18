irm_strategy_micromosaics_full_rotation = function(number.of.insecticides,
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


  #If there is 1 or less insecticide left micro-mosaics no longer possible --> set conditions to terminate simulation
  if(length(available.to.deploy <= 1)){#set deployments to NA to stop the simulation
    deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.i)
    deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.j)
  }




  #Temporarily make the to.replace insecticide unavailable; this means it cannot be a new candidate,
  #and prevents the same insecticide being deployed as both i and j
  if(deployed.insecticide.i %in% available.to.deploy &
     deployed.insecticide.j %in% available.to.deploy){
    available.to.deploy.temp = available.to.deploy[!available.to.deploy %in% c(deployed.insecticide.i, deployed.insecticide.j)]}


  #If there is 1 or less insecticide left micro-mosaics no longer possible --> set conditions to terminate simulation
  if(length(available.to.deploy.temp < 1)){#set deployments to NA to stop the simulation
    deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.i)
    deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.j)
  }


  #Next need to find the two candidate insecticides for deployment

  #Choose the first candidate insecticide
  candidate.insecticide.1 = choose_the_next_insecticide(previous.insecticide = deployed.insecticide.i,
                                                        available.insecticides = available.to.deploy.temp,
                                                        number.of.insecticides = number.of.insecticides)

  #remove candidate.1
  available.to.deploy.temp = available.to.deploy.temp[!available.to.deploy.temp %in% candidate.insecticide.1]


  #find second candidate.insecticide:::
  candidate.insecticide.2 = choose_the_next_insecticide(previous.insecticide = deployed.insecticide.i,
                                                        available.insecticides = available.to.deploy.temp,
                                                        number.of.insecticides = number.of.insecticides)


  deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = candidate.insecticide.1,
                                                              deployment.frequency = deployment.frequency,
                                                              deployment.vector = deployment.vector.i)

  deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = candidate.insecticide.2,
                                                              deployment.frequency = deployment.frequency,
                                                              deployment.vector = deployment.vector.j)



  return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated.i, deployment.vector.updated.j))

}
