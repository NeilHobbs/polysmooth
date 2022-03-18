irm_strategy_micromosaics_partial_rotation = function(number.of.insecticides,
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
  if(length(available.to.deploy) <= 1){#set deployments to NA to stop the simulation
    deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.i)
    deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.j)

    return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated.i, deployment.vector.updated.j))
  }

  ###replace the lowest number insecticide
    #Note that it would be more practical to replace the insecticide to which there is the most resistance...
  to.replace = min(c(deployed.insecticide.i, deployed.insecticide.j))
  to.remain.deployed = max(c(deployed.insecticide.i, deployed.insecticide.j))




  #Temporarily make the to.replace insecticide unavailable; this means it cannot be a new candidate,
    #and prevents the same insecticide being deployed as both i and j
  if(to.replace %in% available.to.deploy == TRUE){
    available.to.deploy.temp = available.to.deploy[!available.to.deploy %in% to.replace]}
    available.to.deploy.temp = available.to.deploy.temp[!available.to.deploy.temp %in% to.remain.deployed]

  #Choose the candidate insecticide
    candidate.insecticide = choose_the_next_insecticide(previous.insecticide = to.replace,
                                                        available.insecticides = available.to.deploy.temp,
                                                        number.of.insecticides = number.of.insecticides)

    #if the candidate insecticide is either of the currently deployed insecticides set deployed to NA to stop the simulation
    if(candidate.insecticide == deployed.insecticide.i |
       candidate.insecticide == deployed.insecticide.j){
      deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                  deployment.frequency = 1,
                                                                  deployment.vector = deployment.vector.i)
      deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                  deployment.frequency = 1,
                                                                  deployment.vector = deployment.vector.j)}

    #otherwise the candidate insecticide can be deployed.
    else{deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = to.remain.deployed,
                                                                       deployment.frequency = deployment.frequency,
                                                                       deployment.vector = deployment.vector.i)

    deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = candidate.insecticide,
                                                                deployment.frequency = deployment.frequency,
                                                                deployment.vector = deployment.vector.j)}



  return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated.i, deployment.vector.updated.j))

}
