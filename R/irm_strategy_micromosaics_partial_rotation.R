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



  ##otherwise...::::
  ###replace the lowest number insecticide
    #Note that it would be more practical to replace the insecticide to which there is the most resistance...
  to.replace = min(c(deployed.insecticide.i, deployed.insecticide.j))
  to.remain.deployed = max(c(deployed.insecticide.i, deployed.insecticide.j))

  if(to.remain.deployed %in% available.to.deploy){
    available.to.deploy.temp = available.to.deploy[!available.to.deploy %in% to.remain.deployed]
    available.to.deploy.temp = available.to.deploy.temp[!available.to.deploy.temp %in% to.replace]

    if(length(available.to.deploy.temp) >= 1){
      candidate.insecticide = choose_the_next_insecticide(previous.insecticide = to.remain.deployed,
                                                          available.insecticides = available.to.deploy.temp,
                                                          number.of.insecticides = number.of.insecticides)

      deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = to.remain.deployed,
                                                                  deployment.frequency = deployment.frequency,
                                                                  deployment.vector = deployment.vector.i)

      deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = candidate.insecticide,
                                                                  deployment.frequency = deployment.frequency,
                                                                  deployment.vector = deployment.vector.j)

      return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated.i, deployment.vector.updated.j, "A"))
    }else{
      deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                  deployment.frequency = 1,
                                                                  deployment.vector = deployment.vector.i)
      deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                  deployment.frequency = 1,
                                                                  deployment.vector = deployment.vector.j)

      return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated.i, deployment.vector.updated.j, "B"))
    }

  }

    #However if the previous option not viable try switching the other insecticide out instead

    if(to.remain.deployed %in% unavailable.to.deploy &
       to.replace %in% available.to.deploy){
      available.to.deploy.temp = available.to.deploy[!available.to.deploy %in% to.remain.deployed]
      available.to.deploy.temp = available.to.deploy.temp[!available.to.deploy.temp %in% to.replace]

      if(length(available.to.deploy.temp) >= 1){
        candidate.insecticide = choose_the_next_insecticide(previous.insecticide = to.replace,
                                                            available.insecticides = available.to.deploy.temp,
                                                            number.of.insecticides = number.of.insecticides)

        deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = to.replace,
                                                                    deployment.frequency = deployment.frequency,
                                                                    deployment.vector = deployment.vector.i)

        deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = candidate.insecticide,
                                                                    deployment.frequency = deployment.frequency,
                                                                    deployment.vector = deployment.vector.j)

        return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated.i, deployment.vector.updated.j), "C")
      }else{
        deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                    deployment.frequency = 1,
                                                                    deployment.vector = deployment.vector.i)
        deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                    deployment.frequency = 1,
                                                                    deployment.vector = deployment.vector.j)

        return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated.i, deployment.vector.updated.j), "D")
      }
    }
  }










