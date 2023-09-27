irm_strategy_micromosaics_sequence = function(number.of.insecticides,
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

  list.available.withdrawn =  return_and_withdrawal_of_insecticides_from_arsenal(number.of.insecticides = number.of.insecticides,
                                                                                 current.generation = current.generation,
                                                                                 withdrawal.threshold = withdrawal.threshold,
                                                                                 return.threshold = return.threshold,
                                                                                 simulation.array = simulation.array,
                                                                                 available.vector = available.vector,
                                                                                 withdrawn.vector = withdrawn.vector)


  available.to.deploy = list.available.withdrawn[[1]]
  unavailable.to.deploy = list.available.withdrawn[[2]]


  #If not enough enough insecticides available stop the simulation.
  if(length(available.to.deploy) <= 1){

    deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.i)

    deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector.j)

  }else(##If both the previous insecticides are available re-deploy


  if(deployed.insecticide.i %in% available.to.deploy &
     deployed.insecticide.j %in% available.to.deploy){

    deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = deployed.insecticide.i,
                                                                deployment.frequency = deployment.frequency,
                                                                deployment.vector = deployment.vector.i)

    deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = deployed.insecticide.j,
                                                                deployment.frequency = deployment.frequency,
                                                                deployment.vector = deployment.vector.j)

  }else( #If only insecticide.i available:
    if(deployed.insecticide.i %in% available.to.deploy &
       deployed.insecticide.j %in% unavailable.to.deploy){

      deployment.vector.updated.i = deploy_the_chosen_insecticide(insecticide.to.deploy = deployed.insecticide.i,
                                                                  deployment.frequency = deployment.frequency,
                                                                  deployment.vector = deployment.vector.i)

      available.to.deploy.temp = available.to.deploy[!available.to.deploy %in% deployed.insecticide.i]


      candidate.insecticide = choose_the_next_insecticide(previous.insecticide = deployed.insecticide.j,
                                                          available.insecticides = available.to.deploy.temp,
                                                          number.of.insecticides = number.of.insecticides)

      deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = candidate.insecticide,
                                                                  deployment.frequency = deployment.frequency,
                                                                  deployment.vector = deployment.vector.j)


    }
    else(#If only insecticide.j available:#
      if(deployed.insecticide.i %in% unavailable.to.deploy &
         deployed.insecticide.j %in% available.to.deploy){


        deployment.vector.updated.j = deploy_the_chosen_insecticide(insecticide.to.deploy = deployed.insecticide.j,
                                                                    deployment.frequency = deployment.frequency,
                                                                    deployment.vector = deployment.vector.j)

        available.to.deploy.temp = available.to.deploy[!available.to.deploy %in% deployed.insecticide.j]


      candidate.insecticide = choose_the_next_insecticide(previous.insecticide = deployed.insecticide.i,
                                                          available.insecticides = available.to.deploy.temp,
                                                          number.of.insecticides = number.of.insecticides)

      deployment.vector.updated.i= deploy_the_chosen_insecticide(insecticide.to.deploy = candidate.insecticide,
                                                                  deployment.frequency = deployment.frequency,
                                                                  deployment.vector = deployment.vector.i)


      }
    else(##If neither insecticide i or j is available::
      if(deployed.insecticide.i %in% unavailable.to.deploy &
         deployed.insecticide.j %in% unavailable.to.deploy){


        candidate.insecticide.i = choose_the_next_insecticide(previous.insecticide = deployed.insecticide.i,
                                                            available.insecticides = available.to.deploy,
                                                            number.of.insecticides = number.of.insecticides)

        available.to.deploy.temp = available.to.deploy[!available.to.deploy %in% candidate.insecticide.i]

      candidate.insecticide.j = choose_the_next_insecticide(previous.insecticide = deployed.insecticide.j,
                                                            available.insecticides = available.to.deploy.temp,
                                                            number.of.insecticides = number.of.insecticides)

      deployment.vector.updated.i= deploy_the_chosen_insecticide(insecticide.to.deploy = candidate.insecticide.i,
                                                                 deployment.frequency = deployment.frequency,
                                                                 deployment.vector = deployment.vector.i)

      deployment.vector.updated.j= deploy_the_chosen_insecticide(insecticide.to.deploy = candidate.insecticide.j,
                                                                 deployment.frequency = deployment.frequency,
                                                                 deployment.vector = deployment.vector.j)
      }
    ))
    )
)

  return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated.i, deployment.vector.updated.j))


}
