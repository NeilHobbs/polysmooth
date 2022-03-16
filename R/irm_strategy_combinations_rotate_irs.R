
#'@param irs.insecticides = A vector containing the insecticides that can be deployed as an IRS
#'@param deployment.frequency.irs = How frequently is IRS deployed in mosquito generations
#'@param current.irs.insecticide = The current insecticide deployed as an IRS

irm_strategy_combinations_rotate_irs = function(number.of.insecticides,
                                                irs.insecticides,
                                                deployment.frequency.irs,
                                                available.vector,
                                                withdrawn.vector,
                                                withdrawal.threshold,
                                                return.threshold,
                                                current.generation,
                                                simulation.array,
                                                current.irs.insecticide,
                                                deployment.vector,
                                                deployment.interval.irs){


  #Step 1: confirm which insecticides are/are not available for deployment
  list.available.withdrawn = return_and_withdrawal_of_insecticides_from_arsenal(number.of.insecticides = number.of.insecticides,
                                                                                current.generation = current.generation,
                                                                                withdrawal.threshold = withdrawal.threshold,
                                                                                return.threshold = return.threshold,
                                                                                simulation.array = simulation.array,
                                                                                available.vector = available.vector,
                                                                                withdrawn.vector = withdrawn.vector)

  #For all insecticides::::
  available.to.deploy = list.available.withdrawn[[1]]
  unavailable.to.deploy = list.available.withdrawn[[2]]

  #Then find which IRS insecticides are available:::
  available.to.deploy.irs = intersect(irs.insecticides, available.to.deploy)


  #break if no insecticides are available for deployment
  if(length(available.to.deploy.irs)==0){deployment.vector.updated = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                                               deployment.frequency = 1,
                                                                                               deployment.vector = deployment.vector)}
  else{
    candidate.insecticide = choose_the_next_insecticide(previous.insecticide = current.irs.insecticide,
                                                        available.insecticides = available.to.deploy.irs,
                                                        number.of.insecticides = number.of.insecticides)

    #if the candidate insecticide is the currently deployed insecticide set deployed to NA to stop the simulation
    if(candidate.insecticide == current.irs.insecticide){
      deployment.vector.updated = deploy_the_chosen_insecticide(insecticide.to.deploy = NA,
                                                                deployment.frequency = 1,
                                                                deployment.vector = deployment.vector)}
    #otherwise the candidate insecticide can be deployed.
    else{deployment.vector.updated = deploy_the_chosen_insecticide(insecticide.to.deploy = candidate.insecticide,
                                                                   deployment.frequency = deployment.interval.irs,
                                                                   deployment.vector = deployment.vector)}
  }


  return(list(available.to.deploy, unavailable.to.deploy, deployment.vector.updated))


}
