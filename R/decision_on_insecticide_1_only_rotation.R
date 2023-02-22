

decision_on_insecticide_1_only_rotation = function(number.of.insecticides,
                                                   current.generation,
                                                   withdrawal.threshold,
                                                   return.threshold,
                                                   simulation.array,
                                                   available.vector,
                                                   withdrawn.vector,
                                                   mixture.df,
                                                   current.mixture,
                                                   deployment.frequency,
                                                   deployment.df,
                                                   insecticide.parameters.df){


  #Step 1: confirm which insecticides are/are not available for deployment [same as with single insecticide deployment]
  list.available.withdrawn =  return_and_withdrawal_of_insecticides_from_arsenal(number.of.insecticides = number.of.insecticides,
                                                                                 current.generation = current.generation,
                                                                                 withdrawal.threshold = withdrawal.threshold,
                                                                                 return.threshold = return.threshold,
                                                                                 simulation.array = simulation.array,
                                                                                 available.vector = available.vector,
                                                                                 withdrawn.vector = withdrawn.vector)

  #note this section does not actually matter:::
  #get insecticides as vectors
  available.to.deploy = list.available.withdrawn[[1]]
  unavailable.to.deploy = list.available.withdrawn[[2]]

  available.mixtures.df = find_available_mixtures(mixture.df = mixture.df,
                                                  withdrawn.insecticides = unavailable.to.deploy)


  #return "pyrethroid":::
  if(1 %in% available.to.deploy == TRUE){
    available.to.deploy = available.to.deploy
  }

  if(1 %in% available.to.deploy == FALSE){
    available.to.deploy = c(1, available.to.deploy)
  }


  #And remove from unavailable:::
  if(1 %in% available.to.deploy == TRUE){
    unavailable.to.deploy = unavailable.to.deploy[!unavailable.to.deploy %in% 1]

  }

  if(1 %in% available.to.deploy == FALSE){
    unavailable.to.deploy = unavailable.to.deploy
  }

  available.mixtures.df = find_available_mixtures(mixture.df = mixture.df,
                                                  withdrawn.insecticides = unavailable.to.deploy)

  #break if no mixtures are available for deployment
  if(nrow(available.mixtures.df)==0){deployment.df.updated = deploy_mixture_with_decay(candidate.mixture.id = NA,
                                                                                       mixture.df = mixture.df,
                                                                                       deployment.df = deployment.df,
                                                                                       deployment.frequency = 1,
                                                                                       insecticide.parameters.df = insecticide.parameters.df)
  return(list(available.mixtures.df, available.vector, withdrawn.vector, deployment.df.updated))

  }
  else{
    candidate.mixture = choose_next_mixture(previous.mixture = current.mixture,
                                            total.mixtures = nrow(mixture.df),
                                            available.mixtures = available.mixtures.df$mixture.id)}

  #if the candidate insecticide is the currently deployed insecticide set deployed to NA to stop the simulation
  if(candidate.mixture == current.mixture){
    deployment.df.updated = deploy_mixture_with_decay(candidate.mixture.id = NA,
                                                      mixture.df = mixture.df,
                                                      deployment.df = deployment.df,
                                                      deployment.frequency = 1,
                                                      insecticide.parameters.df = insecticide.parameters.df)}
  #otherwise the candidate insecticide can be deployed.
  else{deployment.df.updated = deploy_mixture_with_decay(candidate.mixture.id = candidate.mixture,
                                                         mixture.df = mixture.df,
                                                         deployment.df = deployment.df,
                                                         deployment.frequency = deployment.frequency,
                                                         insecticide.parameters.df = insecticide.parameters.df)}

  return(list(available.mixtures.df, available.vector, withdrawn.vector, deployment.df.updated))
}


