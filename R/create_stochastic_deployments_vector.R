#'@title Creates a vector of stochastic deployment intervals
#'
#'@description It is unlikely that insecticide deployments will be conducted perfectly so that they are deployed at the exact desired intervals.
#'This function allow for the creation of vector of stochastic deployment intervals to better represent the noise that can go on in the field.
#'
#'@param desired.deployment.interval = The desired deployment interval for insecticide deployments in mosquito generations
#'@param deployment.error = The error in the timing of insecticide deployments in mosquito generations.
#'@param max.generations = The maximum number of generations the simulation is running for.

create_stochastic_deployments_vector = function(desired.deployment.interval,
                                                deployment.error,
                                                max.generations){
  actual.deployment.intervals = c()

  for(i in 1:10000){#set arbitraily high

    actual.deployment.intervals[i] = stochastic_deployment(desired.deployment.interval = desired.deployment.interval,
                                                           deployment.error = deployment.error)

    if(actual.deployment.intervals[i] > max.generations){actual.deployment.intervals[i] = max.generations}

  }

  #Create a cumulative sum of the randomly generated deployments
  cumulative.deployments = cumsum(actual.deployment.intervals)

  #If the cumulative is greater than total deployment, set as NA
  cumulative.deployments = ifelse(cumulative.deployments > max.generations,
                                  yes = NA,
                                  no = cumulative.deployments)

  #Remove the NAs
  cumulative.deployments = cumulative.deployments[!is.na(cumulative.deployments)]

  #Take the actual deployments
  actual.deployment.intervals = actual.deployment.intervals[1:length(cumulative.deployments)]

  if(max(cumulative.deployments) != max.generations){

    additional.deployment = max.generations - max(cumulative.deployments)
    actual.deployment.intervals = c(actual.deployment.intervals, additional.deployment)

    return(actual.deployment.intervals)
  }

  if(max(cumulative.deployments) == max.generations){
    return(actual.deployment.intervals)
  }

}
