#'@title Generates a random deployment interval
#'
#'@param desired.deployment.interval = The desired deployment interval for the insecticide in generations
#'@param deployment.error = The number of generations the deployments may actually be out by.

stochastic_deployment = function(desired.deployment.interval,
                                 deployment.error){

  actual.error = round(runif(1, min = -deployment.error, max = deployment.error), digits = 0)
  stochastic.deployment.interval = desired.deployment.interval + actual.error

  if(stochastic.deployment.interval <= 0){
    while(stochastic.deployment.interval <= 0){
      actual.error = round(runif(1, min = -deployment.error, max = deployment.error), digits = 0)
      stochastic.deployment.interval = desired.deployment.interval + actual.error
    }}

  return(stochastic.deployment.interval)
}
