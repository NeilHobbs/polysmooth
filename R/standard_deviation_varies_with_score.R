#It may be feasible that the standard deviation of the population varies with
# the mean polygenic resistance score. We may expect the standard deviation to increase
#with increasing mean values to highlight an increasingly variable population.

#How to implement that is challenging as this is an area where field data is not
# available, i.e. how to scale appropriately. A simple method would be to allow
#linear increase in the SD a proportion of the initial zero PRS SD.

standard_deviation_varies_with_score = function(standard.deviation.at.zero,
                                                current.trait.mean){

  increase = current.trait.mean / standard.deviation.at.zero

  current.standard.deviation = standard.deviation.at.zero + increase

  return(current.standard.deviation)
}

# trait.mean.values = seq(1, 900, 1)
# new.sd = c()
#
# for(i in 1:900){
# new.sd[i] = standard_deviation_varies_with_score(standard.deviation.at.zero = 20,
#                                      current.trait.mean = trait.mean.values[i])
# }
#
# plot(trait.mean.values, new.sd)
