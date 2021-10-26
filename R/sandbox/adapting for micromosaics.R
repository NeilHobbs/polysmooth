micromosaic = function(max.cycles,
                       insecticide.coverage.1,
                       insecticide.coverage.2,
                       trait.mean.1,
                       trait.mean.2,
                       standard.deviation,
                       vector.length,
                       female.exposure,
                       male.selection.diff,
                       current.insecticide.efficacy.1,
                       current.insecticide.efficacy.2){

  norm.dist.1 = create_normal_distribution(vector.length = vector.length,
                                         trait.mean = trait.mean.1,
                                         standard.deviation = standard.deviation)

  norm.dist.2 = create_normal_distribution(vector.length = vector.length,
                                           trait.mean = trait.mean.2,
                                           standard.deviation = standard.deviation)


  rel.dens = calculate_density_of_trait_values(vector.length = vector.length,
                                               trait.mean = 0, #value does not matter
                                               standard.deviation = standard.deviation)

  survival.probability.1 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = norm.dist.1),
                                                                     regression.coefficient = 0.48,
                                                                     regression.intercept = 0.15,
                                                                     current.insecticide.efficacy = current.insecticide.efficacy.1)

  survival.probability.2 = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = norm.dist.2),
                                                                       regression.coefficient = 0.48,
                                                                       regression.intercept = 0.15,
                                                                       current.insecticide.efficacy = current.insecticide.efficacy.2)

   do.not.encounter = 1 - ((insecticide.coverage.1 + insecticide.coverage.2) * female.exposure)

  update.density = list()
  update.mean.z = list()
  pop.size = list()
  selection.diff = list()
  response = list()

  #Need to figure out a way to put in dispersal in each gonotrophic cycle???

  for(i in 1:max.cycles){

    if(i == 1){temp.vec = ((rel.dens*do.not.encounter) + (rel.dens*female.exposure*insecticide.coverage.2*mean(survival.probability.2)) + (rel.dens*female.exposure*insecticide.coverage.1*survival.probability.1))}
    else(temp.vec = ((update.density[[i-1]]*do.not.encounter) + (update.density[[i-1]]*female.exposure*insecticide.coverage.2*mean(survival.probability.2)) + (update.density[[i-1]]*insecticide.coverage.1*female.exposure*survival.probability.1)))

    update.density[[i]] = temp.vec

    pop.size[[i]] = sum(update.density[[i]])

    relative.contribution.after.selection.scaled = (rel.dens)/(update.density[[i]] * norm.dist.1)

    update.mean.z[[i]] = (sum(norm.dist.1 * update.density[[i]]))/ pop.size[[i]]

    selection.diff[[i]] = update.mean.z[[i]] - trait.mean.1

    response[[i]] = 0.3 * 10 * ((selection.diff[[i]] + male.selection.diff) / 2)

  }
  total.oviposition = sum(unlist(pop.size))
  overall.response.1 = sum(unlist(response)*(unlist(pop.size)/total.oviposition))

  return(overall.response.1)
}

# micromosaic(max.cycles = 100,
#             insecticide.coverage.1  = 1,
#             insecticide.coverage.2 = 1,
#             trait.mean.1 = 0,
#             trait.mean.2 = 0,
#             standard.deviation = 10,
#             vector.length = 1000,
#             female.exposure = 1,
#             male.selection.diff = 0,
#             current.insecticide.efficacy.1 = 1,
#             current.insecticide.efficacy.2 = 1)

#create some parameter space:
# trait.mean.1.vals = rep(runif(1000, 0, 900), 2)
# trait.mean.2.vals = rep(runif(1000, 0, 900), 2)
# female.exposure.vals = rep(runif(1000, 0, 1), 2)
# efficacy.vals.1 = rep(runif(1000, 0, 1.2), 2)
# efficacy.vals.2 = c(runif(1000, 0, 0), runif(1000, 0, 1.2))
# coverage.1 = c(rep(1, 1000), rep(0.5, 1000))
# coverage.2 = c(rep(0, 1000), rep(0.5, 1000))



#
#
# trait.mean.1.vals = rep(100, 101)
# trait.mean.2.vals = rep(0, 101)
# female.exposure.vals = rep(1, 101)
# efficacy.vals.1 = rep(0.2, 101)
# efficacy.vals.2 = rep(0.2, 101)
# coverage.1 = seq(0, 1, by = 0.01)
# coverage.2 = seq(1, 0, by = -0.01)
#
# response.vals.1 = c()
#
# for(i in 1:length(trait.mean.1.vals)){
#   response.vals.1[i] = micromosaic(max.cycles = 5,
#             insecticide.coverage.1 = coverage.1[i],
#             insecticide.coverage.2 = coverage.2[i],
#             trait.mean.1 = trait.mean.1.vals[i],
#             trait.mean.2 = trait.mean.2.vals[i],
#             standard.deviation = 20,
#             vector.length = 1000,
#             female.exposure = female.exposure.vals[i],
#             male.selection.diff = 1,
#             current.insecticide.efficacy.1 = efficacy.vals.1[i],
#             current.insecticide.efficacy.2 = efficacy.vals.2[i])
# }
#
#
# plot(coverage.1, response.vals.1)

