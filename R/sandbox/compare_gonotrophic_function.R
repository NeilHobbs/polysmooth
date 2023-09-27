compare_gonotrophic_function = function(sd.scaled = TRUE, ##TRUE or FALSE
                                        female.fitness.cost = 0,
                                        male.fitness.cost = 0,
                                        female.exposure = 0.7,
                                        male.exposure = 0.7,
                                        heritability = 0.3,
                                        dispersal.rate = 0.3,
                                        coverage = 0.8,
                                        standard.deviation = 50,
                                        vector.length = 100,
                                        min.cross.selection = 0,
                                        max.cross.selection = 0){

micromosaics.list = list()
combimicro.list = list()
rotations.list = list()
sequences.list = list()
mixtures.list = list()
micro.solo.list = list()

g.cycles = c(1, 5)


# for(i in 1:length(g.cycles)){
#   combimicro.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
#                         irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
#                         number.of.insecticides = 2,
#                         sd.scaled = sd.scaled, ##TRUE or FALSE
#                         exposure.scaling.factor = 10,
#                         female.fitness.cost = female.fitness.cost,
#                         male.fitness.cost = male.fitness.cost,
#                         female.exposure = female.exposure,
#                         male.exposure = male.exposure,
#                         heritability = heritability,
#                         dispersal.rate = dispersal.rate,
#                         coverage = coverage,
#                         standard.deviation = 50,
#                         vector.length = vector.length,
#                         maximum.bioassay.survival.proportion = 1,
#                         michaelis.menten.slope = 1,
#                         regression.coefficient = 0.48,
#                         regression.intercept = 0.15,
#                         maximum.generations = 100,
#                         half.population.bioassay.survival.resistance = 900,
#                         withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                         return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                         deployment.frequency = 100, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                         maximum.resistance.value = 25000,
#                         starting.refugia.resistance.score = 0,
#                         starting.intervention.resistance.score = 0,
#                         applied.insecticide.dose = 1,
#                         recommended.insecticide.dose = 1,
#                         threshold.generations = 0,
#                         base.efficacy.decay.rate = 0,
#                         rapid.decay.rate = 0,
#                         deployment.interval.llin = 100, #only for combinations
#                         deployment.interval.irs = 100, #only for combinations
#                         probability.only.i.male = 0, #only for combinations
#                         probability.only.j.male = 0, #only for combinations
#                         probability.both.i.j.male = 0, #only for combinations
#                         probability.only.i.female = 0, #only for combinations
#                         probability.only.j.female = 0, #only for combinations
#                         probability.both.i.j.female = 0, #only for combinations
#                         n.cycles = g.cycles[i],
#                         intervention.coverage.1 = 0.5,
#                         intervention.coverage.2 = 0.5,
#                         intervention.coverage.1.2 = 0,
#                         z.sd.intercept = 18,
#                         z.sd.coefficient = 0.4,
#                         mixture.strategy = "pyrethroid.plus",
#                         llin.insecticides = 1,
#                         irs.insecticides = 2,
#                         min.cross.selection = min.cross.selection,
#                         max.cross.selection = max.cross.selection)
# }

for(i in 1:length(g.cycles)){
  micromosaics.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "micromosaics", #singles, mixtures, micromosaics, combinations
                                                   irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                                   number.of.insecticides = 2,
                                                   sd.scaled = sd.scaled, ##TRUE or FALSE
                                                   exposure.scaling.factor = 10,
                                                   female.fitness.cost = female.fitness.cost,
                                                   male.fitness.cost = male.fitness.cost,
                                                   female.exposure = female.exposure,
                                                   male.exposure = male.exposure,
                                                   heritability = heritability,
                                                   dispersal.rate = dispersal.rate,
                                                   coverage = coverage,
                                                   standard.deviation = 50,
                                                   vector.length = vector.length,
                                                   maximum.bioassay.survival.proportion = 1,
                                                   michaelis.menten.slope = 1,
                                                   regression.coefficient = 0.48,
                                                   regression.intercept = 0.15,
                                                   maximum.generations = 100,
                                                   half.population.bioassay.survival.resistance = 900,
                                                   withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                   return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                   deployment.frequency = 100, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                   maximum.resistance.value = 25000,
                                                   starting.refugia.resistance.score = 0,
                                                   starting.intervention.resistance.score = 0,
                                                   applied.insecticide.dose = 1,
                                                   recommended.insecticide.dose = 1,
                                                   threshold.generations = 0,
                                                   base.efficacy.decay.rate = 0,
                                                   rapid.decay.rate = 0,
                                                   deployment.interval.llin = 30, #only for combinations
                                                   deployment.interval.irs = 10, #only for combinations
                                                   probability.only.i.male = 1, #only for combinations
                                                   probability.only.j.male = 1, #only for combinations
                                                   probability.both.i.j.male = 0, #only for combinations
                                                   probability.only.i.female = 1, #only for combinations
                                                   probability.only.j.female = 1, #only for combinations
                                                   probability.both.i.j.female = 0, #only for combinations
                                                   n.cycles = g.cycles[i],
                                                   intervention.coverage.1 = 0.5,
                                                   intervention.coverage.2 = 0.5,
                                                   intervention.coverage.1.2 = 0,
                                                   z.sd.intercept = 18,
                                                   z.sd.coefficient = 0.4,
                                                   mixture.strategy = "pyrethroid.plus",
                                                   llin.insecticides = 1,
                                                   irs.insecticides = 2,
                                                   min.cross.selection = min.cross.selection,
                                                   max.cross.selection = max.cross.selection)
}

for(i in 1:length(g.cycles)){
  rotations.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                                                  irm.switch.strategy = "rotation", #"rotation", "sequence", "insecticide.1"
                                                  number.of.insecticides = 2,
                                                  sd.scaled = sd.scaled, ##TRUE or FALSE
                                                  exposure.scaling.factor = 10,
                                                  female.fitness.cost = female.fitness.cost,
                                                  male.fitness.cost = male.fitness.cost,
                                                  female.exposure = female.exposure,
                                                  male.exposure = male.exposure,
                                                  heritability = heritability,
                                                  dispersal.rate = dispersal.rate,
                                                  coverage = coverage,
                                                  standard.deviation = 50,
                                                  vector.length = vector.length,
                                                  maximum.bioassay.survival.proportion = 1,
                                                  michaelis.menten.slope = 1,
                                                  regression.coefficient = 0.48,
                                                  regression.intercept = 0.15,
                                                  maximum.generations = 100,
                                                  half.population.bioassay.survival.resistance = 900,
                                                  withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                  return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                  deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                  maximum.resistance.value = 25000,
                                                  starting.refugia.resistance.score = 0,
                                                  starting.intervention.resistance.score = 0,
                                                  applied.insecticide.dose = 1,
                                                  recommended.insecticide.dose = 1,
                                                  threshold.generations = 0,
                                                  base.efficacy.decay.rate = 0,
                                                  rapid.decay.rate = 0,
                                                  deployment.interval.llin = 30, #only for combinations
                                                  deployment.interval.irs = 10, #only for combinations
                                                  probability.only.i.male = 0.7, #only for combinations
                                                  probability.only.j.male = 0.2, #only for combinations
                                                  probability.both.i.j.male = 0.1, #only for combinations
                                                  probability.only.i.female = 0.4, #only for combinations
                                                  probability.only.j.female = 0.2, #only for combinations
                                                  probability.both.i.j.female = 0.4, #only for combinations
                                                  n.cycles = g.cycles[i],
                                                  intervention.coverage.1 = 0.5,
                                                  intervention.coverage.2 = 0.5,
                                                  intervention.coverage.1.2 = NA,
                                                  z.sd.intercept = 18,
                                                  z.sd.coefficient = 0.4,
                                                  mixture.strategy = "pyrethroid.plus",
                                                  llin.insecticides,
                                                  irs.insecticides,
                                                  min.cross.selection = min.cross.selection,
                                                  max.cross.selection = max.cross.selection)
}

for(i in 1:length(g.cycles)){
  sequences.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                                                irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                                number.of.insecticides = 1,
                                                sd.scaled = sd.scaled, ##TRUE or FALSE
                                                exposure.scaling.factor = 10,
                                                female.fitness.cost = female.fitness.cost,
                                                male.fitness.cost = male.fitness.cost,
                                                female.exposure = female.exposure,
                                                male.exposure = male.exposure,
                                                heritability = heritability,
                                                dispersal.rate = dispersal.rate,
                                                coverage = coverage,
                                                standard.deviation = 50,
                                                vector.length = vector.length,
                                                maximum.bioassay.survival.proportion = 1,
                                                michaelis.menten.slope = 1,
                                                regression.coefficient = 0.48,
                                                regression.intercept = 0.15,
                                                maximum.generations = 100,
                                                half.population.bioassay.survival.resistance = 900,
                                                withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                deployment.frequency = 100, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                maximum.resistance.value = 25000,
                                                starting.refugia.resistance.score = 0,
                                                starting.intervention.resistance.score = 0,
                                                applied.insecticide.dose = 1,
                                                recommended.insecticide.dose = 1,
                                                threshold.generations = 0,
                                                base.efficacy.decay.rate = 0,
                                                rapid.decay.rate = 0,
                                                deployment.interval.llin = 30, #only for combinations
                                                deployment.interval.irs = 10, #only for combinations
                                                probability.only.i.male = 0.7, #only for combinations
                                                probability.only.j.male = 0.2, #only for combinations
                                                probability.both.i.j.male = 0.1, #only for combinations
                                                probability.only.i.female = 0.4, #only for combinations
                                                probability.only.j.female = 0.2, #only for combinations
                                                probability.both.i.j.female = 0.4, #only for combinations
                                                n.cycles = g.cycles[i],
                                                intervention.coverage.1 = 0.5,
                                                intervention.coverage.2 = 0.5,
                                                intervention.coverage.1.2 = NA,
                                                z.sd.intercept = 18,
                                                z.sd.coefficient = 0.4,
                                                mixture.strategy = "pyrethroid.plus",
                                                llin.insecticides,
                                                irs.insecticides,
                                                min.cross.selection = min.cross.selection,
                                                max.cross.selection = max.cross.selection)
}

for(i in 1:length(g.cycles)){
  micro.solo.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "micromosaics", #singles, mixtures, micromosaics, combinations
                                                   irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                                   number.of.insecticides = 2,
                                                   sd.scaled = sd.scaled, ##TRUE or FALSE
                                                   exposure.scaling.factor = 10,
                                                   female.fitness.cost = female.fitness.cost,
                                                   male.fitness.cost = male.fitness.cost,
                                                   female.exposure = female.exposure,
                                                   male.exposure = male.exposure,
                                                   heritability = heritability,
                                                   dispersal.rate = dispersal.rate,
                                                   coverage = coverage,
                                                   standard.deviation = 50,
                                                   vector.length = vector.length,
                                                   maximum.bioassay.survival.proportion = 1,
                                                   michaelis.menten.slope = 1,
                                                   regression.coefficient = 0.48,
                                                   regression.intercept = 0.15,
                                                   maximum.generations = 100,
                                                   half.population.bioassay.survival.resistance = 900,
                                                   withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                   return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                   deployment.frequency = 100, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                   maximum.resistance.value = 25000,
                                                   starting.refugia.resistance.score = 0,
                                                   starting.intervention.resistance.score = 0,
                                                   applied.insecticide.dose = 1,
                                                   recommended.insecticide.dose = 1,
                                                   threshold.generations = 0,
                                                   base.efficacy.decay.rate = 0,
                                                   rapid.decay.rate = 0,
                                                   deployment.interval.llin = 30, #only for combinations
                                                   deployment.interval.irs = 10, #only for combinations
                                                   probability.only.i.male = 0.7, #only for combinations
                                                   probability.only.j.male = 0.2, #only for combinations
                                                   probability.both.i.j.male = 0.1, #only for combinations
                                                   probability.only.i.female = 0.4, #only for combinations
                                                   probability.only.j.female = 0.2, #only for combinations
                                                   probability.both.i.j.female = 0.4, #only for combinations
                                                   n.cycles = g.cycles[i],
                                                   intervention.coverage.1 = 1,
                                                   intervention.coverage.2 = 0,
                                                   intervention.coverage.1.2 = NA,
                                                   z.sd.intercept = 18,
                                                   z.sd.coefficient = 0.4,
                                                   mixture.strategy = "pyrethroid.plus",
                                                   llin.insecticides,
                                                   irs.insecticides,
                                                   min.cross.selection = min.cross.selection,
                                                   max.cross.selection = max.cross.selection)
}

for(i in 1:length(g.cycles)){
  mixtures.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                               irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                               number.of.insecticides = 2,
                                               sd.scaled = sd.scaled, ##TRUE or FALSE
                                               exposure.scaling.factor = 10,
                                               female.fitness.cost = female.fitness.cost,
                                               male.fitness.cost = male.fitness.cost,
                                               female.exposure = female.exposure,
                                               male.exposure = male.exposure,
                                               heritability = heritability,
                                               dispersal.rate = dispersal.rate,
                                               coverage = coverage,
                                               standard.deviation = 50,
                                               vector.length = vector.length,
                                               maximum.bioassay.survival.proportion = 1,
                                               michaelis.menten.slope = 1,
                                               regression.coefficient = 0.48,
                                               regression.intercept = 0.15,
                                               maximum.generations = 100,
                                               half.population.bioassay.survival.resistance = 900,
                                               withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                               return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                               deployment.frequency = 100, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                               maximum.resistance.value = 25000,
                                               starting.refugia.resistance.score = 0,
                                               starting.intervention.resistance.score = 0,
                                               applied.insecticide.dose = 1,
                                               recommended.insecticide.dose = 1,
                                               threshold.generations = 0,
                                               base.efficacy.decay.rate = 0,
                                               rapid.decay.rate = 0,
                                               deployment.interval.llin = 30, #only for combinations
                                               deployment.interval.irs = 10, #only for combinations
                                               probability.only.i.male = 0.7, #only for combinations
                                               probability.only.j.male = 0.2, #only for combinations
                                               probability.both.i.j.male = 0.1, #only for combinations
                                               probability.only.i.female = 0.4, #only for combinations
                                               probability.only.j.female = 0.2, #only for combinations
                                               probability.both.i.j.female = 0.4, #only for combinations
                                               n.cycles = g.cycles[i],
                                               intervention.coverage.1 = 1,
                                               intervention.coverage.2 = 0,
                                               intervention.coverage.1.2 = NA,
                                               z.sd.intercept = 18,
                                               z.sd.coefficient = 0.4,
                                               mixture.strategy = "pyrethroid.plus",
                                               llin.insecticides,
                                               irs.insecticides,
                                               min.cross.selection = 0,
                                               max.cross.selection = 0)
}




single.g = ggplot(micromosaics.list[[1]], aes(x=time.in.generations,
                                  y=resistance.score))+
  geom_point(colour = "red")+
  # geom_point(data =combimicro.list[[1]], aes(x=time.in.generations,
  #                                           y=resistance.score),
  #            colour = "green")+
  geom_point(data =rotations.list[[1]], aes(x=time.in.generations,
                                           y=resistance.score),
             colour = "blue")+
  geom_point(data =sequences.list[[1]], aes(x=time.in.generations,
                                            y=resistance.score),
             colour = "yellow")+
  # geom_point(data =micro.solo.list[[1]], aes(x=time.in.generations,
  #                                           y=resistance.score),
  #            colour = "orange")+
  geom_point(data =mixtures.list[[1]], aes(x=time.in.generations,
                                            y=resistance.score),
             colour = "purple")+
  ggtitle("1 gonotrophic cycle")+
  facet_wrap(~site)+
  theme_classic()


multi.g = ggplot(micromosaics.list[[2]], aes(x=time.in.generations,
                                                y=resistance.score))+
    geom_point(colour = "red")+
    geom_point(data =rotations.list[[2]], aes(x=time.in.generations,
                                              y=resistance.score),
               colour = "blue")+
    geom_point(data =sequences.list[[2]], aes(x=time.in.generations,
                                              y=resistance.score),
               colour = "yellow")+
  # geom_point(data =micro.solo.list[[2]], aes(x=time.in.generations,
  #                                            y=resistance.score),
  #            colour = "orange", size = 2)+
    geom_point(data =mixtures.list[[2]], aes(x=time.in.generations,
                                             y=resistance.score),
               colour = "purple")+
  ggtitle("5 gonotrophic cycles")+
  facet_wrap(~site)+
  theme_classic()

the.plot = single.g / multi.g

return(the.plot)
}

compare_gonotrophic_function(sd.scaled = FALSE, ##TRUE or FALSE
                             female.fitness.cost = 0,
                             male.fitness.cost = 0,
                             female.exposure = 0.7,
                             male.exposure = 0.7,
                             heritability = 0.3,
                             dispersal.rate = 0.3,
                             coverage = 0.8,
                             standard.deviation = 50,
                             vector.length = 250,
                             min.cross.selection = -0.3,
                             max.cross.selection = -0.3)



