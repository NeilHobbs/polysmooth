##MIXTURES
  #Impact of Dosing
  #Impact of Decay
  #Impact of Resistance

# df = data.frame(1)
library(devtools)
load_all()

parameter_space_smooth = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv")
parameter_space_smooth = parameter_space_smooth%>%
  dplyr::filter(Intervention.Coverage >= 0.5)

# set1 = 1:250
# set2 = 251:500
# set3 = 501:750
# set4 = 751:1000
# set5 = 1001:1250
# set6 = 1251:1500
# set7 = 1501:1750
# set8 = 1751:2000
# set9 = 2001:2250
# set10 = 2251:2500



impact_decay_dosing_resistance_mixtures = function(parameter.space.df,
                                                   min_i,
                                                   max_i){

mixture.list = list()

for(j in min_i:max_i){


  base.decay.novel = rep(c(0.005, 0.015, 0.025), 3)
  threshold.gens.novel = c(10, 10, 10, 15, 15, 15, 20, 20, 20)

  novel.solo = c()
  for(i in 1:9){
    A = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                                irm.switch.strategy = "sequence", #"rotation", "sequence", "novel.sequence"
                                mixture.strategy = NA,
                                number.of.insecticides = 1,
                                sd.scaled = TRUE, ##TRUE or FALSE
                                exposure.scaling.factor = 10,
                                female.fitness.cost = 0,
                                male.fitness.cost = 0,
                                female.exposure = parameter.space.df$Female.Insecticide.Exposure[j],
                                male.exposure = parameter.space.df$Male.Insecticide.Exposure[j],
                                heritability = parameter.space.df$Heritability[j],
                                dispersal.rate = parameter.space.df$Dispersal[j],
                                coverage = parameter.space.df$Intervention.Coverage[j],
                                standard.deviation = 50,
                                vector.length = 200,
                                maximum.bioassay.survival.proportion = 1,
                                michaelis.menten.slope = 1,
                                regression.coefficient = 0.48,
                                regression.intercept = 0.15,
                                maximum.generations = 200,
                                half.population.bioassay.survival.resistance = 900,
                                withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                maximum.resistance.value = 90000,
                                starting.refugia.resistance.score = 0,
                                starting.intervention.resistance.score = 0,
                                applied.insecticide.dose = 1,
                                recommended.insecticide.dose = 1,
                                threshold.generations = threshold.gens.novel[i],
                                base.efficacy.decay.rate = base.decay.novel[i],
                                rapid.decay.rate = 0.08,
                                deployment.interval.llin = NA, #only for combinations
                                deployment.interval.irs = NA, #only for combinations
                                probability.only.i.male = NA, #only for combinations
                                probability.only.j.male = NA, #only for combinations
                                probability.both.i.j.male = NA, #only for combinations
                                probability.only.i.female = NA, #only for combinations
                                probability.only.j.female = NA, #only for combinations
                                probability.both.i.j.female = NA, #only for combinations
                                n.cycles = 1,
                                intervention.coverage.1 = NA,
                                intervention.coverage.2 = NA,
                                intervention.coverage.1.2 = NA,
                                z.sd.coefficient = 0.4,
                                z.sd.intercept = 18,
                                llin.insecticides = NA,
                                irs.insecticides = NA,
                                min.cross.selection = 0,
                                max.cross.selection = 0)


    B = A%>%
      dplyr::filter(site == "intervention")

    novel.solo[i] = max(B$resistance.score)

    print(c(i, j))
  }

  novel.solo.df = data.frame(novel.solo, base.decay.novel, threshold.gens.novel)




  base.decay.pyrethroid = rep(rep(c(0.005, 0.015, 0.025), 3), 4)
  threshold.gens.pyrethroid = rep(c(10, 10, 10, 15, 15, 15, 20, 20, 20), 4)
  start.pyrethroid.resistance = c(rep(0, 9), rep(100, 9), rep(900, 9), rep(3600, 9))

  #Pyrethroid Solo
  pyrethroid.solo = c()
  for(i in 1:36){

    A = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                                irm.switch.strategy = "sequence", #"rotation", "sequence", "novel.sequence"
                                mixture.strategy = NA,
                                number.of.insecticides = 1,
                                sd.scaled = TRUE, ##TRUE or FALSE
                                exposure.scaling.factor = 10,
                                female.fitness.cost = 0,
                                male.fitness.cost = 0,
                                female.exposure = parameter.space.df$Female.Insecticide.Exposure[j],
                                male.exposure = parameter.space.df$Male.Insecticide.Exposure[j],
                                heritability = parameter.space.df$Heritability[j],
                                dispersal.rate = parameter.space.df$Dispersal[j],
                                coverage = parameter.space.df$Intervention.Coverage[j],
                                standard.deviation = 50,
                                vector.length = 200,
                                maximum.bioassay.survival.proportion = 1,
                                michaelis.menten.slope = 1,
                                regression.coefficient = 0.48,
                                regression.intercept = 0.15,
                                maximum.generations = 200,
                                half.population.bioassay.survival.resistance = 900,
                                withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                maximum.resistance.value = 90000,
                                starting.refugia.resistance.score = start.pyrethroid.resistance[i],
                                starting.intervention.resistance.score = start.pyrethroid.resistance[i],
                                applied.insecticide.dose = 1,
                                recommended.insecticide.dose = 1,
                                threshold.generations = 0,
                                base.efficacy.decay.rate = 0,
                                rapid.decay.rate = 0,
                                deployment.interval.llin = NA, #only for combinations
                                deployment.interval.irs = NA, #only for combinations
                                probability.only.i.male = NA, #only for combinations
                                probability.only.j.male = NA, #only for combinations
                                probability.both.i.j.male = NA, #only for combinations
                                probability.only.i.female = NA, #only for combinations
                                probability.only.j.female = NA, #only for combinations
                                probability.both.i.j.female = NA, #only for combinations
                                n.cycles = 1,
                                intervention.coverage.1 = NA,
                                intervention.coverage.2 = NA,
                                intervention.coverage.1.2 = NA,
                                z.sd.coefficient = 0.4,
                                z.sd.intercept = 18,
                                llin.insecticides = NA,
                                irs.insecticides = NA,
                                min.cross.selection = 0,
                                max.cross.selection = 0)


    B = A%>%
      dplyr::filter(site == "intervention")

    pyrethroid.solo[i] = max(B$resistance.score)

    print(c(i, j))
    }

  pyrethroid.solo.df = data.frame(base.decay.pyrethroid, threshold.gens.pyrethroid, pyrethroid.solo, start.pyrethroid.resistance)


  #Then MIXTURES
  #threshold gens should be equal; this is more to do with the physical structure of the net etc - so would be the same for both insecticides.

  base.decay.1 = rep(rep(rep(rep(c(0.005, 0.015, 0.025), 3), 3), 4), 4)

  base.decay.2 = rep(rep(rep(c(rep(0.005, 3), rep(0.015, 3), rep(0.025, 3)), 3), 4), 4)

  #threshold gens should be equal; this is more to do with the physical structure of the net etc - so would be the same for both insecticides.
  threshold.gens = rep(rep(c(rep(10, 9), rep(15, 9), rep(20, 9)), 4), 4)

  start.resistance.2 = rep(c(rep(0, 27), rep(100, 27), rep(900, 27), rep(3600, 27)), 4)
  dose.1 = c(rep(1, 108), rep(1, 108), rep(0.5, 108), rep(0.5, 108))
  dose.2 = c(rep(1, 108), rep(0.5, 108), rep(1, 108), rep(0.5, 108))




  mixture.pyrethroid = c()
  mixture.novel = c()
  for(i in 1:432){

    sim.df = run_simulation_advanced(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                     irm.switch.strategy = "sequence", #"rotation", "sequence", "novel.sequence"
                                     mixture.strategy = "mix.sequential.discrete",
                                     number.of.insecticides = 2,
                                     sd.scaled = TRUE, ##TRUE or FALSE
                                     exposure.scaling.factor = 10,
                                     female.fitness.cost = 0,
                                     male.fitness.cost = 0,
                                     female.exposure = parameter.space.df$Female.Insecticide.Exposure[j],
                                     male.exposure = parameter.space.df$Male.Insecticide.Exposure[j],
                                     heritability = parameter.space.df$Heritability[j],
                                     dispersal.rate = parameter.space.df$Dispersal[j],
                                     coverage = parameter.space.df$Intervention.Coverage[j],
                                     standard.deviation = 50,
                                     vector.length = 200,
                                     maximum.bioassay.survival.proportion = 1,
                                     michaelis.menten.slope = 1,
                                     regression.coefficient = 0.48,
                                     regression.intercept = 0.15,
                                     maximum.generations = 200,
                                     half.population.bioassay.survival.resistance = 900,
                                     withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                     return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                     deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                     maximum.resistance.value = 90000,
                                     starting.refugia.resistance.score = c(0, start.resistance.2[i]),
                                     starting.intervention.resistance.score = c(0, start.resistance.2[i]),
                                     applied.insecticide.dose = c(dose.1[i], dose.2[i]),
                                     recommended.insecticide.dose = 1,
                                     threshold.generations = threshold.gens[i],
                                     base.efficacy.decay.rate = c(base.decay.1[i], base.decay.2[i]),
                                     rapid.decay.rate = 0.08,
                                     deployment.interval.llin = NA, #only for combinations
                                     deployment.interval.irs = NA, #only for combinations
                                     probability.only.i.male = NA, #only for combinations
                                     probability.only.j.male = NA, #only for combinations
                                     probability.both.i.j.male = NA, #only for combinations
                                     probability.only.i.female = NA, #only for combinations
                                     probability.only.j.female = NA, #only for combinations
                                     probability.both.i.j.female = NA, #only for combinations
                                     n.cycles = 1,
                                     intervention.coverage.1 = NA,
                                     intervention.coverage.2 = NA,
                                     intervention.coverage.1.2 = NA,
                                     z.sd.coefficient = 0.4,
                                     z.sd.intercept = 18,
                                     llin.insecticides = NA,
                                     irs.insecticides = NA,
                                     min.cross.selection = 0,
                                     max.cross.selection = 0)



    sim.df.novel = sim.df%>%
      dplyr::filter(site == "intervention")%>%
      dplyr::filter(insecticide.tracked == 1)


    sim.df.pyrethroid= sim.df%>%
      dplyr::filter(site == "intervention")%>%
      dplyr::filter(insecticide.tracked == 2)

    # outcomes:
    #1. Change resistance "novel"

    mixture.novel[i] = max(sim.df.novel$resistance.score)

    #2. Change resistance "pyrethroid"

    mixture.pyrethroid[i] = max(sim.df.pyrethroid$resistance.score)

    print(c(i, j))

  }


  ##Now to match up the simulations:::
  mixture.df = data.frame(mixture.novel,
                          mixture.pyrethroid,
                          base.decay.1,
                          base.decay.2,
                          threshold.gens,
                          dose.1,
                          dose.2,
                          start.resistance.2)


  novel.solo.df = novel.solo.df%>%
    dplyr::rename(base.decay.1 = "base.decay.novel")%>%
    dplyr::rename(threshold.gens.1 = "threshold.gens.novel")

  pyrethroid.solo.df = pyrethroid.solo.df%>%
    dplyr::rename(base.decay.2 = "base.decay.pyrethroid")%>%
    dplyr::rename(threshold.gens = "threshold.gens.pyrethroid")%>%
    dplyr::rename(start.resistance.2 = "start.pyrethroid.resistance")

  mix.novel.df = dplyr::inner_join(mixture.df, novel.solo.df)
  mix.novel.pyrethroid.df = dplyr::inner_join(mix.novel.df, pyrethroid.solo.df)

  heritability = rep(parameter.space.df$Heritability[j], 432)
  male.exposure = rep(parameter.space.df$Male.Insecticide.Exposure[j], 432)
  female.exposure = rep(parameter.space.df$Female.Insecticide.Exposure[j], 432)
  intervention.coverage = rep(parameter.space.df$Intervention.Coverage[j], 432)
  dispersal = rep(parameter.space.df$Dispersal[j], 432)

  mix.novel.pyrethroid.df = data.frame(mix.novel.pyrethroid.df,
                                       heritability,
                                       male.exposure,
                                       female.exposure,
                                       intervention.coverage,
                                       dispersal)

  mixture.list[[j]] = mix.novel.pyrethroid.df

  print(c(i, j))
}
return(mixture.list)

}


set1 = impact_decay_dosing_resistance_mixtures(parameter.space.df = parameter_space_smooth,
                                                    min_i = 1,
                                                    max_i = 250)
mixtures.smooth.set.1 = do.call(rbind, set1)

write.csv(mixtures.smooth.set.1, "mixtures.smooth.set.1.csv")

set2 = impact_decay_dosing_resistance_mixtures(parameter.space.df = parameter_space_smooth,
                                               min_i = 251,
                                               max_i = 500)
mixtures.smooth.set.2 = do.call(rbind, set2)

write.csv(mixtures.smooth.set.2, "mixtures.smooth.set.2.csv")

set3 = impact_decay_dosing_resistance_mixtures(parameter.space.df = parameter_space_smooth,
                                               min_i = 501,
                                               max_i = 750)
mixtures.smooth.set.3 = do.call(rbind, set3)

write.csv(mixtures.smooth.set.3, "mixtures.smooth.set.3.csv")

set4 = impact_decay_dosing_resistance_mixtures(parameter.space.df = parameter_space_smooth,
                                               min_i = 751,
                                               max_i = 1000)
mixtures.smooth.set.4 = do.call(rbind, set4)

write.csv(mixtures.smooth.set.4, "mixtures.smooth.set.4.csv")

set5 = impact_decay_dosing_resistance_mixtures(parameter.space.df = parameter_space_smooth,
                                               min_i = 1001,
                                               max_i = 1250)
mixtures.smooth.set.5 = do.call(rbind, set5)

write.csv(mixtures.smooth.set.5, "mixtures.smooth.set.5.csv")


set6 = impact_decay_dosing_resistance_mixtures(parameter.space.df = parameter_space_smooth,
                                               min_i = 1251,
                                               max_i = 1500)
mixtures.smooth.set.6 = do.call(rbind, set6)

write.csv(mixtures.smooth.set.6, "mixtures.smooth.set.6.csv")


set7 = impact_decay_dosing_resistance_mixtures(parameter.space.df = parameter_space_smooth,
                                               min_i = 1501,
                                               max_i = 1750)
mixtures.smooth.set.7 = do.call(rbind, set7)

write.csv(mixtures.smooth.set.7, "mixtures.smooth.set.7.csv")


set8 = impact_decay_dosing_resistance_mixtures(parameter.space.df = parameter_space_smooth,
                                               min_i = 1751,
                                               max_i = 2000)
mixtures.smooth.set.8 = do.call(rbind, set8)

write.csv(mixtures.smooth.set.8, "mixtures.smooth.set.8.csv")


set9 = impact_decay_dosing_resistance_mixtures(parameter.space.df = parameter_space_smooth,
                                               min_i = 2001,
                                               max_i = 2250)
mixtures.smooth.set.9 = do.call(rbind, set9)

write.csv(mixtures.smooth.set.9, "mixtures.smooth.set.9.csv")

set10= impact_decay_dosing_resistance_mixtures(parameter.space.df = parameter_space_smooth,
                                               min_i = 2251,
                                               max_i = 2500)
mixtures.smooth.set.10 = do.call(rbind, set10)

write.csv(mixtures.smooth.set.10, "mixtures.smooth.set.10.csv")
