#Comparing sequences, rotations and mixtures under the smooth selection paradigm.

library(devtools)
load_all()

parameter.space.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polysmooth/Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv")


solo.list.novel = list()

for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe(simulation.array = run_simulation_smooth(number.of.insecticides = 1,
                                                                            exposure.scaling.factor = 10,
                                                                            female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
                                                                            male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
                                                                            female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                            male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                            heritability = parameter.space.df$Heritability[v],
                                                                            dispersal.rate = parameter.space.df$Dispersal[v],
                                                                            intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                            standard.deviation = 50,
                                                                            vector.length = 1000,
                                                                            maximum.bioassay.survival.proportion = 1,
                                                                            michaelis.menten.slope = 1,
                                                                            regression.coefficient = 0.48,
                                                                            regression.intercept = 0.15,
                                                                            maximum.generations = 500,
                                                                            irm.strategy = "sequence",
                                                                            half.population.bioassay.survival.resistance = 900,
                                                                            withdrawal.threshold.value = 0.1,
                                                                            return.threshold.value = 0.08,
                                                                            deployment.frequency = 10, #minimum deployment frequency
                                                                            maximum.resistance.value = 25000,
                                                                            starting.refugia.resistance.score = 0,
                                                                            starting.intervention.resistance.score = 0,
                                                                            applied.insecticide.dose = 1,
                                                                            recommended.insecticide.dose = 1,
                                                                            threshold.generations = 5,#no decay, so this value does not matter
                                                                            base.efficacy.decay.rate = 0,
                                                                            rapid.decay.rate = 0,
                                                                            population.suppression = FALSE,
                                                                            min.cross.selection = 0,
                                                                            max.cross.selection = 0,
                                                                            deployment.type = "singles",
                                                                            mixture.strategy = NA),
                               maximum.generations = 500, number.of.insecticides = 1)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)



  solo.list.novel[[v]] = simulation.duration

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, solo.list.novel)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//solo.novel.smooth.csv")



#Now do as mixture with pre-resistance:::
start.resistance.old = c(rep(0, 5000),
                         rep(18, 5000),
                         rep(47, 5000),
                         rep(100, 5000),
                         rep(225, 5000),
                         rep(900, 5000),
                         rep(3600, 5000),
                         rep(8100, 5000))

parameter.space.df = rbind(parameter.space.df, parameter.space.df,
                           parameter.space.df, parameter.space.df,
                           parameter.space.df, parameter.space.df,
                           parameter.space.df, parameter.space.df)

parameter.space.df$start.resistance.old = start.resistance.old





solo.list.resistant = list()

for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe(simulation.array = run_simulation_smooth(number.of.insecticides = 1,
                                                                            exposure.scaling.factor = 10,
                                                                            female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
                                                                            male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
                                                                            female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                            male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                            heritability = parameter.space.df$Heritability[v],
                                                                            dispersal.rate = parameter.space.df$Dispersal[v],
                                                                            intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                            standard.deviation = 50,
                                                                            vector.length = 1000,
                                                                            maximum.bioassay.survival.proportion = 1,
                                                                            michaelis.menten.slope = 1,
                                                                            regression.coefficient = 0.48,
                                                                            regression.intercept = 0.15,
                                                                            maximum.generations = 500,
                                                                            irm.strategy = "sequence",
                                                                            half.population.bioassay.survival.resistance = 900,
                                                                            withdrawal.threshold.value = 1,
                                                                            return.threshold.value = 0.08,
                                                                            deployment.frequency = 10, #minimum deployment frequency
                                                                            maximum.resistance.value = 25000,
                                                                            starting.refugia.resistance.score = parameter.space.df$start.resistance.old[v],
                                                                            starting.intervention.resistance.score = parameter.space.df$start.resistance.old[v],
                                                                            applied.insecticide.dose = 1,
                                                                            recommended.insecticide.dose = 1,
                                                                            threshold.generations = 5,#no decay, so this value does not matter
                                                                            base.efficacy.decay.rate = 0,
                                                                            rapid.decay.rate = 0,
                                                                            population.suppression = FALSE,
                                                                            min.cross.selection = 0,
                                                                            max.cross.selection = 0,
                                                                            deployment.type = "singles",
                                                                            mixture.strategy = NA),
                               maximum.generations = 500, number.of.insecticides = 1)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)
  mean.resistance = mean(B$resistance.score)
  peak.resistance = max(B$resistance.score)

  solo.list.resistant[[v]] = data.frame(simulation.duration, mean.resistance, peak.resistance)

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, solo.list.resistant)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//solo.resistant.smooth.csv")


mixture.list = list() #need to adapt code to make decision only on insecticide 1.

for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe_mixtures(simulation.array = run_simulation_smooth(number.of.insecticides = 2,
                                                                                     exposure.scaling.factor = 10,
                                                                                     female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
                                                                                     male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
                                                                                     female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                     male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                     heritability = parameter.space.df$Heritability[v],
                                                                                     dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                     intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                     standard.deviation = 50,
                                                                                     vector.length = 1000,
                                                                                     maximum.bioassay.survival.proportion = 1,
                                                                                     michaelis.menten.slope = 1,
                                                                                     regression.coefficient = 0.48,
                                                                                     regression.intercept = 0.15,
                                                                                     maximum.generations = 500,
                                                                                     irm.strategy = "insecticide.1",
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     withdrawal.threshold.value = 0.1,
                                                                                     return.threshold.value = 0.08,
                                                                                     deployment.frequency = 10, #minimum deployment frequency
                                                                                     maximum.resistance.value = 25000,
                                                                                     starting.refugia.resistance.score = c(0, start.resistance.old[v]),
                                                                                     starting.intervention.resistance.score = c(0, start.resistance.old[v]),
                                                                                     applied.insecticide.dose = 1,
                                                                                     recommended.insecticide.dose = 1,
                                                                                     threshold.generations = 5,#no decay, so this value does not matter
                                                                                     base.efficacy.decay.rate = 0,
                                                                                     rapid.decay.rate = 0,
                                                                                     population.suppression = FALSE,
                                                                                     min.cross.selection = 0,
                                                                                     max.cross.selection = 0,
                                                                                     deployment.type = "mixtures",
                                                                                     mixture.strategy = "mix.sequential.discrete"),
                                        maximum.generations = 500, number.of.insecticides = 2)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)

  #insecticide.1
  B.1 = B%>%
    dplyr::filter(insecticide.tracked == 1)
  mean.insecticide.1 = mean(B.1$resistance.intensity)
  peak.insecticide.1 = max(B.1$resistance.intensity)

  #insecticide.2
  B.2 = B%>%
    dplyr::filter(insecticide.tracked == 2)
  mean.insecticide.2 = mean(B.2$resistance.intensity)
  peak.insecticide.2 = max(B.2$resistance.intensity)



  mixture.list[[v]] = data.frame(simulation.duration, mean.insecticide.1, peak.insecticide.1,
                                 mean.insecticide.2, peak.insecticide.2)

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, mixture.list)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//mixture.preresistance.smooth.csv")



#using half dose insecticides

mixture.list.half = list() #need to adapt code to make decision only on insecticide 1.

for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe_mixtures(simulation.array = run_simulation_smooth(number.of.insecticides = 2,
                                                                                 exposure.scaling.factor = 10,
                                                                                 female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
                                                                                 male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
                                                                                 female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                                 male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                                 heritability = parameter.space.df$Heritability[v],
                                                                                 dispersal.rate = parameter.space.df$Dispersal[v],
                                                                                 intervention.coverage = parameter.space.df$Intervention.Coverage[v],
                                                                                 standard.deviation = 50,
                                                                                 vector.length = 1000,
                                                                                 maximum.bioassay.survival.proportion = 1,
                                                                                 michaelis.menten.slope = 1,
                                                                                 regression.coefficient = 0.48,
                                                                                 regression.intercept = 0.15,
                                                                                 maximum.generations = 500,
                                                                                 irm.strategy = "insecticide.1",
                                                                                 half.population.bioassay.survival.resistance = 900,
                                                                                 withdrawal.threshold.value = 0.1,
                                                                                 return.threshold.value = 0.08,
                                                                                 deployment.frequency = 10, #minimum deployment frequency
                                                                                 maximum.resistance.value = 25000,
                                                                                 starting.refugia.resistance.score = c(0, start.resistance.old[v]),
                                                                                 starting.intervention.resistance.score = c(0, start.resistance.old[v]),
                                                                                 applied.insecticide.dose = 0.5,
                                                                                 recommended.insecticide.dose = 1,
                                                                                 threshold.generations = 5,#no decay, so this value does not matter
                                                                                 base.efficacy.decay.rate = 0,
                                                                                 rapid.decay.rate = 0,
                                                                                 population.suppression = FALSE,
                                                                                 min.cross.selection = 0,
                                                                                 max.cross.selection = 0,
                                                                                 deployment.type = "mixtures",
                                                                                 mixture.strategy = "mix.sequential.discrete"),
                                        maximum.generations = 500, number.of.insecticides = 2)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = max(B$time.in.generations)

  #insecticide.1
  B.1 = B%>%
    dplyr::filter(insecticide.tracked == 1)
  mean.insecticide.1 = mean(B.1$resistance.intensity)
  peak.insecticide.1 = max(B.1$resistance.intensity)

  #insecticide.2
  B.2 = B%>%
    dplyr::filter(insecticide.tracked == 2)
  mean.insecticide.2 = mean(B.2$resistance.intensity)
  peak.insecticide.2 = max(B.2$resistance.intensity)



  mixture.list.half[[v]] = data.frame(simulation.duration, mean.insecticide.1, peak.insecticide.1,
                                 mean.insecticide.2, peak.insecticide.2)

  if(v %% 10 == 0){print(v)}
}

sim.duration = do.call(rbind, mixture.list.half)

df = cbind(parameter.space.df, sim.duration)
write.csv(df, ".//mixture.preresistance.smooth.half.csv")


