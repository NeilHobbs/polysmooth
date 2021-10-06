#Set 24 Smooth Selection no fitness costs or dispersal
#Identify what would be the suitable standard deviation and exposure scaling factor for calibration.


##Model Calibration for Smooth Selection

#Still need to have it such that the "average" insecticide lasts 10 years with continual use.
#Issue is now that the response at each generation time point is dependent on both the current
#mean value of the polygenic resistance score and the standard deviation of the Normal distribution.


parameter.space = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.3.csv")
standard.deviation = c(rep(15, 5000), rep(20, 5000), rep(25, 5000), rep(30, 5000), rep(35, 5000), rep(40, 5000))

parameter.space.df = rbind(parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space)

parameter.space.df$standard.deviation = standard.deviation

sequence.list = list()

for(v in 1:nrow(parameter.space.df)){

A = get_simulation_dataframe(simulation.array = run_simulation_smooth(number.of.insecticides = 1,
                                                                  exposure.scaling.factor = 10,
                                                                  female.fitness.cost = 0,
                                                                  male.fitness.cost = 0,
                                                                  female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                  male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                  heritability = parameter.space.df$Heritability[v],
                                                                  dispersal.rate = 0,
                                                                  intervention.coverage = 1,
                                                                  standard.deviation = parameter.space.df$standard.deviation[v],
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
                                                                  deployment.frequency = 2, #minimum deployment frequency
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



sequence.list[[v]] = simulation.duration

}

sim.duration.beta.10 = do.call(rbind, sequence.list)

df = cbind(parameter.space.df, sim.duration.beta.10)



ggplot(df, aes(x=sim.duration))+
  geom_histogram()+
  facet_wrap(~standard.deviation)


plot(df$standard.deviation,
     df$exposure.scaling.factor)


sequence.list.2 = list()
for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe(simulation.array = run_simulation_smooth(number.of.insecticides = 1,
                                                                        exposure.scaling.factor = 20,
                                                                        female.fitness.cost = 0,
                                                                        male.fitness.cost = 0,
                                                                        female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                        male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                        heritability = parameter.space.df$Heritability[v],
                                                                        dispersal.rate = 0,
                                                                        intervention.coverage = 1,
                                                                        standard.deviation = parameter.space.df$standard.deviation[v],
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
                                                                        deployment.frequency = 2, #minimum deployment frequency
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



  sequence.list.2[[v]] = simulation.duration

}

sim.duration.beta.20 = do.call(rbind, sequence.list.2)

df$sim.duration.beta.20 = sim.duration.beta.20

ggplot(df, aes(x=sim.duration.beta.20))+
  geom_histogram()+
  facet_wrap(~standard.deviation)



write.csv(df, ".//sequence.set.24.csv")

sequence.list.3 = list()
for(v in 1:nrow(parameter.space.df)){

  A = get_simulation_dataframe(simulation.array = run_simulation_smooth(number.of.insecticides = 1,
                                                                        exposure.scaling.factor = 30,
                                                                        female.fitness.cost = 0,
                                                                        male.fitness.cost = 0,
                                                                        female.insecticide.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                                                                        male.insecticide.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                                                                        heritability = parameter.space.df$Heritability[v],
                                                                        dispersal.rate = 0,
                                                                        intervention.coverage = 1,
                                                                        standard.deviation = parameter.space.df$standard.deviation[v],
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
                                                                        deployment.frequency = 2, #minimum deployment frequency
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



  sequence.list.3[[v]] = simulation.duration

}
