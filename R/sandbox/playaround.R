library(devtools)
load_all()
library(ggplot2)



compare_micro_mosaics = function(number.of.insecticides = 2,
                                 maximum.generations = 200,
                                 starting.intervention.resistance.score =0,
                                 applied.insecticide.dose = 1,
                                 recommended.insecticide.dose = 1,
                                 threshold.generations = 15,
                                 base.efficacy.decay.rate = 0,
                                 rapid.decay.rate =0,
                                 deployment.interval = 20,
                                 max.cycles = 10,
                                 standard.deviation = 30,
                                 vector.length = 1000,
                                 female.insecticide.exposure = 0.4,
                                 male.insecticide.exposure = 0.5,
                                 heritability = 0.3,
                                 regression.coefficient = 0.48,
                                 regression.intercept = 0.15,
                                 exposure.scaling.factor = 20,
                                 male.fitness.cost = 0,
                                 female.fitness.cost=0,
                                 half.population.bioassay.survival.resistance = 900,
                                 michaelis.menten.slope = 1,
                                 maximum.bioassay.survival.proportion = 1,
                                 cross.selection = 0,
                                 female.natural.survival.probability = 0.95,
                                 male.natural.survival.probability = 0.95){



intervention.coverage.1 = seq(1, 0, by = -0.1)
intervention.coverage.2 = seq(0, 1, by = 0.1)


micro.mosaic.sim.list = list()
for(i in 1:length(intervention.coverage.1)){
  A = run_simulation_micromosaic_smooth(number.of.insecticides = number.of.insecticides,
                                  maximum.generations = maximum.generations,
                                  starting.intervention.resistance.score =starting.intervention.resistance.score,
                                  applied.insecticide.dose = applied.insecticide.dose,
                                  recommended.insecticide.dose = recommended.insecticide.dose,
                                  threshold.generations = threshold.generations,
                                  base.efficacy.decay.rate = base.efficacy.decay.rate,
                                  rapid.decay.rate =rapid.decay.rate,
                                  deployment.interval = deployment.interval,
                                  max.cycles = max.cycles,
                                  intervention.coverage.1 = intervention.coverage.1[i],
                                  intervention.coverage.2 = intervention.coverage.2[i],
                                  standard.deviation = standard.deviation,
                                  vector.length = vector.length,
                                  female.insecticide.exposure = female.insecticide.exposure,
                                  male.insecticide.exposure = male.insecticide.exposure,
                                  heritability = heritability,
                                  regression.coefficient = regression.coefficient,
                                  regression.intercept = regression.intercept,
                                  exposure.scaling.factor = exposure.scaling.factor,
                                  male.fitness.cost = male.fitness.cost,
                                  half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                  michaelis.menten.slope = michaelis.menten.slope,
                                  maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                  cross.selection = cross.selection,
                                  male.natural.survival.probability = male.natural.survival.probability,
                                  female.natural.survival.probability = female.natural.survival.probability)

B = get_simulation_dataframe(simulation.array = A,
                             maximum.generations = maximum.generations,
                             number.of.insecticides = 2)
B$coverage.1 = intervention.coverage.1[i]
B$coverage.2 = intervention.coverage.2[i]

B.1= B%>%
  dplyr::filter(site == "intervention")%>%
  dplyr::filter(time.in.generations == maximum.generations)%>%
  dplyr::filter(insecticide.tracked == 1)

B.2 = B%>%
  dplyr::filter(site == "intervention")%>%
  dplyr::filter(time.in.generations == maximum.generations)%>%
  dplyr::filter(insecticide.tracked == 2)

B.3 = rbind(B.1, B.2)

micro.mosaic.sim.list[[i]] = B.3
}


df.micro = do.call(rbind, micro.mosaic.sim.list)


df.micro.1 = df.micro%>%
  dplyr::filter(insecticide.tracked == 1)

df.micro.2 = df.micro%>%
  dplyr::filter(insecticide.tracked == 2)


df.micro.1$total.resistance = df.micro.1$resistance.score + df.micro.2$resistance.score
df.micro.1$average.resistance = (df.micro.1$resistance.score + df.micro.2$resistance.score)/2


mixture.sim.full = run_simulation_micromosaic_mixtures_smooth(number.of.insecticides = number.of.insecticides,
                                                              maximum.generations = maximum.generations,
                                                              starting.intervention.resistance.score =starting.intervention.resistance.score,
                                                              applied.insecticide.dose = 1,
                                                              recommended.insecticide.dose = 1,
                                                              threshold.generations = threshold.generations,
                                                              base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                              rapid.decay.rate =rapid.decay.rate,
                                                              deployment.interval = deployment.interval,
                                                              max.cycles = max.cycles,
                                                              intervention.coverage = 1,
                                                              standard.deviation = standard.deviation,
                                                              vector.length = vector.length,
                                                              female.insecticide.exposure = female.insecticide.exposure,
                                                              male.insecticide.exposure = male.insecticide.exposure,
                                                              heritability = heritability,
                                                              regression.coefficient = regression.coefficient,
                                                              regression.intercept = regression.intercept,
                                                              exposure.scaling.factor = exposure.scaling.factor,
                                                              male.fitness.cost = male.fitness.cost,
                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                              cross.selection = cross.selection)[[1]]

mixture.sim.half = run_simulation_micromosaic_mixtures_smooth(number.of.insecticides = number.of.insecticides,
                                                              maximum.generations = maximum.generations,
                                                              starting.intervention.resistance.score =starting.intervention.resistance.score,
                                                              applied.insecticide.dose = 0.5,
                                                              recommended.insecticide.dose = 1,
                                                              threshold.generations = threshold.generations,
                                                              base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                              rapid.decay.rate =rapid.decay.rate,
                                                              deployment.interval = deployment.interval,
                                                              max.cycles = max.cycles,
                                                              intervention.coverage = 1,
                                                              standard.deviation = standard.deviation,
                                                              vector.length = vector.length,
                                                              female.insecticide.exposure = female.insecticide.exposure,
                                                              male.insecticide.exposure = male.insecticide.exposure,
                                                              heritability = heritability,
                                                              regression.coefficient = regression.coefficient,
                                                              regression.intercept = regression.intercept,
                                                              exposure.scaling.factor = exposure.scaling.factor,
                                                              male.fitness.cost = male.fitness.cost,
                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                              cross.selection = cross.selection)[[1]]

mixture.sim.threequarters = run_simulation_micromosaic_mixtures_smooth(number.of.insecticides = number.of.insecticides,
                                                              maximum.generations = maximum.generations,
                                                              starting.intervention.resistance.score =starting.intervention.resistance.score,
                                                              applied.insecticide.dose = 0.75,
                                                              recommended.insecticide.dose = 1,
                                                              threshold.generations = threshold.generations,
                                                              base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                              rapid.decay.rate =rapid.decay.rate,
                                                              deployment.interval = deployment.interval,
                                                              max.cycles = max.cycles,
                                                              intervention.coverage = 1,
                                                              standard.deviation = standard.deviation,
                                                              vector.length = vector.length,
                                                              female.insecticide.exposure = female.insecticide.exposure,
                                                              male.insecticide.exposure = male.insecticide.exposure,
                                                              heritability = heritability,
                                                              regression.coefficient = regression.coefficient,
                                                              regression.intercept = regression.intercept,
                                                              exposure.scaling.factor = exposure.scaling.factor,
                                                              male.fitness.cost = male.fitness.cost,
                                                              half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                              michaelis.menten.slope = michaelis.menten.slope,
                                                              maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                              cross.selection = cross.selection)[[1]]

full.1 = mixture.sim.full["intervention", 1, maximum.generations]
full.2 = mixture.sim.full["intervention", 2, maximum.generations]
half.1 = mixture.sim.half["intervention", 1, maximum.generations]
half.2 = mixture.sim.half["intervention", 2, maximum.generations]
threequarters.1 = mixture.sim.threequarters["intervention", 1, maximum.generations]
threequarters.2 = mixture.sim.threequarters["intervention", 2, maximum.generations]


rotation.sim = run_simulation_smooth_multiple_gonotrophic_cycles(number.of.insecticides = number.of.insecticides,
                                                                 exposure.scaling.factor = exposure.scaling.factor,
                                                                 female.fitness.cost = female.fitness.cost,
                                                                 male.fitness.cost = male.fitness.cost,
                                                                 female.insecticide.exposure = female.insecticide.exposure,
                                                                 male.insecticide.exposure = male.insecticide.exposure,
                                                                 heritability = heritability,
                                                                 dispersal.rate = 0,
                                                                 intervention.coverage = 1,
                                                                 standard.deviation = standard.deviation,
                                                                 vector.length =vector.length,
                                                                 maximum.bioassay.survival.proportion =maximum.bioassay.survival.proportion,
                                                                 michaelis.menten.slope =michaelis.menten.slope,
                                                                 regression.coefficient = regression.coefficient,
                                                                 regression.intercept =regression.intercept,
                                                                 maximum.generations = maximum.generations,
                                                                 irm.strategy = "rotation",
                                                                 half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                 withdrawal.threshold.value = 1,
                                                                 return.threshold.value = 1,
                                                                 deployment.frequency = deployment.interval,
                                                                 maximum.resistance.value = 10000,
                                                                 starting.refugia.resistance.score = 0,
                                                                 starting.intervention.resistance.score = starting.intervention.resistance.score,
                                                                 applied.insecticide.dose = 1,
                                                                 recommended.insecticide.dose = 1,
                                                                 threshold.generations = threshold.generations,
                                                                 base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                                 rapid.decay.rate = rapid.decay.rate,
                                                                 min.cross.selection = cross.selection,
                                                                 max.cross.selection = cross.selection,
                                                                 max.cycles = max.cycles,
                                                                 female.natural.survival.probability = female.natural.survival.probability,
                                                                 male.natural.survival.probability = male.natural.survival.probability)[[1]]

rotation.1 = rotation.sim["intervention", 1, maximum.generations]
rotation.2 = rotation.sim["intervention", 2, maximum.generations]


the.plot = ggplot(df.micro.1, aes(x=coverage.1, y = resistance.score))+
  geom_line(size = 3, alpha = 0.7,
            colour = "red")+
  geom_line(data = df.micro.2, aes(x=coverage.1, y=resistance.score),
            colour = "blue", size = 3, alpha = 0.7)+
  geom_hline(yintercept = full.1, colour = "orange4", size = 3)+
  geom_hline(yintercept = full.2, colour = "orange", size = 1.5)+
  geom_hline(yintercept = half.1, colour = "seagreen", size = 3)+
  geom_hline(yintercept = half.2, colour = "seagreen1", size = 1.5)+
  geom_hline(yintercept = threequarters.1, colour = "#3f007d", size = 3)+
  geom_hline(yintercept = threequarters.2, colour = "#807dba", size = 1.5)+
  geom_hline(yintercept = rotation.1, colour = "red",
             linetype = "dashed", size = 1.5)+
  geom_hline(yintercept = rotation.2, colour = "blue",
             linetype = "dashed", size = 1.5)+
  geom_vline(xintercept = 0.4, linetype = "dashed",
             size = 1.5, colour = "grey", alpha = 0.5)+
  geom_vline(xintercept = 0.6, linetype = "dashed",
             size = 1.5, colour = "grey", alpha = 0.5)+
  ylim(0, max(c(df.micro.1$resistance.score,
                df.micro.2$resistance.score)))+
  ylab(paste0("Polygenic Resistance Score After ", maximum.generations, " Generations"))+
  xlab("Proportion Coverage Insecticide 1")+
  theme_bw()

return(the.plot)
}




compare_micro_mosaics(number.of.insecticides = 2,
                      maximum.generations = 200,
                      starting.intervention.resistance.score = 0,
                      applied.insecticide.dose = 1,
                      recommended.insecticide.dose = 1,
                      threshold.generations = 15,
                      base.efficacy.decay.rate = 0.016,
                      rapid.decay.rate = 0.08,
                      deployment.interval = 20,
                      max.cycles = 10,
                      standard.deviation = 50,
                      vector.length = 1000,
                      female.insecticide.exposure = 0.1,
                      male.insecticide.exposure = 0,
                      heritability =  0.3,
                      regression.coefficient = 0.48,
                      regression.intercept = 0.15,
                      exposure.scaling.factor = 10,
                      male.fitness.cost = 0,
                      female.fitness.cost=0,
                      half.population.bioassay.survival.resistance = 900,
                      michaelis.menten.slope = 1,
                      maximum.bioassay.survival.proportion = 1,
                      cross.selection = 0,
                      male.natural.survival.probability = 1,
                      female.natural.survival.probability = 1)


compare_micro_mosaics(number.of.insecticides = 2,
                      maximum.generations = 200,
                      starting.intervention.resistance.score = c(0, 50),
                      applied.insecticide.dose = 1,
                      recommended.insecticide.dose = 1,
                      threshold.generations = c(15, 10),
                      base.efficacy.decay.rate = c(0.016, 0.018),
                      rapid.decay.rate = c(0.08, 0.06),
                      deployment.interval = 20,
                      max.cycles = 10,
                      standard.deviation = 30,
                      vector.length = 1000,
                      female.insecticide.exposure = 0.7,
                      male.insecticide.exposure = 0.5,
                      heritability =  c(0.3, 0.2),
                      regression.coefficient = 0.48,
                      regression.intercept = 0.15,
                      exposure.scaling.factor = 20,
                      male.fitness.cost = 0,
                      female.fitness.cost=0,
                      half.population.bioassay.survival.resistance = 900,
                      michaelis.menten.slope = 1,
                      maximum.bioassay.survival.proportion = 1,
                      cross.selection = 0.2,
                      male.natural.survival.probability = 1,
                      female.natural.survival.probability = 1)

#What if the two insecticides have different heritabilities etc

intervention.coverage.1 = seq(1, 0, by = -0.1)
intervention.coverage.2 = seq(0, 1, by = 0.1)

micro.mosaic.sim.list = list()
for(i in 1:length(intervention.coverage.1)){
  A = run_simulation_micromosaic_smooth(number.of.insecticides = 2,
                                        maximum.generations = 200,
                                        starting.intervention.resistance.score =0,
                                        applied.insecticide.dose = 1,
                                        recommended.insecticide.dose = 1,
                                        threshold.generations = 15,
                                        base.efficacy.decay.rate = 0,
                                        rapid.decay.rate =0,
                                        deployment.interval = 20,
                                        max.cycles = 10,
                                        intervention.coverage.1 = intervention.coverage.1[i],
                                        intervention.coverage.2 = intervention.coverage.2[i],
                                        standard.deviation = 30,
                                        vector.length = 1000,
                                        female.insecticide.exposure = 0.7,
                                        male.insecticide.exposure = 0.5,
                                        heritability = c(0.3, 0.5),
                                        regression.coefficient = 0.48,
                                        regression.intercept = 0.15,
                                        exposure.scaling.factor = 10,
                                        male.fitness.cost = 0,
                                        half.population.bioassay.survival.resistance = 900,
                                        michaelis.menten.slope = 1,
                                        maximum.bioassay.survival.proportion = 1,
                                        cross.selection = 0)

  B = get_simulation_dataframe(simulation.array = A,
                               maximum.generations = 200,
                               number.of.insecticides = 2)

  B$coverage.1 = intervention.coverage.1[i]
  B$coverage.2 = intervention.coverage.2[i]
  B$female.exposure = 0.7

  B.1= B%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(insecticide.tracked == 1)

  B.2 = B%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(insecticide.tracked == 2)

  B.3 = rbind(B.1, B.2)

  micro.mosaic.sim.list[[i]] = B.3
}

df = do.call(rbind, micro.mosaic.sim.list)


ggplot(df, aes(x=time.in.generations,
               y=resistance.score,
               colour = as.character(insecticide.tracked)))+
  geom_line()+
  facet_wrap(~coverage.1)

##################

intervention.coverage.1 = seq(1, 0, by = -0.1)
intervention.coverage.2 = seq(0, 1, by = 0.1)


micro.mosaic.sim.list = list()
for(i in 1:length(intervention.coverage.1)){
  A = run_simulation_micromosaic_smooth(number.of.insecticides = 2,
                                        maximum.generations = 200,
                                        starting.intervention.resistance.score =0,
                                        applied.insecticide.dose = 1,
                                        recommended.insecticide.dose = 1,
                                        threshold.generations = 15,
                                        base.efficacy.decay.rate = 0,
                                        rapid.decay.rate =0,
                                        deployment.interval = 20,
                                        max.cycles = 1,
                                        intervention.coverage.1 = intervention.coverage.1[i],
                                        intervention.coverage.2 = intervention.coverage.2[i],
                                        standard.deviation = 30,
                                        vector.length = 1000,
                                        female.insecticide.exposure = 0.7,
                                        male.insecticide.exposure = 0.5,
                                        heritability = c(0.3, 0.2),
                                        regression.coefficient = 0.48,
                                        regression.intercept = 0.15,
                                        exposure.scaling.factor = 20,
                                        male.fitness.cost = 0,
                                        half.population.bioassay.survival.resistance = 900,
                                        michaelis.menten.slope = 1,
                                        maximum.bioassay.survival.proportion = 1,
                                        cross.selection = 0)

  B = get_simulation_dataframe(simulation.array = A,
                               maximum.generations = 200,
                               number.of.insecticides = 2)
  B$coverage.1 = intervention.coverage.1[i]
  B$coverage.2 = intervention.coverage.2[i]
  B$female.exposure = 0.7

  B.1= B%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(time.in.generations == 200)%>%
    dplyr::filter(insecticide.tracked == 1)

  B.2 = B%>%
    dplyr::filter(site == "intervention")%>%
    dplyr::filter(time.in.generations == 200)%>%
    dplyr::filter(insecticide.tracked == 2)

  B.3 = rbind(B.1, B.2)

  micro.mosaic.sim.list[[i]] = B.3
}


df.micro = do.call(rbind, micro.mosaic.sim.list)


df.micro.1 = df.micro%>%
  dplyr::filter(insecticide.tracked == 1)

df.micro.2 = df.micro%>%
  dplyr::filter(insecticide.tracked == 2)


df.micro.1$total.resistance = df.micro.1$resistance.score + df.micro.2$resistance.score
df.micro.1$average.resistance = (df.micro.1$resistance.score + df.micro.2$resistance.score)/2


mixture.sim.full = run_simulation_micromosaic_mixtures_smooth(number.of.insecticides = 2,
                                                              maximum.generations = 200,
                                                              starting.intervention.resistance.score =0,
                                                              applied.insecticide.dose = 1,
                                                              recommended.insecticide.dose = 1,
                                                              threshold.generations = 15,
                                                              base.efficacy.decay.rate = 0,
                                                              rapid.decay.rate =0,
                                                              deployment.interval = 20,
                                                              max.cycles = 1,
                                                              intervention.coverage = 1,
                                                              standard.deviation = 30,
                                                              vector.length = 1000,
                                                              female.insecticide.exposure = 0.7,
                                                              male.insecticide.exposure = 0.5,
                                                              heritability = c(0.3, 0.2),
                                                              regression.coefficient = 0.48,
                                                              regression.intercept = 0.15,
                                                              exposure.scaling.factor = 20,
                                                              male.fitness.cost = 0,
                                                              half.population.bioassay.survival.resistance = 900,
                                                              michaelis.menten.slope = 1,
                                                              maximum.bioassay.survival.proportion = 1,
                                                              cross.selection = 0)[[1]]

mixture.sim.half = run_simulation_micromosaic_mixtures_smooth(number.of.insecticides = 2,
                                                              maximum.generations = 200,
                                                              starting.intervention.resistance.score =0,
                                                              applied.insecticide.dose = 0.5,
                                                              recommended.insecticide.dose = 1,
                                                              threshold.generations = 15,
                                                              base.efficacy.decay.rate = 0,
                                                              rapid.decay.rate =0,
                                                              deployment.interval = 20,
                                                              max.cycles = 1,
                                                              intervention.coverage = 1,
                                                              standard.deviation = 30,
                                                              vector.length = 1000,
                                                              female.insecticide.exposure = 0.7,
                                                              male.insecticide.exposure = 0.5,
                                                              heritability = c(0.3, 0.2),
                                                              regression.coefficient = 0.48,
                                                              regression.intercept = 0.15,
                                                              exposure.scaling.factor = 20,
                                                              male.fitness.cost = 0,
                                                              half.population.bioassay.survival.resistance = 900,
                                                              michaelis.menten.slope = 1,
                                                              maximum.bioassay.survival.proportion = 1,
                                                              cross.selection = 0)[[1]]

full.1 = mixture.sim.full["intervention", 1, 200]
full.2 = mixture.sim.full["intervention", 2, 200]
half.1 = mixture.sim.half["intervention", 1, 200]
half.2 = mixture.sim.half["intervention", 2, 200]


ggplot(df.micro.1, aes(x=coverage.1, y = resistance.score))+
  geom_line(size = 3, alpha = 0.7,
             colour = "red")+
  geom_line(data = df.micro.2, aes(x=coverage.1, y=resistance.score),
             colour = "blue", size = 3, alpha = 0.7)+
  geom_hline(yintercept = full.1, colour = "orange4", size = 3)+
  geom_hline(yintercept = full.2, colour = "orange", size = 1.5)+
  geom_hline(yintercept = half.1, colour = "seagreen", size = 3)+
  geom_hline(yintercept = half.2, colour = "seagreen1", size = 1.5)+
  geom_vline(xintercept = 0.4, linetype = "dashed",
             size = 1.5, colour = "grey", alpha = 0.5)+
  geom_vline(xintercept = 0.6, linetype = "dashed",
             size = 1.5, colour = "grey", alpha = 0.5)+
  ylab("Polygenic Resistance Score")+
  xlab("Proportion Coverage Insecticide 1")+
  theme_bw()




