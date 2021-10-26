library(devtools)
load_all()
library(ggplot2)


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
                                  max.cycles = 5,
                                  intervention.coverage.1 = intervention.coverage.1[i],
                                  intervention.coverage.2 = intervention.coverage.2[i],
                                  standard.deviation = 30,
                                  vector.length = 1000,
                                  female.insecticide.exposure = 0.7,
                                  male.insecticide.exposure = 0.5,
                                  heritability = 0.3,
                                  regression.coefficient = 0.48,
                                  regression.intercept = 0.15,
                                  exposure.scaling.factor = 10,
                                  male.fitness.cost = 0,
                                  half.population.bioassay.survival.resistance = 900,
                                  michaelis.menten.slope = 1,
                                  maximum.bioassay.survival.proportion = 1,
                                  cross.selection = -0.5)

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
                                                              max.cycles = 5,
                                                              intervention.coverage = 1,
                                                              standard.deviation = 30,
                                                              vector.length = 1000,
                                                              female.insecticide.exposure = 0.7,
                                                              male.insecticide.exposure = 0.5,
                                                              heritability = 0.3,
                                                              regression.coefficient = 0.48,
                                                              regression.intercept = 0.15,
                                                              exposure.scaling.factor = 10,
                                                              male.fitness.cost = 0,
                                                              half.population.bioassay.survival.resistance = 900,
                                                              michaelis.menten.slope = 1,
                                                              maximum.bioassay.survival.proportion = 1,
                                                              cross.selection = -0.5)[[1]]

mixture.sim.half = run_simulation_micromosaic_mixtures_smooth(number.of.insecticides = 2,
                                                              maximum.generations = 200,
                                                              starting.intervention.resistance.score =0,
                                                              applied.insecticide.dose = 0.5,
                                                              recommended.insecticide.dose = 1,
                                                              threshold.generations = 15,
                                                              base.efficacy.decay.rate = 0,
                                                              rapid.decay.rate =0,
                                                              deployment.interval = 20,
                                                              max.cycles = 5,
                                                              intervention.coverage = 1,
                                                              standard.deviation = 30,
                                                              vector.length = 1000,
                                                              female.insecticide.exposure = 0.7,
                                                              male.insecticide.exposure = 0.5,
                                                              heritability = 0.3,
                                                              regression.coefficient = 0.48,
                                                              regression.intercept = 0.15,
                                                              exposure.scaling.factor = 10,
                                                              male.fitness.cost = 0,
                                                              half.population.bioassay.survival.resistance = 900,
                                                              michaelis.menten.slope = 1,
                                                              maximum.bioassay.survival.proportion = 1,
                                                              cross.selection = -0.5)[[1]]

full.1 = mixture.sim.full["intervention", 1, 200]
full.2 = mixture.sim.full["intervention", 2, 200]
half.1 = mixture.sim.half["intervention", 1, 200]
half.2 = mixture.sim.half["intervention", 2, 200]


ggplot(df.micro.1, aes(x=coverage.1, y = resistance.score))+
  geom_point(size = 3, alpha = 0.7,
             colour = "red")+
  geom_point(data = df.micro.2, aes(x=coverage.1, y=resistance.score),
             colour = "blue", size = 3, alpha = 0.7)+
  geom_hline(yintercept = full.1, colour = "orange4", size = 3)+
  geom_hline(yintercept = full.1, colour = "orange", size = 1.5)+
  geom_hline(yintercept = half.1, colour = "seagreen", size = 3)+
  geom_hline(yintercept = half.2, colour = "seagreen1", size = 1.5)+
  geom_vline(xintercept = 0.4, linetype = "dashed",
             size = 1.5, colour = "grey", alpha = 0.5)+
  geom_vline(xintercept = 0.6, linetype = "dashed",
             size = 1.5, colour = "grey", alpha = 0.5)+
  ylab("Polygenic Resistance Score")+
  xlab("Proportion Coverage Insecticide 1")+
  theme_bw()


