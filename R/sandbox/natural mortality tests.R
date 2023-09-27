###How to Set up Simulations for Combinations...


gonotrophic.cycles = c(rep(1, 24), rep(3, 24), rep(5, 24))
daily.survival = rep(rep(c(0.5, 0.6, 0.7, 0.8, 0.9, 1), 4), 3)
gonotrophic.length = rep(c(rep(2, 6), rep(3, 6), rep(4, 6), rep(5, 6)), 3)



sim.list.test.mortality = list()
for(i in 1:72){

  sim.list.test.mortality[[i]] = run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                          irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                                          number.of.insecticides = 2,
                                          sd.scaled = FALSE, ##TRUE or FALSE
                                          exposure.scaling.factor = 10,
                                          female.fitness.cost = 0,
                                          male.fitness.cost = 0,
                                          female.exposure = 0.9,
                                          male.exposure = 0.1,
                                          heritability = 0.3,
                                          dispersal.rate = 0.3,
                                          coverage = 0.9,
                                          standard.deviation = 50,
                                          vector.length = 1000,
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
                                          threshold.generations = 10,
                                          base.efficacy.decay.rate = 0,
                                          rapid.decay.rate = 0,
                                          deployment.interval.llin = 100, #only for combinations
                                          deployment.interval.irs = 100, #only for combinations
                                          probability.only.i.male = 0.5, #only for combinations
                                          probability.only.j.male = 0.25, #only for combinations
                                          probability.both.i.j.male = 0.5, #only for combinations
                                          probability.only.i.female = 0.25, #only for combinations
                                          probability.only.j.female =  0.25, #only for combinations
                                          probability.both.i.j.female = 0.5, #only for combinations
                                          n.cycles = gonotrophic.cycles[i],
                                          intervention.coverage.1 = 0,
                                          intervention.coverage.2 = 0.25,
                                          intervention.coverage.1.2 = 0.75,
                                          z.sd.intercept = 18,
                                          z.sd.coefficient = 0.4,
                                          mixture.strategy = "pyrethroid.plus",
                                          llin.insecticides = 1,
                                          irs.insecticides = 2,
                                          min.cross.selection = 0,
                                          max.cross.selection = 0,
                                          gonotrophic.cycle.length = gonotrophic.length[i],
                                          natural.daily.survival = daily.survival[i])

  print(i)
}


peak.llin = c()
peak.irs = c()
for(i in 1:72){

  peak.llin[i] = max(subset(sim.list.test.mortality[[i]], insecticide.tracked == 1)$bioassay.survival)
  peak.irs[i] = max(subset(sim.list.test.mortality[[i]], insecticide.tracked == 2)$bioassay.survival)



}


sim.list.test.mortality = list()
for(i in 1:72){

  sim.list.test.mortality[[i]] = run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                          irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                                          number.of.insecticides = 2,
                                          sd.scaled = FALSE, ##TRUE or FALSE
                                          exposure.scaling.factor = 10,
                                          female.fitness.cost = 0,
                                          male.fitness.cost = 0,
                                          female.exposure = 0.9,
                                          male.exposure = 0.1,
                                          heritability = 0.3,
                                          dispersal.rate = 0.3,
                                          coverage = 0.9,
                                          standard.deviation = 50,
                                          vector.length = 1000,
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
                                          applied.insecticide.dose = c(1, 0),
                                          recommended.insecticide.dose = 1,
                                          threshold.generations = 10,
                                          base.efficacy.decay.rate = 0,
                                          rapid.decay.rate = 0,
                                          deployment.interval.llin = 100, #only for combinations
                                          deployment.interval.irs = 100, #only for combinations
                                          probability.only.i.male = 1, #only for combinations
                                          probability.only.j.male = 0, #only for combinations
                                          probability.both.i.j.male = 0, #only for combinations
                                          probability.only.i.female = 1, #only for combinations
                                          probability.only.j.female =  0, #only for combinations
                                          probability.both.i.j.female = 0, #only for combinations
                                          n.cycles = gonotrophic.cycles[i],
                                          intervention.coverage.1 = 0,
                                          intervention.coverage.2 = 0.25,
                                          intervention.coverage.1.2 = 0.75,
                                          z.sd.intercept = 18,
                                          z.sd.coefficient = 0.4,
                                          mixture.strategy = "pyrethroid.plus",
                                          llin.insecticides = 1,
                                          irs.insecticides = 2,
                                          min.cross.selection = 0,
                                          max.cross.selection = 0,
                                          gonotrophic.cycle.length = gonotrophic.length[i],
                                          natural.daily.survival = daily.survival[i])

  print(i)
}

solo.peak.llin = c()
solo.peak.irs = c()
for(i in 1:72){

  solo.peak.llin[i] = max(subset(sim.list.test.mortality[[i]], insecticide.tracked == 1)$bioassay.survival)
  solo.peak.irs[i] = max(subset(sim.list.test.mortality[[i]], insecticide.tracked == 2)$bioassay.survival)



}


df.1 = data.frame(solo.peak.llin, peak.llin, peak.irs, gonotrophic.cycles, gonotrophic.length, daily.survival)
df.1$llin.percentage.change = (((peak.llin- solo.peak.llin)/solo.peak.llin)*100)

range(df.1$llin.percentage.change)

ggplot(df.1, aes(x=daily.survival,
                 y=gonotrophic.length,
                 fill = llin.percentage.change))+
  geom_tile(colour = "black")+
  facet_grid(.~gonotrophic.cycles)+
  xlab("Daily Survival Probability")+
  ylab("Gonotrophic Cycle Length (days)")+
  theme_bw()+
  guides(fill=guide_legend(title="Percentage Change"))+
  theme(legend.position = "bottom")




