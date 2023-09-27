parameter.space = read.csv("./Simulation Experiments/Setting Up Simulations/parameter.space.seqrot.for.mixtures.csv")

lifespans.list = list()
for(i in 6617:7000){

  the.sim.1 = run_simulation_advanced(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                      irm.switch.strategy = "novel.sequence", #"rotation", "sequence", "insecticide.1"
                                      number.of.insecticides = 2,
                                      sd.scaled = TRUE, ##TRUE or FALSE
                                      exposure.scaling.factor = 10,
                                      female.fitness.cost = 0,
                                      male.fitness.cost = 0,
                                      female.exposure = parameter.space$Female.Insecticide.Exposure[i],
                                      male.exposure = parameter.space$Male.Insecticide.Exposure[i],
                                      heritability = parameter.space$Heritability[i],
                                      dispersal.rate = parameter.space$Dispersal[i],
                                      coverage = parameter.space$Intervention.Coverage[i],
                                      standard.deviation = 50,
                                      vector.length = 1000,
                                      maximum.bioassay.survival.proportion = 1,
                                      michaelis.menten.slope = 1,
                                      regression.coefficient = 0.48,
                                      regression.intercept = 0.15,
                                      maximum.generations = 10000,
                                      half.population.bioassay.survival.resistance = 900,
                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                      return.threshold.value = 0.01, #does not matter as no fitness costs
                                      deployment.frequency = 2, #minimum resolution
                                      maximum.resistance.value = 90000,
                                      starting.refugia.resistance.score = c(parameter.space$initial.pyr.PRS[i], 20),
                                      starting.intervention.resistance.score = c(parameter.space$initial.pyr.PRS[i], 20),
                                      applied.insecticide.dose = parameter.space$dose[i], #dose is always the same
                                      recommended.insecticide.dose = 1,
                                      threshold.generations = 30,
                                      base.efficacy.decay.rate = 0,
                                      rapid.decay.rate = 0,
                                      deployment.interval.llin = 30, #only for combinations
                                      deployment.interval.irs = 10, #only for combinations
                                      probability.only.i.male = 0, #only for combinations
                                      probability.only.j.male = 0, #only for combinations
                                      probability.both.i.j.male = 1, #only for combinations
                                      probability.only.i.female = 0, #only for combinations
                                      probability.only.j.female = 0, #only for combinations
                                      probability.both.i.j.female = 1, #only for combinations
                                      n.cycles = 1,
                                      intervention.coverage.1 = 0,
                                      intervention.coverage.2 = 0,
                                      intervention.coverage.1.2 = 1,
                                      z.sd.intercept = 18,
                                      z.sd.coefficient = 0.4,
                                      mixture.strategy = "pyrethroid.plus",
                                      llin.insecticides,
                                      irs.insecticides,
                                      min.cross.selection = 0,
                                      max.cross.selection = 0,
                                      gonotrophic.cycle.length = 3,
                                      natural.daily.survival = 1)

  print("part 1")

  #Get end pyr resistances:
  pyr.1.int = c(subset(the.sim.1, site == "intervention"&
                         insecticide.tracked == 1)$resistance.score[max(the.sim.1$time.in.generations)])

  pyr.1.ref = c(subset(the.sim.1, site == "refugia"&
                         insecticide.tracked == 1)$resistance.score[max(the.sim.1$time.in.generations)])


  the.sim.2 = run_simulation_advanced(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                      irm.switch.strategy = "novel.sequence", #"rotation", "sequence", "insecticide.1"
                                      number.of.insecticides = 2,
                                      sd.scaled = TRUE, ##TRUE or FALSE
                                      exposure.scaling.factor = 10,
                                      female.fitness.cost = 0,
                                      male.fitness.cost = 0,
                                      female.exposure = parameter.space$Female.Insecticide.Exposure[i],
                                      male.exposure = parameter.space$Male.Insecticide.Exposure[i],
                                      heritability = parameter.space$Heritability[i],
                                      dispersal.rate = parameter.space$Dispersal[i],
                                      coverage = parameter.space$Intervention.Coverage[i],
                                      standard.deviation = 50,
                                      vector.length = 1000,
                                      maximum.bioassay.survival.proportion = 1,
                                      michaelis.menten.slope = 1,
                                      regression.coefficient = 0.48,
                                      regression.intercept = 0.15,
                                      maximum.generations = 10000,
                                      half.population.bioassay.survival.resistance = 900,
                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                      return.threshold.value = 0.01, #does not matter as no fitness costs
                                      deployment.frequency = 2, #minimum resolution
                                      maximum.resistance.value = 90000,
                                      starting.refugia.resistance.score = c(pyr.1.ref, 20),
                                      starting.intervention.resistance.score = c(pyr.1.int, 20),
                                      applied.insecticide.dose = parameter.space$dose[i], #dose is always the same
                                      recommended.insecticide.dose = 1,
                                      threshold.generations = 30,
                                      base.efficacy.decay.rate = 0,
                                      rapid.decay.rate = 0,
                                      deployment.interval.llin = 30, #only for combinations
                                      deployment.interval.irs = 10, #only for combinations
                                      probability.only.i.male = 0, #only for combinations
                                      probability.only.j.male = 0, #only for combinations
                                      probability.both.i.j.male = 1, #only for combinations
                                      probability.only.i.female = 0, #only for combinations
                                      probability.only.j.female = 0, #only for combinations
                                      probability.both.i.j.female = 1, #only for combinations
                                      n.cycles = 1,
                                      intervention.coverage.1 = 0,
                                      intervention.coverage.2 = 0,
                                      intervention.coverage.1.2 = 1,
                                      z.sd.intercept = 18,
                                      z.sd.coefficient = 0.4,
                                      mixture.strategy = "pyrethroid.plus",
                                      llin.insecticides,
                                      irs.insecticides,
                                      min.cross.selection = 0,
                                      max.cross.selection = 0,
                                      gonotrophic.cycle.length = 3,
                                      natural.daily.survival = 1)

  print("part 2")
  #Get end pyr resistances:

  pyr.2.int = subset(the.sim.2, site == "intervention"&
                       insecticide.tracked == 1)$resistance.score[max(the.sim.2$time.in.generations)]

  pyr.2.ref = subset(the.sim.2, site == "refugia"&
                       insecticide.tracked == 1)$resistance.score[max(the.sim.2$time.in.generations)]


  the.sim.3 = run_simulation_advanced(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                      irm.switch.strategy = "novel.sequence", #"rotation", "sequence", "insecticide.1"
                                      number.of.insecticides = 2,
                                      sd.scaled = TRUE, ##TRUE or FALSE
                                      exposure.scaling.factor = 10,
                                      female.fitness.cost = 0,
                                      male.fitness.cost = 0,
                                      female.exposure = parameter.space$Female.Insecticide.Exposure[i],
                                      male.exposure = parameter.space$Male.Insecticide.Exposure[i],
                                      heritability = parameter.space$Heritability[i],
                                      dispersal.rate = parameter.space$Dispersal[i],
                                      coverage = parameter.space$Intervention.Coverage[i],
                                      standard.deviation = 50,
                                      vector.length = 1000,
                                      maximum.bioassay.survival.proportion = 1,
                                      michaelis.menten.slope = 1,
                                      regression.coefficient = 0.48,
                                      regression.intercept = 0.15,
                                      maximum.generations = 10000,
                                      half.population.bioassay.survival.resistance = 900,
                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                      return.threshold.value = 0.01, #does not matter as no fitness costs
                                      deployment.frequency = 2, #minimum resolution
                                      maximum.resistance.value = 90000,
                                      starting.refugia.resistance.score = c(pyr.2.ref, 20),
                                      starting.intervention.resistance.score = c(pyr.2.int, 20),
                                      applied.insecticide.dose = parameter.space$dose[i], #dose is always the same
                                      recommended.insecticide.dose = 1,
                                      threshold.generations = 30,
                                      base.efficacy.decay.rate = 0,
                                      rapid.decay.rate = 0,
                                      deployment.interval.llin = 30, #only for combinations
                                      deployment.interval.irs = 10, #only for combinations
                                      probability.only.i.male = 0, #only for combinations
                                      probability.only.j.male = 0, #only for combinations
                                      probability.both.i.j.male = 1, #only for combinations
                                      probability.only.i.female = 0, #only for combinations
                                      probability.only.j.female = 0, #only for combinations
                                      probability.both.i.j.female = 1, #only for combinations
                                      n.cycles = 1,
                                      intervention.coverage.1 = 0,
                                      intervention.coverage.2 = 0,
                                      intervention.coverage.1.2 = 1,
                                      z.sd.intercept = 18,
                                      z.sd.coefficient = 0.4,
                                      mixture.strategy = "pyrethroid.plus",
                                      llin.insecticides,
                                      irs.insecticides,
                                      min.cross.selection = 0,
                                      max.cross.selection = 0)

  print("part 3")
  #Get end pyr resistances:
  pyr.3.int = c(subset(the.sim.3, site == "intervention" &
                         insecticide.tracked == 1)$resistance.score[max(the.sim.3$time.in.generations)])

  pyr.3.ref = c(subset(the.sim.3, site == "refugia" &
                         insecticide.tracked == 1)$resistance.score[max(the.sim.3$time.in.generations)])

  end.novel.1 = subset(the.sim.1, site == "intervention" &
                         insecticide.tracked == 2)$resistance.score[max(the.sim.1$time.in.generations)]

  end.novel.2 = subset(the.sim.2, site == "intervention" &
                         insecticide.tracked == 2)$resistance.score[max(the.sim.2$time.in.generations)]

  end.novel.3 = subset(the.sim.3, site == "intervention"&
                         insecticide.tracked == 2)$resistance.score[max(the.sim.3$time.in.generations)]

  duration.novel.1 = max(the.sim.1$time.in.generations)

  duration.novel.2 = max(the.sim.2$time.in.generations)

  duration.novel.3 = max(the.sim.3$time.in.generations)

  df = data.frame(start.pyr = parameter.space$initial.pyr.PRS[i],
                  mix.dose = parameter.space$dose[i],
                  f.exposure = parameter.space$Female.Insecticide.Exposure[i],
                  m.exposure = parameter.space$Male.Insecticide.Exposure[i],
                  heritability =  parameter.space$Heritability[i],
                  dispersal = parameter.space$Dispersal[i],
                  coverage = parameter.space$Intervention.Coverage[i],
                  pyr.1.int,
                  pyr.2.int,
                  pyr.3.int,
                  end.novel.1,
                  end.novel.2,
                  end.novel.3,
                  duration.novel.1,
                  duration.novel.2,
                  duration.novel.3)


  lifespans.list[[i]] = df
  print(i)
}

# df.2001_3000 = do.call(rbind, lifespans.list)
# write.csv(df.2001_3000, "seqrot_for_mixtures.2001_3000.csv")

df.6001_7000 = do.call(rbind, lifespans.list)
write.csv(df.6001_7000, "seqrot_for_mixtures.6001_7000.csv")
