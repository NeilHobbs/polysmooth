###How to Set up Simulations for Combinations...

###Coverages:
c.LLIN = rep(c(rep(c(0, 0, 0, 0, 0.25, 0.25, 0.25, 0.5, 0.5, 0.75), 13), 0.25, 0.5, 0.75), 1)
c.IRS = rep(c(rep(c(0, 0.25, 0.5, 0.75, 0, 0.25, 0.5, 0, 0.25, 0), 13), 0.75, 0.5, 0.25), 1)
c.LLIN.IRS = rep(c(rep(c(1, 0.75, 0.5, 0.25, 0.75, 0.5, 0.25,  0.5, 0.25, 0.25), 13), 0, 0, 0), 1)





temp.df = data.frame(c.LLIN, c.IRS, c.LLIN.IRS)




#Encounters:
e.LLIN = rep(c(c(rep(0, 10),
               rep(0.25, 10),
               rep(0.5, 10),
               rep(0.75, 10),
               rep(0, 10),
               rep(0, 10),
               rep(0, 10),
               rep(0.25, 10),
               rep(0.5, 10),
               rep(0.25, 10),
               rep(0.25, 10),
               rep(0.5, 10),
               rep(0.75, 10)
), 0, 0, 0), 1)

e.IRS = rep(c(c(rep(0, 10),
              rep(0, 10),
              rep(0, 10),
              rep(0, 10),
              rep(0.25, 10),
              rep(0.5, 10),
              rep(0.75, 10),
              rep(0.25, 10),
              rep(0.5, 10),
              rep(0.75, 10),
              rep(0.5, 10),
              rep(0.25, 10),
              rep(0.25, 10), 0, 0, 0)
), 1)

e.LLIN.IRS = rep(c(c(rep(1, 10),
                   rep(0.75, 10),
                   rep(0.5, 10),
                   rep(0.25, 10),
                   rep(0.75, 10),
                   rep(0.5, 10),
                   rep(0.25, 10),
                   rep(0.5, 10),
                   rep(0, 10),
                   rep(0, 10),
                   rep(0.25, 10),
                   rep(0.25, 10),
                   rep(0, 10), 1, 1, 1)
), 1)


gonotrophic.cycles = rep(5, 133)



female.exposure = runif(100, min=0.4, max=0.9)
the.list = list()

for(j in 1:100){
  sim.list = list()
  for(i in 1:133){

    sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                            irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                                            number.of.insecticides = 2,
                                            sd.scaled = FALSE, ##TRUE or FALSE
                                            exposure.scaling.factor = 10,
                                            female.fitness.cost = 0,
                                            male.fitness.cost = 0,
                                            female.exposure = female.exposure[j],
                                            male.exposure = 0.7,
                                            heritability = 0.3,
                                            dispersal.rate = 0.3,
                                            coverage = 0.7,
                                            standard.deviation = 50,
                                            vector.length = 100,
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
                                            probability.only.i.male = e.LLIN[i], #only for combinations
                                            probability.only.j.male = e.IRS[i], #only for combinations
                                            probability.both.i.j.male = e.LLIN.IRS[i], #only for combinations
                                            probability.only.i.female = e.LLIN[i], #only for combinations
                                            probability.only.j.female =  e.IRS[i], #only for combinations
                                            probability.both.i.j.female = e.LLIN.IRS[i], #only for combinations
                                            n.cycles = gonotrophic.cycles[i],
                                            intervention.coverage.1 = c.LLIN[i],
                                            intervention.coverage.2 = c.IRS[i],
                                            intervention.coverage.1.2 = c.LLIN.IRS[i],
                                            z.sd.intercept = 18,
                                            z.sd.coefficient = 0.4,
                                            mixture.strategy = "pyrethroid.plus",
                                            llin.insecticides = 1,
                                            irs.insecticides = 2,
                                            min.cross.selection = 0,
                                            max.cross.selection = 0,
                                            gonotrophic.cycle.length = 3,
                                            natural.daily.survival = 0.8)

    print(c(i, j, "combination"))
  }


  peak.llin = c()
  peak.irs = c()
  for(i in 1:133){

    peak.llin[i] = max(subset(sim.list[[i]], insecticide.tracked == 1)$bioassay.survival)
    peak.irs[i] = max(subset(sim.list[[i]], insecticide.tracked == 2)$bioassay.survival)



  }


  sim.list = list()
  for(i in 1:133){

    sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                            irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                                            number.of.insecticides = 2,
                                            sd.scaled = FALSE, ##TRUE or FALSE
                                            exposure.scaling.factor = 10,
                                            female.fitness.cost = 0,
                                            male.fitness.cost = 0,
                                            female.exposure = female.exposure[j],
                                            male.exposure = 0.7,
                                            heritability = 0.3,
                                            dispersal.rate = 0.3,
                                            coverage = 0.7,
                                            standard.deviation = 50,
                                            vector.length = 100,
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
                                            intervention.coverage.1 = c.LLIN[i],
                                            intervention.coverage.2 = c.IRS[i],
                                            intervention.coverage.1.2 = c.LLIN.IRS[i],
                                            z.sd.intercept = 18,
                                            z.sd.coefficient = 0.4,
                                            mixture.strategy = "pyrethroid.plus",
                                            llin.insecticides = 1,
                                            irs.insecticides = 2,
                                            min.cross.selection = 0,
                                            max.cross.selection = 0,
                                            gonotrophic.cycle.length = 3,
                                            natural.daily.survival = 0.8)

    print(c(i, j, "LLIN Alone"))
  }

  solo.peak.llin = c()
  solo.peak.irs = c()
  for(i in 1:133){

    solo.peak.llin[i] = max(subset(sim.list[[i]], insecticide.tracked == 1)$bioassay.survival)
    solo.peak.irs[i] = max(subset(sim.list[[i]], insecticide.tracked == 2)$bioassay.survival)



  }



  df = data.frame(peak.llin, peak.irs, e.LLIN, e.IRS, e.LLIN.IRS,
                  c.LLIN, c.IRS, c.LLIN.IRS, gonotrophic.cycles, solo.peak.llin)


  df$coverage.llin = df$c.LLIN + df$c.LLIN.IRS
  df$coverage.irs = df$c.IRS + c.LLIN.IRS
  df$encounter.llin = df$e.LLIN + df$e.LLIN.IRS
  df$encounter.irs = df$e.IRS + df$e.LLIN.IRS


  df$llin.percentage.change = (((peak.llin- solo.peak.llin)/solo.peak.llin)*100)
  df$female.exposure = rep(female.exposure[j], 133)

  the.list[[j]] = df

  print(c(j, "complete"))
}

