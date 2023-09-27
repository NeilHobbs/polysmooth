###
#Looking at the impact of the between gonotrophic cycle survival:
library(devtools)
load_all()

g.length = rep(1, 10)
natural.survival = seq(0.1, 1, 0.1)


#param values:
female.exposure.value = runif(100, 0.4, 0.9)
male.exposure.value = runif(100, 0, 1)
heritability.value = runif(100, 0.05, 0.3)
dispersal.rate.value = runif(100, 0.1, 0.9)
coverage.value = runif(100, 0.1, 0.9)

sim.list = list()
for(j in 1:100){

seq.sim.list = list()
rot.sim.list = list()
mix.sim.list = list()
mm.sim.list = list()
combi.sim.list = list()

for(i in 1:10){

seq.sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                        irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                        number.of.insecticides = 1,
                        sd.scaled = FALSE, ##TRUE or FALSE
                        exposure.scaling.factor = 10,
                        female.fitness.cost = 0,
                        male.fitness.cost = 0,
                        female.exposure = female.exposure.value[j],
                        male.exposure = male.exposure.value[j],
                        heritability = heritability.value[j],
                        dispersal.rate = dispersal.rate.value[j],
                        coverage = coverage.value[j],
                        standard.deviation = 50,
                        vector.length = 100,
                        maximum.bioassay.survival.proportion = 1,
                        michaelis.menten.slope = 1,
                        regression.coefficient = 0.48,
                        regression.intercept = 0.15,
                        maximum.generations = 20,
                        half.population.bioassay.survival.resistance = 900,
                        withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                        return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                        deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                        maximum.resistance.value = 25000,
                        starting.refugia.resistance.score = 0,
                        starting.intervention.resistance.score = 0,
                        applied.insecticide.dose = 1,
                        recommended.insecticide.dose = 1,
                        threshold.generations = 10,
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
                        n.cycles = 3,
                        intervention.coverage.1 = 0.4,
                        intervention.coverage.2 = 0.4,
                        intervention.coverage.1.2 = 0.2,
                        z.sd.intercept = 18,
                        z.sd.coefficient = 0.4,
                        mixture.strategy = "pyrethroid.plus",
                        llin.insecticides,
                        irs.insecticides,
                        min.cross.selection =0,
                        max.cross.selection =0,
                        gonotrophic.cycle.length = g.length[i],
                        natural.daily.survival = natural.survival[i])
print("Sequence")


rot.sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                                            irm.switch.strategy = "rotation", #"rotation", "sequence", "insecticide.1"
                                            number.of.insecticides = 2,
                                            sd.scaled = FALSE, ##TRUE or FALSE
                                            exposure.scaling.factor = 10,
                                            female.fitness.cost = 0,
                                            male.fitness.cost = 0,
                                            female.exposure = female.exposure.value[j],
                                            male.exposure = male.exposure.value[j],
                                            heritability = heritability.value[j],
                                            dispersal.rate = dispersal.rate.value[j],
                                            coverage = coverage.value[j],
                                            standard.deviation = 50,
                                            vector.length = 100,
                                            maximum.bioassay.survival.proportion = 1,
                                            michaelis.menten.slope = 1,
                                            regression.coefficient = 0.48,
                                            regression.intercept = 0.15,
                                            maximum.generations = 20,
                                            half.population.bioassay.survival.resistance = 900,
                                            withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                            return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                            deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                            maximum.resistance.value = 25000,
                                            starting.refugia.resistance.score = 0,
                                            starting.intervention.resistance.score = 0,
                                            applied.insecticide.dose = 1,
                                            recommended.insecticide.dose = 1,
                                            threshold.generations = 10,
                                            base.efficacy.decay.rate = 0,
                                            rapid.decay.rate = 0,
                                            deployment.interval.llin = 10, #only for combinations
                                            deployment.interval.irs = 10, #only for combinations
                                            probability.only.i.male = 0.7, #only for combinations
                                            probability.only.j.male = 0.2, #only for combinations
                                            probability.both.i.j.male = 0.1, #only for combinations
                                            probability.only.i.female = 0.4, #only for combinations
                                            probability.only.j.female = 0.2, #only for combinations
                                            probability.both.i.j.female = 0.4, #only for combinations
                                            n.cycles = 3,
                                            intervention.coverage.1 = 0.4,
                                            intervention.coverage.2 = 0.4,
                                            intervention.coverage.1.2 = 0.2,
                                            z.sd.intercept = 18,
                                            z.sd.coefficient = 0.4,
                                            mixture.strategy = "pyrethroid.plus",
                                            llin.insecticides,
                                            irs.insecticides,
                                            min.cross.selection =0,
                                            max.cross.selection =0,
                                            gonotrophic.cycle.length = g.length[i],
                                            natural.daily.survival = natural.survival[i])

print("rotation")

  mix.sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                              irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                              number.of.insecticides = 2,
                                              sd.scaled = FALSE, ##TRUE or FALSE
                                              exposure.scaling.factor = 10,
                                              female.fitness.cost = 0,
                                              male.fitness.cost = 0,
                                              female.exposure = female.exposure.value[j],
                                              male.exposure = male.exposure.value[j],
                                              heritability = heritability.value[j],
                                              dispersal.rate = dispersal.rate.value[j],
                                              coverage = coverage.value[j],
                                              standard.deviation = 50,
                                              vector.length = 100,
                                              maximum.bioassay.survival.proportion = 1,
                                              michaelis.menten.slope = 1,
                                              regression.coefficient = 0.48,
                                              regression.intercept = 0.15,
                                              maximum.generations = 20,
                                              half.population.bioassay.survival.resistance = 900,
                                              withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                              return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                              deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                              maximum.resistance.value = 25000,
                                              starting.refugia.resistance.score = 0,
                                              starting.intervention.resistance.score = 0,
                                              applied.insecticide.dose = 1,
                                              recommended.insecticide.dose = 1,
                                              threshold.generations = 10,
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
                                              n.cycles = 3,
                                              intervention.coverage.1 = 0.4,
                                              intervention.coverage.2 = 0.4,
                                              intervention.coverage.1.2 = 0.2,
                                              z.sd.intercept = 18,
                                              z.sd.coefficient = 0.4,
                                              mixture.strategy = "pyrethroid.plus",
                                              llin.insecticides,
                                              irs.insecticides,
                                              min.cross.selection =0,
                                              max.cross.selection =0,
                                              gonotrophic.cycle.length = g.length[i],
                                              natural.daily.survival = natural.survival[i])
  print("mixture")

mm.sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "micromosaics", #singles, mixtures, micromosaics, combinations
                                            irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                            number.of.insecticides = 2,
                                            sd.scaled = FALSE, ##TRUE or FALSE
                                            exposure.scaling.factor = 10,
                                            female.fitness.cost = 0,
                                           male.fitness.cost = 0,
                                           female.exposure = female.exposure.value[j],
                                           male.exposure = male.exposure.value[j],
                                           heritability = heritability.value[j],
                                           dispersal.rate = dispersal.rate.value[j],
                                           coverage = coverage.value[j],
                                           standard.deviation = 50,
                                           vector.length = 100,
                                           maximum.bioassay.survival.proportion = 1,
                                           michaelis.menten.slope = 1,
                                           regression.coefficient = 0.48,
                                           regression.intercept = 0.15,
                                           maximum.generations = 20,
                                           half.population.bioassay.survival.resistance = 900,
                                            withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                            return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                            deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                            maximum.resistance.value = 25000,
                                            starting.refugia.resistance.score = 0,
                                            starting.intervention.resistance.score = 0,
                                            applied.insecticide.dose = 1,
                                            recommended.insecticide.dose = 1,
                                            threshold.generations = 10,
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
                                            n.cycles = 3,
                                            intervention.coverage.1 = 0.5,
                                            intervention.coverage.2 = 0.5,
                                            intervention.coverage.1.2 = 0,
                                            z.sd.intercept = 18,
                                            z.sd.coefficient = 0.4,
                                            mixture.strategy = "pyrethroid.plus",
                                            llin.insecticides,
                                            irs.insecticides,
                                            min.cross.selection =0,
                                            max.cross.selection =0,
                                            gonotrophic.cycle.length = g.length[i],
                                            natural.daily.survival = natural.survival[i])

print("micromosaics")

combi.sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                           irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                                           number.of.insecticides = 2,
                                           sd.scaled = FALSE, ##TRUE or FALSE
                                           exposure.scaling.factor = 10,
                                           female.fitness.cost = 0,
                                           male.fitness.cost = 0,
                                           female.exposure = female.exposure.value[j],
                                           male.exposure = male.exposure.value[j],
                                           heritability = heritability.value[j],
                                           dispersal.rate = dispersal.rate.value[j],
                                           coverage = coverage.value[j],
                                           standard.deviation = 50,
                                           vector.length = 100,
                                           maximum.bioassay.survival.proportion = 1,
                                           michaelis.menten.slope = 1,
                                           regression.coefficient = 0.48,
                                           regression.intercept = 0.15,
                                           maximum.generations = 20,
                                           half.population.bioassay.survival.resistance = 900,
                                           withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                           return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                           deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                           maximum.resistance.value = 25000,
                                           starting.refugia.resistance.score = 0,
                                           starting.intervention.resistance.score = 0,
                                           applied.insecticide.dose = 1,
                                           recommended.insecticide.dose = 1,
                                           threshold.generations = 10,
                                           base.efficacy.decay.rate = 0,
                                           rapid.decay.rate = 0,
                                           deployment.interval.llin = 30, #only for combinations
                                           deployment.interval.irs = 10, #only for combinations
                                           probability.only.i.male = 0.25, #only for combinations
                                           probability.only.j.male = 0.25, #only for combinations
                                           probability.both.i.j.male = 0.5, #only for combinations
                                           probability.only.i.female = 0.25, #only for combinations
                                           probability.only.j.female = 0.25, #only for combinations
                                           probability.both.i.j.female = 0.5, #only for combinations
                                           n.cycles = 3,
                                           intervention.coverage.1 = 0.25,
                                           intervention.coverage.2 = 0.25,
                                           intervention.coverage.1.2 = 0.5,
                                           z.sd.intercept = 18,
                                           z.sd.coefficient = 0.4,
                                           mixture.strategy = "pyrethroid.plus",
                                           llin.insecticides = 1,
                                           irs.insecticides = 2,
                                           min.cross.selection =0,
                                           max.cross.selection =0,
                                           gonotrophic.cycle.length = g.length[i],
                                           natural.daily.survival = natural.survival[i])
print("combination")

}
df.seq = do.call(rbind, seq.sim.list)
df.seq.1 = subset(df.seq, time.in.generations == 20 &
                site == "intervention" &
                  insecticide.tracked == 1)

df.rot = do.call(rbind, rot.sim.list)
df.rot.1 = subset(df.rot, time.in.generations == 20 &
                    site == "intervention"&
                    insecticide.tracked == 1)


df.mix = do.call(rbind, mix.sim.list)
df.mix.1 = subset(df.mix, time.in.generations == 20 &
                   site == "intervention"&
                    insecticide.tracked == 1)


df.mm = do.call(rbind, mm.sim.list)
df.mm.1 = subset(df.mm, time.in.generations == 20 &
                    site == "intervention"&
                   insecticide.tracked == 1)


df.combi = do.call(rbind, combi.sim.list)
df.combi.1 = subset(df.combi, time.in.generations == 20 &
                   site == "intervention"&
                     insecticide.tracked == 1)

bioassay.survival = c(df.seq.1$bioassay.survival,
                      df.rot.1$bioassay.survival,
                      df.mix.1$bioassay.survival,
                      df.mm.1$bioassay.survival,
                      df.combi.1$bioassay.survival)



strategy = c(rep("sequence", 10), rep("rotation", 10), rep("mixture", 10), rep("micromosaic", 10), rep("combination", 10))

natural.survival.probability = rep(natural.survival, 5)

comparison.df = data.frame(bioassay.survival,
                           strategy,
                           natural.survival.probability,
                           f.exposure = rep(female.exposure.value[j], 50),
                           m.exposure = rep(male.exposure.value[j], 50),
                           heritabillity = rep(heritability.value[j], 50),
                           dispersal = rep(dispersal.rate.value[j], 50),
                           coverage = rep(coverage.value[j], 50))



sim.list[[j]] = comparison.df
print(c(j, "complete"))

}


simulation.df = do.call(rbind, sim.list)

simulation.df$sim = rep(seq(1, 100, 1), each = 50)

simulation.df.0.1 = subset(simulation.df, natural.survival.probability == 0.1)
simulation.df.1.0 = subset(simulation.df, natural.survival.probability == 1)

gradient = simulation.df.1.0$bioassay.survival/simulation.df.0.1$bioassay.survival

simulation.df.0.1$gradient = gradient



simulation = c()
minimum.gradient = c()
maximum.gradient = c()

for(i in 1:100){

  A = subset(simulation.df.0.1, sim == i)


  max.gradient = max(A$gradient)

simulation[i] = i
minimum.gradient[i]  = min(A$gradient)
maximum.gradient[i]  = max(A$gradient)

}



temp.df = data.frame(simulation, minimum.gradient, maximum.gradient)

ggplot(subset(simulation.df, sim == 57 &
              strategy != "sequence"), aes(x=natural.survival.probability,
                                             y=bioassay.survival*100,
                                             colour = strategy))+
  geom_line(size = 3)+
  geom_vline(xintercept = (0.7^3))+
  geom_vline(xintercept = c(0.9^3))+
  ylab("End Bioassay Survival")+
  xlab("Natural Between Gonotrophic Survival Probability (p^n)")+
  theme_bw()

unique(subset(subset(simulation.df.0.1, strategy != "sequence"),
              gradient > 3.4)$sim)

simulation.df.0.1%>%
  dplyr::group_by(sim , strategy)%>%
  dplyr::summarise(range(gradient))




ggplot(subset(simulation.df.0.1,
              strategy %in% c("rotation",
                            "micromosaic")), aes(y=gradient,
                              x = strategy,
                              group = sim))+
  geom_line(size = 1,
            alpha = 0.5,
            colour = "grey")+
  geom_point(colour = "grey")+
  theme_bw()+
  theme(legend.position = "none")


subset(simulation.df.0.1,
       stragegy = "rotation")





sim.list.rot.mm = list()
for(j in 1:100){

  seq.sim.list = list()
  rot.sim.list = list()

  for(i in 1:10){


    rot.sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                                                irm.switch.strategy = "rotation", #"rotation", "sequence", "insecticide.1"
                                                number.of.insecticides = 2,
                                                sd.scaled = FALSE, ##TRUE or FALSE
                                                exposure.scaling.factor = 10,
                                                female.fitness.cost = 0,
                                                male.fitness.cost = 0,
                                                female.exposure = female.exposure.value[j],
                                                male.exposure = male.exposure.value[j],
                                                heritability = heritability.value[j],
                                                dispersal.rate = dispersal.rate.value[j],
                                                coverage = coverage.value[j],
                                                standard.deviation = 50,
                                                vector.length = 100,
                                                maximum.bioassay.survival.proportion = 1,
                                                michaelis.menten.slope = 1,
                                                regression.coefficient = 0.48,
                                                regression.intercept = 0.15,
                                                maximum.generations = 200,
                                                half.population.bioassay.survival.resistance = 900,
                                                withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                maximum.resistance.value = 25000,
                                                starting.refugia.resistance.score = 0,
                                                starting.intervention.resistance.score = 0,
                                                applied.insecticide.dose = 1,
                                                recommended.insecticide.dose = 1,
                                                threshold.generations = 10,
                                                base.efficacy.decay.rate = 0,
                                                rapid.decay.rate = 0,
                                                deployment.interval.llin = 10, #only for combinations
                                                deployment.interval.irs = 10, #only for combinations
                                                probability.only.i.male = 0.7, #only for combinations
                                                probability.only.j.male = 0.2, #only for combinations
                                                probability.both.i.j.male = 0.1, #only for combinations
                                                probability.only.i.female = 0.4, #only for combinations
                                                probability.only.j.female = 0.2, #only for combinations
                                                probability.both.i.j.female = 0.4, #only for combinations
                                                n.cycles = 1,
                                                intervention.coverage.1 = 0.4,
                                                intervention.coverage.2 = 0.4,
                                                intervention.coverage.1.2 = 0.2,
                                                z.sd.intercept = 18,
                                                z.sd.coefficient = 0.4,
                                                mixture.strategy = "pyrethroid.plus",
                                                llin.insecticides,
                                                irs.insecticides,
                                                min.cross.selection =0,
                                                max.cross.selection =0,
                                                gonotrophic.cycle.length = g.length[i],
                                                natural.daily.survival = natural.survival[i])

    print("rotation")

    mm.sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "micromosaics", #singles, mixtures, micromosaics, combinations
                                               irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                               number.of.insecticides = 2,
                                               sd.scaled = FALSE, ##TRUE or FALSE
                                               exposure.scaling.factor = 10,
                                               female.fitness.cost = 0,
                                               male.fitness.cost = 0,
                                               female.exposure = female.exposure.value[j],
                                               male.exposure = male.exposure.value[j],
                                               heritability = heritability.value[j],
                                               dispersal.rate = dispersal.rate.value[j],
                                               coverage = coverage.value[j],
                                               standard.deviation = 50,
                                               vector.length = 100,
                                               maximum.bioassay.survival.proportion = 1,
                                               michaelis.menten.slope = 1,
                                               regression.coefficient = 0.48,
                                               regression.intercept = 0.15,
                                               maximum.generations = 200,
                                               half.population.bioassay.survival.resistance = 900,
                                               withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                               return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                               deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                               maximum.resistance.value = 25000,
                                               starting.refugia.resistance.score = 0,
                                               starting.intervention.resistance.score = 0,
                                               applied.insecticide.dose = 1,
                                               recommended.insecticide.dose = 1,
                                               threshold.generations = 10,
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
                                               n.cycles = 1,
                                               intervention.coverage.1 = 0.5,
                                               intervention.coverage.2 = 0.5,
                                               intervention.coverage.1.2 = 0,
                                               z.sd.intercept = 18,
                                               z.sd.coefficient = 0.4,
                                               mixture.strategy = "pyrethroid.plus",
                                               llin.insecticides,
                                               irs.insecticides,
                                               min.cross.selection =0,
                                               max.cross.selection =0,
                                               gonotrophic.cycle.length = g.length[i],
                                               natural.daily.survival = natural.survival[i])

    print("micromosaics")



  }


  df.rot = do.call(rbind, rot.sim.list)
  df.rot.1 = subset(df.rot, time.in.generations == 200 &
                      site == "intervention"&
                      insecticide.tracked == 1)

  df.mm = do.call(rbind, mm.sim.list)
  df.mm.1 = subset(df.mm, time.in.generations == 200 &
                     site == "intervention"&
                     insecticide.tracked == 1)


  bioassay.survival = c(df.rot.1$bioassay.survival,
                        df.mm.1$bioassay.survival)



  strategy = c(rep("rotation", 10),  rep("micromosaic", 10))

  natural.survival.probability = rep(natural.survival, 2)

  comparison.df = data.frame(bioassay.survival,
                             strategy,
                             natural.survival.probability,
                             f.exposure = rep(female.exposure.value[j], 20),
                             m.exposure = rep(male.exposure.value[j], 20),
                             heritabillity = rep(heritability.value[j], 20),
                             dispersal = rep(dispersal.rate.value[j], 20),
                             coverage = rep(coverage.value[j], 20))



  sim.list.rot.mm[[j]] = comparison.df
  print(c(j, "complete"))

}


sim.df.rot.mm = do.call(rbind, sim.list.rot.mm)

sim.df.rot.mm.0.1 = subset(sim.df.rot.mm, natural.survival.probability == 0.1)
sim.df.rot.mm.1.0 = subset(sim.df.rot.mm, natural.survival.probability == 1)

sim.df.rot.mm.0.1$gradient = sim.df.rot.mm.1.0$bioassay.survival/sim.df.rot.mm.0.1$bioassay.survival



sim.df.rot.mm.0.1$category = rep(ifelse(subset(sim.df.rot.mm.0.1, strategy == "micromosaic")$gradient >
         subset(sim.df.rot.mm.0.1, strategy == "rotation")$gradient,
       yes = "MM Higher",
       no = "ROT Higher"), each = 2)

table(sim.df.rot.mm.0.1$category)

ggplot(sim.df.rot.mm.0.1, aes(x=strategy,
                              y=gradient,
                              colour = category,
                              group = f.exposure))+
  geom_line()+
  geom_point()+
  theme_bw()






sim.df.rot = subset(sim.df.rot.mm, strategy == "rotation")
sim.df.mm = subset(sim.df.rot.mm, strategy == "micromosaic")


sim.df.mm$who.wins = ifelse(sim.df.rot$bioassay.survival < sim.df.mm$bioassay.survival,
                  yes = "rot win",
                  no = "mm win")

sim.df.mm$difference = sim.df.rot$bioassay.survival - sim.df.mm$bioassay.survival

the.df = data.frame(table(sim.df.mm$who.wins, sim.df.mm$natural.survival.probability))

ggplot(sim.df.mm, aes(x=natural.survival.probability,
                   y=difference,
                   group = f.exposure,
                   colour = f.exposure))+
  geom_line()+
  theme_bw()







