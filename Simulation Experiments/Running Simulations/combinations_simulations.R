#Combination simualtions::: IRS + LLIN
  #NO DECAY

library(devtools)
load_all()

parameter.space.df = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.combinations.csv")

parameter.space.df = rbind(parameter.space.df, parameter.space.df, parameter.space.df,
                           parameter.space.df, parameter.space.df)

parameter.space.df$initial.pyr = rep(c(4.5, 100, 225, 900, 3600), each = 10000)


end.insecticide.i = c()
end.insecticide.j = c()

for(i in 1:50000){

A = run_simulation_advanced_combinations_simplified(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                                irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                                                number.of.insecticides = 2,
                                                sd.scaled = TRUE, ##TRUE or FALSE
                                                exposure.scaling.factor = 10,
                                                female.fitness.cost = parameter.space.df$Female.Fitness.Cost[i],
                                                male.fitness.cost = parameter.space.df$Male.Fitness.Cost[i],
                                                female.exposure = parameter.space.df$Female.Insecticide.Exposure[i],
                                                male.exposure = parameter.space.df$Male.Insecticide.Exposure[i],
                                                heritability = parameter.space.df$Heritability[i],
                                                dispersal.rate = parameter.space.df$Dispersal[i],
                                                coverage = parameter.space.df$Intervention.Coverage[i],
                                                standard.deviation = 50,
                                                vector.length = 500,
                                                maximum.bioassay.survival.proportion = 1,
                                                michaelis.menten.slope = 1,
                                                regression.coefficient = 0.48,
                                                regression.intercept = 0.15,
                                                maximum.generations = 200,
                                                half.population.bioassay.survival.resistance = 900,
                                                withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                deployment.frequency = 200, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                maximum.resistance.value = 25000,
                                                starting.refugia.resistance.score = c(parameter.space.df$initial.pyr[i], 0),
                                                starting.intervention.resistance.score = c(parameter.space.df$initial.pyr[i], 0),
                                                applied.insecticide.dose = 1,
                                                recommended.insecticide.dose = 1,
                                                threshold.generations = 15,
                                                base.efficacy.decay.rate = 0,
                                                rapid.decay.rate = 0,
                                                deployment.interval.llin = 200, #only for combinations
                                                deployment.interval.irs = 200, #only for combinations
                                                probability.only.i.male = parameter.space.df$m.encounter.i[i], #only for combinations
                                                probability.only.j.male = parameter.space.df$m.encounter.j[i], #only for combinations
                                                probability.both.i.j.male = parameter.space.df$m.encounter.ij[i], #only for combinations
                                                probability.only.i.female = parameter.space.df$f.encounter.i[i], #only for combinations
                                                probability.only.j.female = parameter.space.df$f.encounter.j[i], #only for combinations
                                                probability.both.i.j.female = parameter.space.df$f.encounter.ij[i], #only for combinations
                                                n.cycles = 5,
                                                intervention.coverage.1 = parameter.space.df$coverage.i[i],
                                                intervention.coverage.2 = parameter.space.df$coverage.j[i],
                                                intervention.coverage.1.2 = parameter.space.df$coverage.ij[i],
                                                z.sd.intercept = 18,
                                                z.sd.coefficient = 0.4,
                                                mixture.strategy = NA,
                                                llin.insecticides = c(1),
                                                irs.insecticides = c(2),
                                                min.cross.selection = 0,
                                                max.cross.selection = 0,
                                                gonotrophic.cycle.length = 3,
                                                natural.daily.survival = parameter.space.df$Between.Gonotrophic.Survival[i])


end.insecticide.i[i] = subset(A, insecticide.tracked == 1 &
                                site == "intervention" &
                                time.in.generations == 200)$bioassay.survival

end.insecticide.j[i] = subset(A, insecticide.tracked == 2 &
                                  site == "intervention" &
                                  time.in.generations == 200)$bioassay.survival

print(i)
}


parameter.space.df$initial.pyr = rep(c(4.5, 100, 225, 900, 3600), each = 10000)

parameter.space.df$end.insecticide.i = end.insecticide.i
parameter.space.df$end.insecticide.j = end.insecticide.j

write.csv(parameter.space.df, "combinations.set_combinations.comparator.csv")



#############################
#standard llin comparator

llin.insecticide.i = c()
irs.insecticide.j = c()

for(i in 47956:50000){

  A = run_simulation_advanced_combinations_simplified(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                                      irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                                                      number.of.insecticides = 2,
                                                      sd.scaled = TRUE, ##TRUE or FALSE
                                                      exposure.scaling.factor = 10,
                                                      female.fitness.cost = parameter.space.df$Female.Fitness.Cost[i],
                                                      male.fitness.cost = parameter.space.df$Male.Fitness.Cost[i],
                                                      female.exposure = parameter.space.df$Female.Insecticide.Exposure[i],
                                                      male.exposure = parameter.space.df$Male.Insecticide.Exposure[i],
                                                      heritability = parameter.space.df$Heritability[i],
                                                      dispersal.rate = parameter.space.df$Dispersal[i],
                                                      coverage = parameter.space.df$Intervention.Coverage[i],
                                                      standard.deviation = 50,
                                                      vector.length = 500,
                                                      maximum.bioassay.survival.proportion = 1,
                                                      michaelis.menten.slope = 1,
                                                      regression.coefficient = 0.48,
                                                      regression.intercept = 0.15,
                                                      maximum.generations = 200,
                                                      half.population.bioassay.survival.resistance = 900,
                                                      withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                      return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                      deployment.frequency = 200, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                      maximum.resistance.value = 25000,
                                                      starting.refugia.resistance.score = c(parameter.space.df$initial.pyr[i], 0),
                                                      starting.intervention.resistance.score = c(parameter.space.df$initial.pyr[i], 0),
                                                      applied.insecticide.dose = c(1, 0), #makes it so infecticide j absent
                                                      recommended.insecticide.dose = 1,
                                                      threshold.generations = 15,
                                                      base.efficacy.decay.rate = 0,
                                                      rapid.decay.rate = 0,
                                                      deployment.interval.llin = 200, #only for combinations
                                                      deployment.interval.irs = 200, #only for combinations
                                                      probability.only.i.male = parameter.space.df$m.encounter.i[i], #only for combinations
                                                      probability.only.j.male = parameter.space.df$m.encounter.j[i], #only for combinations
                                                      probability.both.i.j.male = parameter.space.df$m.encounter.ij[i], #only for combinations
                                                      probability.only.i.female = parameter.space.df$f.encounter.i[i], #only for combinations
                                                      probability.only.j.female = parameter.space.df$f.encounter.j[i], #only for combinations
                                                      probability.both.i.j.female = parameter.space.df$f.encounter.ij[i], #only for combinations
                                                      n.cycles = 5,
                                                      intervention.coverage.1 = parameter.space.df$coverage.i[i],
                                                      intervention.coverage.2 = parameter.space.df$coverage.j[i],
                                                      intervention.coverage.1.2 = parameter.space.df$coverage.ij[i],
                                                      z.sd.intercept = 18,
                                                      z.sd.coefficient = 0.4,
                                                      mixture.strategy = NA,
                                                      llin.insecticides = c(1),
                                                      irs.insecticides = c(2),
                                                      min.cross.selection = 0,
                                                      max.cross.selection = 0,
                                                      gonotrophic.cycle.length = 3,
                                                      natural.daily.survival = parameter.space.df$Between.Gonotrophic.Survival[i])


  llin.insecticide.i[i] = subset(A, insecticide.tracked == 1 &
                                  site == "intervention" &
                                  time.in.generations == 200)$bioassay.survival

  irs.insecticide.j[i] = subset(A, insecticide.tracked == 2 &
                                  site == "intervention" &
                                  time.in.generations == 200)$bioassay.survival

  print(i)
}


parameter.space.df$end.insecticide.i = llin.insecticide.i
parameter.space.df$end.insecticide.j = irs.insecticide.j

write.csv(parameter.space.df, "combinations.set_LLIN_only.comparator.csv")








