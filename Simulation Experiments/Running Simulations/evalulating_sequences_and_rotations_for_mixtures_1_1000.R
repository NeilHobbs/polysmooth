parameter.space = read.csv("./Simulation Experiments/Setting Up Simulations/parameter.space.seqrot.for.mixtures.csv")

#SEQUENCES::::

sequences.list = list()
rotations.list = list()
for(i in 1:150000){

  sequences.list[[i]] = wrapper_run_simulation_mixtures_sd_scaled_simplified_sequences(maximum.generations = 300,
                                                                                      vector.length = 1000,
                                                                                      female.exposure = parameter.space$Female.Insecticide.Exposure[i],
                                                                                      exposure.scaling.factor = 10,
                                                                                      coverage = parameter.space$Intervention.Coverage[i],
                                                                                      dispersal.rate = parameter.space$Dispersal[i],
                                                                                      male.exposure = parameter.space$Male.Insecticide.Exposure[i],
                                                                                      maximum.bioassay.survival.proportion = 1,
                                                                                      michaelis.menten.slope = 1,
                                                                                      half.population.bioassay.survival.resistance = 900,
                                                                                      regression.coefficient = 0.48,
                                                                                      regression.intercept = 0.15,
                                                                                      n.cycles = 1,
                                                                                      number.of.insecticides = 3,
                                                                                      calc.withdrawal.threshold = 900000,
                                                                                      calc.return.threshold = 900000,
                                                                                      available.vector = c(1, 2, 3),
                                                                                      withdrawn.vector = c(),
                                                                                      z.sd.intercept = 18,
                                                                                      z.sd.coefficient = 0.4,
                                                                                      min.cross.selection = 0,
                                                                                      max.cross.selection = 0,
                                                                                      between.gonotrophic.survival = 1,
                                                                                      heritability = parameter.space$Heritability[i],
                                                                                      i.dose.a = parameter.space$,
                                                                                      i.dose.b = 1,
                                                                                      j.dose = 1,
                                                                                      k.dose = 1,
                                                                                      starting.refugia.resistance.score = c(10, 10, 10),
                                                                                      starting.intervention.resistance.score = c(10, 10, 10))

print("sequences")

rotations.list[[i]] = wrapper_run_simulation_mixtures_sd_scaled_simplified_rotations(maximum.generations = 300,
                                                                                     vector.length = 1000,
                                                                                     female.exposure = 0.7,
                                                                                     exposure.scaling.factor = 10,
                                                                                     coverage = 0.7,
                                                                                     dispersal.rate = 0.3,
                                                                                     male.exposure = 0.7,
                                                                                     maximum.bioassay.survival.proportion = 1,
                                                                                     michaelis.menten.slope = 1,
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     regression.coefficient = 0.48,
                                                                                     regression.intercept = 0.15,
                                                                                     n.cycles = 1,
                                                                                     number.of.insecticides = 3,
                                                                                     calc.withdrawal.threshold = 900000,
                                                                                     calc.return.threshold = 900000,
                                                                                     available.vector = c(1, 2, 3),
                                                                                     withdrawn.vector = c(),
                                                                                     z.sd.intercept = 18,
                                                                                     z.sd.coefficient = 0.4,
                                                                                     min.cross.selection = 0,
                                                                                     max.cross.selection = 0,
                                                                                     between.gonotrophic.survival = 1,
                                                                                     heritability = 0.3,
                                                                                     i.dose.a = 1,
                                                                                     i.dose.b = 1,
                                                                                     j.dose = 1,
                                                                                     k.dose = 1,
                                                                                     starting.refugia.resistance.score = c(10, 10, 10),
                                                                                     starting.intervention.resistance.score = c(10, 10, 10))

print("rotations")
print(i)

}










