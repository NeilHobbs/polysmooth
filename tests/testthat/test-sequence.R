#Providing rotation never occurs (e.g. deployment interval too long), then it should be identical to
  #to a sequence.

sequence.df.fixed  = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                                       irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                       number.of.insecticides = 2,
                                       sd.scaled = FALSE, ##TRUE or FALSE
                                       exposure.scaling.factor = 10,
                                       female.fitness.cost = 0,
                                       male.fitness.cost = 0,
                                       female.exposure = 0.7,
                                       male.exposure = 0.7,
                                       heritability = 0.3,
                                       dispersal.rate = 0.2,
                                       coverage = 0.8,
                                       standard.deviation = 50,
                                       vector.length = 1000,
                                       maximum.bioassay.survival.proportion = 1,
                                       michaelis.menten.slope = 1,
                                       regression.coefficient = 0.48,
                                       regression.intercept = 0.15,
                                       maximum.generations = 50,
                                       half.population.bioassay.survival.resistance = 900,
                                       withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                       return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                       deployment.frequency = 50, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                       maximum.resistance.value = 25000,
                                       starting.refugia.resistance.score = 0,
                                       starting.intervention.resistance.score = 0,
                                       applied.insecticide.dose = 1,
                                       recommended.insecticide.dose = 1,
                                       threshold.generations = 0,
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
                                       intervention.coverage.1.2 = NA,
                                       z.sd.intercept = 18,
                                       z.sd.coefficient = 0.4,
                                       mixture.strategy = "pyrethroid.plus",
                                       llin.insecticides,
                                       irs.insecticides,
                                       min.cross.selection = 0,
                                       max.cross.selection = 0)

rotation.df.fixed  = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                                       irm.switch.strategy = "rotation", #"rotation", "sequence", "insecticide.1"
                                       number.of.insecticides = 2,
                                       sd.scaled = FALSE, ##TRUE or FALSE
                                       exposure.scaling.factor = 10,
                                       female.fitness.cost = 0,
                                       male.fitness.cost = 0,
                                       female.exposure = 0.7,
                                       male.exposure = 0.7,
                                       heritability = 0.3,
                                       dispersal.rate = 0.2,
                                       coverage = 0.8,
                                       standard.deviation = 50,
                                       vector.length = 1000,
                                       maximum.bioassay.survival.proportion = 1,
                                       michaelis.menten.slope = 1,
                                       regression.coefficient = 0.48,
                                       regression.intercept = 0.15,
                                       maximum.generations = 50,
                                       half.population.bioassay.survival.resistance = 900,
                                       withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                       return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                       deployment.frequency = 50, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                       maximum.resistance.value = 25000,
                                       starting.refugia.resistance.score = 0,
                                       starting.intervention.resistance.score = 0,
                                       applied.insecticide.dose = 1,
                                       recommended.insecticide.dose = 1,
                                       threshold.generations = 0,
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
                                       intervention.coverage.1.2 = NA,
                                       z.sd.intercept = 18,
                                       z.sd.coefficient = 0.4,
                                       mixture.strategy = "pyrethroid.plus",
                                       llin.insecticides,
                                       irs.insecticides,
                                       min.cross.selection = 0,
                                       max.cross.selection = 0)

sequence.df.scaled  = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                                             irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                             number.of.insecticides = 2,
                                             sd.scaled = TRUE, ##TRUE or FALSE
                                             exposure.scaling.factor = 10,
                                             female.fitness.cost = 0,
                                             male.fitness.cost = 0,
                                             female.exposure = 0.7,
                                             male.exposure = 0.7,
                                             heritability = 0.3,
                                             dispersal.rate = 0.2,
                                             coverage = 0.8,
                                             standard.deviation = 50,
                                             vector.length = 1000,
                                             maximum.bioassay.survival.proportion = 1,
                                             michaelis.menten.slope = 1,
                                             regression.coefficient = 0.48,
                                             regression.intercept = 0.15,
                                             maximum.generations = 50,
                                             half.population.bioassay.survival.resistance = 900,
                                             withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                             return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                             deployment.frequency = 50, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                             maximum.resistance.value = 25000,
                                             starting.refugia.resistance.score = 0,
                                             starting.intervention.resistance.score = 0,
                                             applied.insecticide.dose = 1,
                                             recommended.insecticide.dose = 1,
                                             threshold.generations = 0,
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
                                             intervention.coverage.1.2 = NA,
                                             z.sd.intercept = 18,
                                             z.sd.coefficient = 0.4,
                                             mixture.strategy = "pyrethroid.plus",
                                             llin.insecticides,
                                             irs.insecticides,
                                             min.cross.selection = 0,
                                             max.cross.selection = 0)

rotation.df.scaled  = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                                             irm.switch.strategy = "rotation", #"rotation", "sequence", "insecticide.1"
                                             number.of.insecticides = 2,
                                             sd.scaled = TRUE, ##TRUE or FALSE
                                             exposure.scaling.factor = 10,
                                             female.fitness.cost = 0,
                                             male.fitness.cost = 0,
                                             female.exposure = 0.7,
                                             male.exposure = 0.7,
                                             heritability = 0.3,
                                             dispersal.rate = 0.2,
                                             coverage = 0.8,
                                             standard.deviation = 50,
                                             vector.length = 1000,
                                             maximum.bioassay.survival.proportion = 1,
                                             michaelis.menten.slope = 1,
                                             regression.coefficient = 0.48,
                                             regression.intercept = 0.15,
                                             maximum.generations = 50,
                                             half.population.bioassay.survival.resistance = 900,
                                             withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                             return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                             deployment.frequency = 50, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                             maximum.resistance.value = 25000,
                                             starting.refugia.resistance.score = 0,
                                             starting.intervention.resistance.score = 0,
                                             applied.insecticide.dose = 1,
                                             recommended.insecticide.dose = 1,
                                             threshold.generations = 0,
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
                                             intervention.coverage.1.2 = NA,
                                             z.sd.intercept = 18,
                                             z.sd.coefficient = 0.4,
                                             mixture.strategy = "pyrethroid.plus",
                                             llin.insecticides,
                                             irs.insecticides,
                                             min.cross.selection = 0,
                                             max.cross.selection = 0)


test_that("sequence and rotation identical fixed SD", {
  expect_equal(rotation.df.fixed, sequence.df.fixed)
})

test_that("sequence and rotation identical scaled SD", {
  expect_equal(rotation.df.scaled, sequence.df.scaled)
})
