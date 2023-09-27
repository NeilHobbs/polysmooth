library(devtools)
load_all()
parameter.space = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.3.csv")

library(dplyr)
library(purrr)

#Set up functions to run a lot faster than run_sim_advanced
  #Does lose user friendlyness though, but oh well.

simplify_function = function(female.exposure,
                             male.exposure,
                             heritability,
                             dispersal.rate,
                             intervention.coverage,
                             start.resistance.2,
                             dose.1,
                             dose.2,
                             threshold.gens,
                             base.decay.1,
                             base.decay.2){




  run_simulation_advanced_mixtures_simplified(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                                                                  irm.switch.strategy = "sequence", #"rotation", "sequence", "novel.sequence"
                                                                                  mixture.strategy = "mix.sequential.discrete",
                                                                                  number.of.insecticides = 2,
                                                                                  sd.scaled = TRUE, ##TRUE or FALSE
                                                                                  exposure.scaling.factor = 10,
                                                                                  female.fitness.cost = 0,
                                                                                  male.fitness.cost = 0,
                                                                                  female.exposure = female.exposure,
                                                                                  male.exposure = male.exposure,
                                                                                  heritability = heritability,
                                                                                  dispersal.rate = dispersal.rate,
                                                                                  coverage = x.5,
                                                                                  standard.deviation = 50,
                                                                                  vector.length = 200,
                                                                                  maximum.bioassay.survival.proportion = 1,
                                                                                  michaelis.menten.slope = 1,
                                                                                  regression.coefficient = 0.48,
                                                                                  regression.intercept = 0.15,
                                                                                  maximum.generations = 200,
                                                                                  half.population.bioassay.survival.resistance = 900,
                                                                                  withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                  return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                  deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                  maximum.resistance.value = 90000,
                                                                                  starting.refugia.resistance.score = c(0, start.resistance.2),
                                                                                  starting.intervention.resistance.score = c(0, start.resistance.2),
                                                                                  applied.insecticide.dose = c(dose.1, dose.2),
                                                                                  recommended.insecticide.dose = 1,
                                                                                  threshold.generations = threshold.gens,
                                                                                  base.efficacy.decay.rate = c(base.decay.1, base.decay.2),
                                                                                  rapid.decay.rate = 0.08,
                                                                                  deployment.interval.llin = NA, #only for combinations
                                                                                  deployment.interval.irs = NA, #only for combinations
                                                                                  probability.only.i.male = NA, #only for combinations
                                                                                  probability.only.j.male = NA, #only for combinations
                                                                                  probability.both.i.j.male = NA, #only for combinations
                                                                                  probability.only.i.female = NA, #only for combinations
                                                                                  probability.only.j.female = NA, #only for combinations
                                                                                  probability.both.i.j.female = NA, #only for combinations
                                                                                  n.cycles = 1,
                                                                                  intervention.coverage.1 = NA,
                                                                                  intervention.coverage.2 = NA,
                                                                                  intervention.coverage.1.2 = NA,
                                                                                  z.sd.coefficient = 0.4,
                                                                                  z.sd.intercept = 18,
                                                                                  llin.insecticides = NA,
                                                                                  irs.insecticides = NA,
                                                                                  min.cross.selection = 0,
                                                                                  max.cross.selection = 0)[[1]]["intervention",  , 200]




}



the.list = list(female.exposure = parameter.space$Female.Insecticide.Exposure[1:10],
                male.exposure = parameter.space$Male.Insecticide.Exposure[1:10],
                heritability = parameter.space$Heritability[1:10],
                dispersal.rate = parameter.space$Dispersal[1:10],
                intervention.coverage = parameter.space$Intervention.Coverage[1:10],
                start.resistance.2 = rep(0, 10),
                dose.1 = rep(1, 10),
                dose.2 = rep(1, 10),
                threshold.gens = rep(20, 10),
                base.decay.1 = rep(0, 10),
                base.decay.2 = rep(0, 10))

A = pmap(the.list, simplify_function)


do.call(rbind, A)
