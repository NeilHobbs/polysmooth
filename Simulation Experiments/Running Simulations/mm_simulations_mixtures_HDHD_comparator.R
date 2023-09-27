#Micromosaics simulations : mixtures (HD_HD) comparator
library(devtools)
load_all()

parameter.space.micromosaics = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.micromosaics.csv")

parameter.space.micromosaics = do.call("rbind", replicate(7, parameter.space.micromosaics, simplify = FALSE))

parameter.space.micromosaics$cross.resistance = rep(c(-0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5), each = 5000)

temp.list = list()

for(i in 1:35000){
  A = subset(convert_output_to_dataframe_mixtures(wrapper_run_simulation_mixtures(insecticide.parameters.df = create_insecticide_parameters_dataframe_advanced(number.of.insecticides = 2,
                                                                                                                                                               applied.insecticide.dose = 0.5,
                                                                                                                                                               recommended.insecticide.dose = 1,
                                                                                                                                                               threshold.generation = 10,
                                                                                                                                                               base.efficacy.decay.rate = 0,
                                                                                                                                                               rapid.decay.rate = 0,
                                                                                                                                                               heritability = parameter.space.micromosaics$Heritability[i],
                                                                                                                                                               female.fitness.cost = parameter.space.micromosaics$Female.Fitness.Cost[i],
                                                                                                                                                               male.fitness.cost = parameter.space.micromosaics$Male.Fitness.Cost[i]),
                                                                                  maximum.generations = 500,
                                                                                  sim.array = set_starting_resistance_scores(sim.array = create_starting_array(n.insecticides = 2,
                                                                                                                                                               maximum.generations = 500),
                                                                                                                             starting.refugia.resistance.score = 0,
                                                                                                                             starting.intervention.resistance.score = 0,
                                                                                                                             number.of.insecticides = 2),
                                                                                  standard.deviation = 50,
                                                                                  vector.length = 250,
                                                                                  female.exposure = parameter.space.micromosaics$Female.Insecticide.Exposure[i],
                                                                                  exposure.scaling.factor = 10,
                                                                                  coverage = parameter.space.micromosaics$Intervention.Coverage[i],
                                                                                  dispersal.rate = parameter.space.micromosaics$Dispersal[i],
                                                                                  male.exposure = parameter.space.micromosaics$Male.Insecticide.Exposure[i],
                                                                                  maximum.bioassay.survival.proportion = 1,
                                                                                  michaelis.menten.slope = 1,
                                                                                  half.population.bioassay.survival.resistance = 900,
                                                                                  regression.coefficient = 0.48,
                                                                                  regression.intercept = 0.15,
                                                                                  n.cycles = 5,
                                                                                  irm.switch.strategy = "sequence",
                                                                                  deployment.frequency = 10,
                                                                                  mixture.strategy = "mix.sequential.discrete",
                                                                                  number.of.insecticides = 2,
                                                                                  calc.withdrawal.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                                                                                                            michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                                                                                                            half.population.bioassay.survival.resistance = 900,
                                                                                                                                                            bioassay.survival = 0.1,
                                                                                                                                                            estimate.precision = 0.000001,
                                                                                                                                                            minimum.resistance.value = 0,
                                                                                                                                                            maximum.resistance.value = 90000),
                                                                                  calc.return.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                                                                                                        michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                                                                                                        half.population.bioassay.survival.resistance = 900,
                                                                                                                                                        bioassay.survival = 0.08,
                                                                                                                                                        estimate.precision = 0.000001,
                                                                                                                                                        minimum.resistance.value = 0,
                                                                                                                                                        maximum.resistance.value = 90000),
                                                                                  available.vector = seq(1, 2, by = 1),
                                                                                  withdrawn.vector = c(),
                                                                                  min.cross.selection = parameter.space.micromosaics$cross.resistance[i],
                                                                                  max.cross.selection = parameter.space.micromosaics$cross.resistance[i],
                                                                                  between.gonotrophic.survival = between_gonotrophic_cycle_survival(gonotrophic.cycle.length = 3,
                                                                                                                                                    natural.daily.survival = 0.8)), maximum.generations = 500, number.of.insecticides = 2,
                                                  maximum.bioassay.survival.proportion = 1, michaelis.menten.slope = 1, half.population.bioassay.survival.resistance = 900),
             site == "intervention")

  peak.score = max(A$resistance.score)
  peak.survival = max(A$bioassay.survival)
  sim.duration = max(A$time.in.generations)

  temp.df = data.frame(peak.score, peak.survival, sim.duration)

  temp.list[[i]] = temp.df

  print(i)
}


B = do.call(rbind, temp.list)

temp.df = cbind(B, parameter.space.micromosaics)
temp.df$strategy = "mixtures (HD_HD)"

write.csv(temp.df, "mm_simulations_mixtures_HDHD_comparator.csv")















