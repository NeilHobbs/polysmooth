#Adding in operational realism: mismatched coverages:

#run for X generations:


parameter.space = read.csv("micromosaics_operational_biological_realism_parameter_space.csv")

temp.list = list()

for(i in 4587:40000){
  A = subset(convert_output_to_dataframe_mixtures(wrapper_run_simulation_mixtures(insecticide.parameters.df = create_insecticide_parameters_dataframe_advanced(number.of.insecticides = 2,
                                                                                                                                                               applied.insecticide.dose = 0.5,
                                                                                                                                                               recommended.insecticide.dose = 1,
                                                                                                                                                               threshold.generation = 10,
                                                                                                                                                               base.efficacy.decay.rate = 0,
                                                                                                                                                               rapid.decay.rate = 0,
                                                                                                                                                               heritability = c(parameter.space$Heritability.i[i], parameter.space$Heritability.j[i]),
                                                                                                                                                               female.fitness.cost = c(parameter.space$Female.Fitness.Cost.i[i], parameter.space$Female.Fitness.Cost.j[i]),
                                                                                                                                                               male.fitness.cost = c(parameter.space$Male.Fitness.Cost.i[i], parameter.space$Male.Fitness.Cost.j[i])),
                                                                                  maximum.generations = 500,
                                                                                  sim.array = set_starting_resistance_scores(sim.array = create_starting_array(n.insecticides = 2,
                                                                                                                                                               maximum.generations = 500),
                                                                                                                             starting.refugia.resistance.score = c(parameter.space$Initial.Resistance.i[i], parameter.space$Initial.Resistance.j[i]),
                                                                                                                             starting.intervention.resistance.score = c(parameter.space$Initial.Resistance.i[i], parameter.space$Initial.Resistance.j[i]),
                                                                                                                             number.of.insecticides = 2),
                                                                                  standard.deviation = 50,
                                                                                  vector.length = 250,
                                                                                  female.exposure = parameter.space$Female.Exposure[i],
                                                                                  exposure.scaling.factor = 10,
                                                                                  coverage = parameter.space$Intervention.Coverage[i],
                                                                                  dispersal.rate = parameter.space$Dispersal[i],
                                                                                  male.exposure = parameter.space$Male.Exposure[i],
                                                                                  maximum.bioassay.survival.proportion = 1,
                                                                                  michaelis.menten.slope = 1,
                                                                                  half.population.bioassay.survival.resistance = 900,
                                                                                  regression.coefficient = 0.48,
                                                                                  regression.intercept = 0.15,
                                                                                  n.cycles = 5,
                                                                                  irm.switch.strategy = "sequence",
                                                                                  deployment.frequency = 10,

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
                                                                                  mixture.strategy = "mix.sequential.discrete",
                                                                                  min.cross.selection = parameter.space$Cross.Resistance[i],
                                                                                  max.cross.selection = parameter.space$Cross.Resistance[i],
                                                                                  between.gonotrophic.survival = between_gonotrophic_cycle_survival(gonotrophic.cycle.length = 3,
                                                                                                                                                    natural.daily.survival = 0.8)), maximum.generations = 500, number.of.insecticides = 2,
                                                  maximum.bioassay.survival.proportion = 1, michaelis.menten.slope = 1, half.population.bioassay.survival.resistance = 900),
             site == "intervention")

  peak.score.i = max(subset(A, insecticide.tracked == 1)$resistance.score)
  peak.score.j = max(subset(A, insecticide.tracked == 2)$resistance.score)

  peak.survival.i = max(subset(A, insecticide.tracked == 1)$bioassay.survival)
  peak.survival.j = max(subset(A, insecticide.tracked == 2)$bioassay.survival)

  sim.duration = max(A$time.in.generations)

  temp.df = data.frame(peak.score.i, peak.score.j, peak.survival.i, peak.survival.j, sim.duration)

  temp.list[[i]] = temp.df

  print(i)
}

df = do.call("rbind", temp.list)

df$strategy = "Mixtures (HD_HD)"

results.df = cbind(df, parameter.space)


write.csv(results.df, "micromosaics_operational_biological_realism_mixtures_HDHD_simulations.csv")

