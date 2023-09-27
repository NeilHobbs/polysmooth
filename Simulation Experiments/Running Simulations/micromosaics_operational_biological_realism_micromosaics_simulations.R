#Adding in operational realism: mismatched coverages:

#run for X generations:


# A = data.frame(lhs::randomLHS(40000, 14))
#
# parameter.space = A|>
#   dplyr::rename(Heritability.i = X1)|>
#   dplyr::rename(Heritability.j = X2)|>
#   dplyr::rename(Initial.Resistance.i = X3)|>
#   dplyr::rename(Initial.Resistance.j = X4)|>
#   dplyr::rename(Male.Fitness.Cost.i = X5)|>
#   dplyr::rename(Male.Fitness.Cost.j = X6)|>
#   dplyr::rename(Female.Fitness.Cost.i = X7)|>
#   dplyr::rename(Female.Fitness.Cost.j= X8)|>
#   dplyr::rename(Cross.Resistance= X9)|>
#   dplyr::rename(Male.Exposure = X10)|>
#   dplyr::rename(Female.Exposure = X11)|>
#   dplyr::rename(Dispersal = X12)|>
#   dplyr::rename(Intervention.Coverage = X13)%>%
#   dplyr::rename(Coverage.i = X14)
#
# parameter.space = parameter.space|>
# dplyr::mutate(Heritability.i = qunif(Heritability.i, 0.05, 0.3))|>
#   dplyr::mutate(Heritability.j = qunif(Heritability.j, 0.05, 0.3))|>
#   dplyr::mutate(Initial.Resistance.i = qunif(Initial.Resistance.i, 0, 80))|>
#   dplyr::mutate(Initial.Resistance.j = qunif(Initial.Resistance.j, 0, 80))|>
#   dplyr::mutate(Male.Fitness.Cost.i = qunif(Male.Fitness.Cost.i, 0.04, 0.6))|>
#   dplyr::mutate(Male.Fitness.Cost.j =qunif(Male.Fitness.Cost.j, 0.04, 0.6))|>
#   dplyr::mutate(Female.Fitness.Cost.i = qunif(Female.Fitness.Cost.i, 0.04, 0.6))|>
#   dplyr::mutate(Female.Fitness.Cost.j= qunif(Female.Fitness.Cost.j, 0.04, 0.6))|>
#   dplyr::mutate(Cross.Resistance= qunif(Cross.Resistance, -0.5, 0.5))|>
#   dplyr::mutate(Male.Exposure = qunif(Male.Exposure, 0, 1))|>
#   dplyr::mutate(Female.Exposure = qunif(Female.Exposure, 0.4, 0.9))|>
#   dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))|>
#   dplyr::mutate(Intervention.Coverage = qunif(Intervention.Coverage, 0.5, 0.9))|>
#   dplyr::mutate(Coverage.i = qunif(Coverage.i, 0.4, 0.6))
#
# #Issue with floating point inaccuries:::
# parameter.space$Coverage.i = round(parameter.space$Coverage.i, 4)
#
# parameter.space$Coverage.j = 1 - parameter.space$Coverage.i
#
#
#
#
# write.csv(parameter.space, ".//micromosaics_operational_biological_realism_parameter_space.csv")

parameter.space = read.csv("micromosaics_operational_biological_realism_parameter_space.csv")

temp.list = list()

for(i in 1:40000){
  A = subset(convert_output_to_dataframe_micromosaics(wrapper_run_simulation_micromosaics(insecticide.parameters.df = create_insecticide_parameters_dataframe_advanced(number.of.insecticides = 2,
                                                                                                                                                                       applied.insecticide.dose = 1,
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
                                                                                          intervention.coverage.1 = parameter.space$Coverage.i[i],
                                                                                          intervention.coverage.2 = parameter.space$Coverage.j[i],
                                                                                          irm.switch.strategy = "sequence",
                                                                                          deployment.frequency = 10,

                                                                                          number.of.insecticides = 2,
                                                                                          withdrawal.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                                                                                                                    michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                                                                                                                    half.population.bioassay.survival.resistance = 900,
                                                                                                                                                                    bioassay.survival = 0.1,
                                                                                                                                                                    estimate.precision = 0.000001,
                                                                                                                                                                    minimum.resistance.value = 0,
                                                                                                                                                                    maximum.resistance.value = 90000),
                                                                                          return.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                                                                                                                michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                                                                                                                half.population.bioassay.survival.resistance = 900,
                                                                                                                                                                bioassay.survival = 0.08,
                                                                                                                                                                estimate.precision = 0.000001,
                                                                                                                                                                minimum.resistance.value = 0,
                                                                                                                                                                maximum.resistance.value = 90000),
                                                                                          available.vector = seq(1, 2, by = 1),
                                                                                          withdrawn.vector = c(),
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

df$strategy = "micromosaics"

results.df = cbind(df, parameter.space)


write.csv(results.df, "micromosaics_operational_biological_realism_micromosaics_simulations.csv")

