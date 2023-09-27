#Micromosaics simulations : mixtures (HD_HD) comparator

#Issue is what level of resistance gives "control failure". The
# 10% cut-off is a somewhat arbitrary threshold. What if
# strategies are run allowing for higher resistanes?

#simulations are therefore starting at 10% bioassay survival and
# running through to 20% to see if this qualitatively
#impacts the relative performance of strategies.

library(devtools)
load_all()

parameter.space.micromosaics = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.micromosaics.csv")

#rescale the fitness costs to account for sd.scaled = TRUE
parameter.space.micromosaics$Female.Fitness.Cost = parameter.space.micromosaics$Female.Fitness.Cost/18
parameter.space.micromosaics$Male.Fitness.Cost = parameter.space.micromosaics$Male.Fitness.Cost/18

parameter.space.micromosaics = do.call("rbind", replicate(7, parameter.space.micromosaics, simplify = FALSE))




parameter.space.micromosaics$cross.resistance = rep(c(-0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5), each = 5000)

temp.list = list()

for(i in 32200:35000){
  A = subset(convert_output_to_dataframe_micromosaics(wrapper_run_simulation_micromosaics_sd_scaled(insecticide.parameters.df = create_insecticide_parameters_dataframe_advanced(number.of.insecticides = 2,
                                                                                                                                                                                 applied.insecticide.dose = 1,
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
                                                                                                                                               starting.refugia.resistance.score = 100,
                                                                                                                                               starting.intervention.resistance.score = 100,
                                                                                                                                               number.of.insecticides = 2),
                                                                                                    z.sd.coefficient = 0.4,
                                                                                                    z.sd.intercept = 18,
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
                                                                                                    intervention.coverage.1 = 0.5,
                                                                                                    intervention.coverage.2 = 0.5,
                                                                                                    number.of.insecticides = 2,
                                                                                                    withdrawal.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                                                                                                                              michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                                                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                                                                                                              bioassay.survival = 0.2,
                                                                                                                                                                              estimate.precision = 0.000001,
                                                                                                                                                                              minimum.resistance.value = 0,
                                                                                                                                                                              maximum.resistance.value = 90000),
                                                                                                    return.threshold = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                                                                                                                                          michaelis.menten.slope = 1, #must be set to 1 to work properly
                                                                                                                                                                          half.population.bioassay.survival.resistance = 900,
                                                                                                                                                                          bioassay.survival = 0.18,
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
temp.df$strategy = "micromosaics (50:50)"

write.csv(temp.df, "mm_simulations_micromosaics_comparator_high_resistance.csv")















