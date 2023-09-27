#################################################
#Micromosaic of Mixture LLIN with Standard LLIN #
#################################################
library(devtools)
load_all()
#No need for fitness costs as insecticides never withdrawn/rotated out.
#start simulations with 0.5% bioassay survival = 4.5 PRS
#as this helps to get simulations to take off when using sd.scaled = TRUE

parameter.space.micromosaics = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.micromosaics.csv")

parameter.space.micromosaics = do.call("rbind", replicate(25, parameter.space.micromosaics, simplify = FALSE))
parameter.space.micromosaics$Female.Fitness.Cost = 0
parameter.space.micromosaics$Male.Fitness.Cost = 0

parameter.space.micromosaics$pyr.start.resistance = rep(c(4.5, 100, 225, 900, 3600), each = 25000)
parameter.space.micromosaics$novel.start.resistance = rep(4.5, 125000)

parameter.space.micromosaics$coverage.llin = rep(rep(c(0, 0.25, 0.5, 0.75, 1), each = 5000), 5)
parameter.space.micromosaics$coverage.mixture = rep(rep(c(1, 0.75, 0.5, 0.25, 0), each = 5000), 5)


temp.list = list()
for(i in 1:125000){

  A = subset(convert_output_to_dataframe_combinations(wrapper_run_simulation_combinations_sd_scaled(insecticide.parameters.df = create_insecticide_parameters_dataframe_advanced(number.of.insecticides = 2,
                                                                                                                                                                                 applied.insecticide.dose = 1,
                                                                                                                                                                                 recommended.insecticide.dose = 1,
                                                                                                                                                                                 threshold.generation = 10,
                                                                                                                                                                                 base.efficacy.decay.rate = 0,
                                                                                                                                                                                 rapid.decay.rate = 0,
                                                                                                                                                                                 heritability = parameter.space.micromosaics$Heritability[i],
                                                                                                                                                                                 female.fitness.cost = 0,
                                                                                                                                                                                 male.fitness.cost = 0),
                                                                                                    maximum.generations = 200,
                                                                                                    sim.array = set_starting_resistance_scores(sim.array = create_starting_array(n.insecticides = 2,
                                                                                                                                                                                 maximum.generations = 200),
                                                                                                                                               starting.refugia.resistance.score = c(parameter.space.micromosaics$pyr.start.resistance[i], parameter.space.micromosaics$novel.start.resistance[i]),
                                                                                                                                               starting.intervention.resistance.score = c(parameter.space.micromosaics$pyr.start.resistance[i], parameter.space.micromosaics$novel.start.resistance[i]),
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
                                                                                                    irs.insecticides = c(2),
                                                                                                    llin.insecticides = c(1),
                                                                                                    deployment.interval.llin = 201,
                                                                                                    deployment.interval.irs = 201,
                                                                                                    probability.only.i.male = 0,
                                                                                                    probability.only.j.male = 0,
                                                                                                    probability.both.i.j.male = 1,
                                                                                                    probability.only.i.female = 0,
                                                                                                    probability.only.j.female = 0,
                                                                                                    probability.both.i.j.female = 1,
                                                                                                    intervention.coverage.llin = parameter.space.micromosaics$coverage.llin[i],
                                                                                                    intervention.coverage.irs = 0,
                                                                                                    intervention.coverage.llin.irs = parameter.space.micromosaics$coverage.mixture[i],
                                                                                                    irm.switch.strategy = "sequence.irs",
                                                                                                    number.of.insecticides = 2,
                                                                                                    withdrawal.threshold = 90000,
                                                                                                    return.threshold = 90000,
                                                                                                    available.vector = seq(1, 2, by = 1),
                                                                                                    withdrawn.vector = c(),
                                                                                                    min.cross.selection = 0,
                                                                                                    max.cross.selection = 0,
                                                                                                    between.gonotrophic.survival = between_gonotrophic_cycle_survival(gonotrophic.cycle.length = 3,
                                                                                                                                                                      natural.daily.survival = 0.8)), maximum.generations = 200, number.of.insecticides = 2,
                                                      maximum.bioassay.survival.proportion = 1, michaelis.menten.slope = 1, half.population.bioassay.survival.resistance = 900),
             site == "intervention")

  peak.score.i = max(subset(A, insecticide.tracked == 1)$resistance.score)
  peak.score.j = max(subset(A, insecticide.tracked == 2)$resistance.score)

  peak.survival.i = max(subset(A, insecticide.tracked == 1)$bioassay.survival)
  peak.survival.j = max(subset(A, insecticide.tracked == 2)$bioassay.survival)

  mean.survival.i = mean(subset(A, insecticide.tracked == 1)$bioassay.survival)
  mean.survival.j = mean(subset(A, insecticide.tracked == 2)$bioassay.survival)

  median.survival.i = median(subset(A, insecticide.tracked == 1)$bioassay.survival)
  median.survival.j = median(subset(A, insecticide.tracked == 2)$bioassay.survival)


  temp.df = data.frame(peak.score.i, peak.score.j, peak.survival.i, peak.survival.j,
                       mean.survival.i, mean.survival.j, median.survival.i, median.survival.j)

  temp.list[[i]] = temp.df

  print(i)

}

mm.df = do.call("rbind", temp.list)

mm.df.final = cbind(mm.df, parameter.space.micromosaics)

write.csv(mm.df.final, "mixture.llin_standard.llin_micromosaics.csv")




