#################################################
#Micromosaic of Mixture LLIN with Standard LLIN #
#################################################
library(devtools)
load_all()
#No need for fitness costs as insecticides never withdrawn/rotated out.
#start simulations with 0.5% bioassay survival = 4.5 PRS
#as this helps to get simulations to take off when using sd.scaled = TRUE

#needs functions from mixtures_micromosaic_simulation_function script

parameter.space.micromosaics = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.micromosaics.csv")

parameter.space.micromosaics = do.call("rbind", replicate(25, parameter.space.micromosaics, simplify = FALSE))
parameter.space.micromosaics$Female.Fitness.Cost = 0
parameter.space.micromosaics$Male.Fitness.Cost = 0

parameter.space.micromosaics$pyr.start.resistance = rep(c(4.5, 100, 225, 900, 3600), each = 25000)
parameter.space.micromosaics$novel.start.resistance = rep(4.5, 125000)

parameter.space.micromosaics$coverage.llin = rep(rep(c(0, 0.25, 0.5, 0.75, 1), each = 5000), 5)
parameter.space.micromosaics$coverage.mixture = rep(rep(c(1, 0.75, 0.5, 0.25, 0), each = 5000), 5)

#LLIN only comparator has to be with full-dose. Which is already run. REMOVE

parameter.space.micromosaics = subset(parameter.space.micromosaics, coverage.llin != 1)



temp.list = list()
for(i in 1:100000){

  A = subset(convert_output_to_dataframe_mixture_micromosaic(wrapper_run_mixture_micromosaic_simulation_sd_scaled(exposure.scaling.factor = 10,
                                                                                                                  female.fitness.cost = parameter.space.micromosaics$Female.Fitness.Cost[i],
                                                                                                                  male.fitness.cost = parameter.space.micromosaics$Male.Fitness.Cost[i],
                                                                                                                  number.of.insecticides = 2,
                                                                                                                  female.exposure = parameter.space.micromosaics$Female.Insecticide.Exposure[i],
                                                                                                                  male.exposure = parameter.space.micromosaics$Male.Insecticide.Exposure[i],
                                                                                                                  heritability = parameter.space.micromosaics$Heritability[i],
                                                                                                                  dispersal.rate = parameter.space.micromosaics$Dispersal[i],
                                                                                                                  coverage = parameter.space.micromosaics$Intervention.Coverage[i],
                                                                                                                  standard.deviation = 50,#Does not matter as runs with variable SD
                                                                                                                  vector.length = 250,
                                                                                                                  maximum.bioassay.survival.proportion = 1,
                                                                                                                  michaelis.menten.slope = 1,
                                                                                                                  regression.coefficient = 0.48,
                                                                                                                  regression.intercept = 0.15,
                                                                                                                  maximum.generations = 200,
                                                                                                                  half.population.bioassay.survival.resistance = 900,
                                                                                                                  starting.refugia.resistance.score = c(parameter.space.micromosaics$pyr.start.resistance[i],
                                                                                                                                                        parameter.space.micromosaics$novel.start.resistance[i]),
                                                                                                                  starting.intervention.resistance.score = c(parameter.space.micromosaics$pyr.start.resistance[i],
                                                                                                                                                             parameter.space.micromosaics$novel.start.resistance[i]),
                                                                                                                  applied.insecticide.dose = 1,
                                                                                                                  recommended.insecticide.dose = 1,
                                                                                                                  threshold.generations = 10, #functionality is turned off
                                                                                                                  base.efficacy.decay.rate = 0,
                                                                                                                  rapid.decay.rate = 0,
                                                                                                                  n.cycles = 5,
                                                                                                                  standard.llin.coverage = parameter.space.micromosaics$coverage.llin[i],
                                                                                                                  mixture.llin.coverage = parameter.space.micromosaics$coverage.mixture[i],
                                                                                                                  z.sd.intercept = 18,
                                                                                                                  z.sd.coefficient = 0.4,
                                                                                                                  min.cross.selection = 0,
                                                                                                                  max.cross.selection = 0,
                                                                                                                  gonotrophic.cycle.length = 3,
                                                                                                                  natural.daily.survival = 0.8,
                                                                                                                  mixture.dosing.j = 0.5,
                                                                                                                  mixture.dosing.i = 0.5),
                                                             maximum.generations = 200,
                                                             number.of.insecticides = 2,
                                                      maximum.bioassay.survival.proportion = 1,
                                                      michaelis.menten.slope = 1,
                                                      half.population.bioassay.survival.resistance = 900),
             site == "intervention")

  peak.score.i = max(subset(A, insecticide.tracked == 1)$resistance.score)
  peak.score.j = max(subset(A, insecticide.tracked == 2)$resistance.score)

  peak.survival.i = max(subset(A, insecticide.tracked == 1)$bioassay.survival)
  peak.survival.j = max(subset(A, insecticide.tracked == 2)$bioassay.survival)

  mean.survival.i = mean(subset(A, insecticide.tracked == 1)$bioassay.survival)
  mean.survival.j = mean(subset(A, insecticide.tracked == 2)$bioassay.survival)

  median.survival.i = median(subset(A, insecticide.tracked == 1)$bioassay.survival)
  median.survival.j = median(subset(A, insecticide.tracked == 2)$bioassay.survival)

  dose = "Half-Dose (50% Efficacy)"

  temp.df = data.frame(peak.score.i, peak.score.j, peak.survival.i, peak.survival.j,
                       mean.survival.i, mean.survival.j, median.survival.i, median.survival.j,
                       dose)

  temp.list[[i]] = temp.df

  print(i)

}

mm.df = do.call("rbind", temp.list)

mm.df.final = cbind(mm.df, parameter.space.micromosaics)

write.csv(mm.df.final, "half.dose.50.mixture.llin_standard.llin_micromosaics.csv")




