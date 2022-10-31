#Calibrating the polysmooth model
#Identify what would be the suitable standard deviation and exposure scaling factor for calibration.


##Model Calibration for Smooth Selection

#Still need to have it such that the "average" insecticide lasts 10 years with continual use.
#Issue is now that the response at each generation time point is dependent on both the current
#mean value of the polygenic resistance score and the standard deviation of the Normal distribution.
library(devtools)
load_all()

parameter.space = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.3.csv")
standard.deviation = rep(c(rep(20, 5000), rep(30, 5000), rep(40, 5000), rep(50, 5000), rep(60, 5000), rep(70, 5000), rep(80, 5000)), 2)

parameter.space.df = rbind(parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space, parameter.space)

exposure.scaling.factor = c(rep(1, 35000), rep(10, 35000))

parameter.space.df$standard.deviation = standard.deviation
parameter.space.df$exposure.scaling.factor = exposure.scaling.factor


calibration = c()

for(v in 1:nrow(parameter.space.df)){

  A = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                              irm.switch.strategy = "sequence", #"rotation", "sequence", "novel.sequence"
                              mixture.strategy = NA ,
                              number.of.insecticides = 1,
                              sd.scaled = FALSE,
                              exposure.scaling.factor = parameter.space.df$exposure.scaling.factor[v],
                              female.fitness.cost = 0,
                              male.fitness.cost = 0,
                              female.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                              male.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                              heritability = parameter.space.df$Heritability[v],
                              dispersal.rate = 0,
                              coverage = 1,
                              standard.deviation = parameter.space.df$standard.deviation[v],
                              vector.length = 1000,
                              maximum.bioassay.survival.proportion = 1,
                              michaelis.menten.slope = 1,
                              regression.coefficient = 0.48,
                              regression.intercept = 0.15,
                              maximum.generations = 500,
                              half.population.bioassay.survival.resistance = 900,
                              withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                              return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                              deployment.frequency = 2, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                              maximum.resistance.value = 90000,
                              starting.refugia.resistance.score = 0,
                              starting.intervention.resistance.score = 0,
                              applied.insecticide.dose = 1,
                              recommended.insecticide.dose = 1,
                              threshold.generations = 10,
                              base.efficacy.decay.rate = 0,
                              rapid.decay.rate = 0,
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
                              z.sd.intercept = NA,
                              z.sd.coefficient = NA,
                              llin.insecticides = NA,
                              irs.insecticides = NA,
                              min.cross.selection = 0,
                              max.cross.selection = 0)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = c(max(B$time.in.generations))

  calibration[v] = simulation.duration

  if(v %% 50 == 0){print(v)}

}

df = cbind(parameter.space.df, calibration)
# write.csv(df, ".//polysmooth_calibration.csv")

calibration.df = read.csv(".//Model Calibration/polysmooth_calibration.csv")


plot.a = ggplot(calibration.df, aes(x=calibration/10))+
  geom_histogram(binwidth = 1,
                 fill = "#99d8c9")+
  geom_vline(xintercept = 8,
             colour = "red",
             alpha = 0.7,
             linetype = "dashed")+
  geom_vline(xintercept = 12,
             colour = "red",
             alpha = 0.7,
             linetype = "dashed")+
  # geom_rect(data = subset(calibration.df, exposure.scaling.factor %in% 10 & standard.deviation %in% 50),
  #           fill = NA, colour = "#c994c7", xmin = -Inf,xmax = Inf,
  #           ymin = -Inf,ymax = Inf,
  #           size = 5) +
  xlab("Operational Lifespan (years)")+
  ylab("Count")+
  facet_grid(exposure.scaling.factor~standard.deviation)+
  theme_classic()


plot.b = ggplot(data = subset(calibration.df, exposure.scaling.factor %in% 10 & standard.deviation %in% 50), aes(x=calibration/10))+
  geom_histogram(binwidth = 1,
                 fill = "#99d8c9",
                 colour = "white") +
  geom_vline(xintercept = 8,
             colour = "red",
             alpha = 0.7,
             linetype = "dashed")+
  geom_vline(xintercept = 12,
             colour = "red",
             alpha = 0.7,
             linetype = "dashed")+
  xlab("Operational Lifespan (years)")+
  ylab("Count")+
  facet_grid(exposure.scaling.factor~standard.deviation)+
  theme_classic()


library(patchwork)

layout = "
AAAB
AAAB
"
plot.a + plot.b +
  plot_layout(design = layout) + plot_annotation(title = "polysmooth")

##dd in Beta (exposure scaling factor) = 20

parameter.space = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.3.csv")
standard.deviation = c(rep(20, 5000), rep(30, 5000), rep(40, 5000), rep(50, 5000), rep(60, 5000), rep(70, 5000), rep(80, 5000))

parameter.space.df = rbind(parameter.space, parameter.space, parameter.space,
                           parameter.space, parameter.space, parameter.space,
                           parameter.space)

exposure.scaling.factor = rep(20, 35000)

parameter.space.df$standard.deviation = standard.deviation
parameter.space.df$exposure.scaling.factor = exposure.scaling.factor


calibration = c()

for(v in 1:nrow(parameter.space.df)){

  A = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                              irm.switch.strategy = "sequence", #"rotation", "sequence", "novel.sequence"
                              mixture.strategy = NA ,
                              number.of.insecticides = 1,
                              sd.scaled = FALSE,
                              exposure.scaling.factor = parameter.space.df$exposure.scaling.factor[v],
                              female.fitness.cost = 0,
                              male.fitness.cost = 0,
                              female.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                              male.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                              heritability = parameter.space.df$Heritability[v],
                              dispersal.rate = 0,
                              coverage = 1,
                              standard.deviation = parameter.space.df$standard.deviation[v],
                              vector.length = 1000,
                              maximum.bioassay.survival.proportion = 1,
                              michaelis.menten.slope = 1,
                              regression.coefficient = 0.48,
                              regression.intercept = 0.15,
                              maximum.generations = 500,
                              half.population.bioassay.survival.resistance = 900,
                              withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                              return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                              deployment.frequency = 2, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                              maximum.resistance.value = 90000,
                              starting.refugia.resistance.score = 0,
                              starting.intervention.resistance.score = 0,
                              applied.insecticide.dose = 1,
                              recommended.insecticide.dose = 1,
                              threshold.generations = 10,
                              base.efficacy.decay.rate = 0,
                              rapid.decay.rate = 0,
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
                              z.sd.intercept = NA,
                              z.sd.coefficient = NA,
                              llin.insecticides = NA,
                              irs.insecticides = NA,
                              min.cross.selection = 0,
                              max.cross.selection = 0)

  B = A%>%
    dplyr::filter(site == "intervention")

  simulation.duration = c(max(B$time.in.generations))

  calibration[v] = simulation.duration

  print(v)

}

df = cbind(parameter.space.df, calibration)
# write.csv(df, ".//polysmooth_calibration_beta20.csv")




