#Comparing sequences, rotations and mixtures under the smooth selection paradigm.

library(devtools)
load_all()

parameter.space.df = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv")



parameter.space.df = rbind(parameter.space.df, parameter.space.df, parameter.space.df, parameter.space.df,
                           parameter.space.df, parameter.space.df, parameter.space.df)


##Add in Cross Selection too:::
cross.selection = c(rep(-0.3, 5000), rep(-0.2, 5000), rep(-0.1, 5000),
                    rep(0, 5000), rep(0.1, 5000), rep(0.2, 5000), rep(0.3, 5000))


parameter.space.df$cross.selection = cross.selection


sequence.duration = c()
sequence.peak = c()
rotation.peak = c()
rotation.duration = c()
mixture.peak = c()
mixture.duration = c()

for(v in 1:nrow(parameter.space.df)){

  SEQ = run_simulation_advanced(irm.deployment.strategy = "singles",
                              irm.switch.strategy = "sequence",
                              number.of.insecticides = 2,
                              sd.scaled = FALSE, #SD will be fixed at 50 for polysmooth
                              exposure.scaling.factor = 10, #As calibrated
                              female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
                              male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
                              female.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                              male.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                              heritability = parameter.space.df$Heritability[v],
                              dispersal.rate = parameter.space.df$Dispersal[v],
                              coverage = parameter.space.df$Intervention.Coverage[v],
                              standard.deviation = 50, #Is fixed
                              vector.length = 250, #is good enough for model precision
                              maximum.bioassay.survival.proportion = 1,
                              michaelis.menten.slope = 1,
                              regression.coefficient = 0.48,
                              regression.intercept = 0.15,
                              maximum.generations = 500,
                              half.population.bioassay.survival.resistance = 900,
                              withdrawal.threshold.value = 0.1,
                              return.threshold.value = 0.08,
                              deployment.frequency = 10, #Use only 10
                              maximum.resistance.value = 25000,
                              starting.refugia.resistance.score = 0,
                              starting.intervention.resistance.score = 0,
                              applied.insecticide.dose = 1, #Deployed at full dose
                              recommended.insecticide.dose = 1, #Deployed at full dose
                              threshold.generations = 10,#No Decay
                              base.efficacy.decay.rate = 0,#No Decay
                              rapid.decay.rate = 0,#No Decay
                              deployment.interval.llin = NA, #only for combinations
                              deployment.interval.irs = NA, #only for combinations
                              probability.only.i.male = NA, #only for combinations
                              probability.only.j.male = NA, #only for combinations
                              probability.both.i.j.male = NA, #only for combinations
                              probability.only.i.female = NA, #only for combinations
                              probability.only.j.female = NA, #only for combinations
                              probability.both.i.j.female = NA, #only for combinations
                              n.cycles = 1, #Only one gonotrophic cycle needed
                              intervention.coverage.1 = NA,#only for combinations/mosaics
                              intervention.coverage.2 = NA, #only for combinations/mosaics
                              intervention.coverage.1.2 =  NA, #only for combinations/mosaics
                              z.sd.intercept =  NA, #SD is FIXED
                              z.sd.coefficient =  NA, #SD is FIXED
                              mixture.strategy = NA, # Not in Mixture
                              llin.insecticides = NA, #only for combinations
                              irs.insecticides = NA, #only for combinations
                              min.cross.selection = parameter.space.df$cross.selection[v],
                              max.cross.selection = parameter.space.df$cross.selection[v])


  ROT = run_simulation_advanced(irm.deployment.strategy = "singles",
                              irm.switch.strategy = "rotation",
                              number.of.insecticides = 2,
                              sd.scaled = FALSE, #SD will be fixed at 50 for polysmooth
                              exposure.scaling.factor = 10, #As calibrated
                              female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
                              male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
                              female.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                              male.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                              heritability = parameter.space.df$Heritability[v],
                              dispersal.rate = parameter.space.df$Dispersal[v],
                              coverage = parameter.space.df$Intervention.Coverage[v],
                              standard.deviation = 50, #Is fixed
                              vector.length = 250, #is good enough for model precision
                              maximum.bioassay.survival.proportion = 1,
                              michaelis.menten.slope = 1,
                              regression.coefficient = 0.48,
                              regression.intercept = 0.15,
                              maximum.generations = 500,
                              half.population.bioassay.survival.resistance = 900,
                              withdrawal.threshold.value = 0.1,
                              return.threshold.value = 0.08,
                              deployment.frequency = 10, #Use only 10
                              maximum.resistance.value = 25000,
                              starting.refugia.resistance.score = 0,
                              starting.intervention.resistance.score = 0,
                              applied.insecticide.dose = 1, #Deployed at full dose
                              recommended.insecticide.dose = 1, #Deployed at full dose
                              threshold.generations = 10,#No Decay
                              base.efficacy.decay.rate = 0,#No Decay
                              rapid.decay.rate = 0,#No Decay
                              deployment.interval.llin = NA, #only for combinations
                              deployment.interval.irs = NA, #only for combinations
                              probability.only.i.male = NA, #only for combinations
                              probability.only.j.male = NA, #only for combinations
                              probability.both.i.j.male = NA, #only for combinations
                              probability.only.i.female = NA, #only for combinations
                              probability.only.j.female = NA, #only for combinations
                              probability.both.i.j.female = NA, #only for combinations
                              n.cycles = 1, #Only one gonotrophic cycle needed
                              intervention.coverage.1 = NA,#only for combinations/mosaics
                              intervention.coverage.2 = NA, #only for combinations/mosaics
                              intervention.coverage.1.2 =  NA, #only for combinations/mosaics
                              z.sd.intercept =  NA, #SD is FIXED
                              z.sd.coefficient =  NA, #SD is FIXED
                              mixture.strategy = NA, # Not in Mixture
                              llin.insecticides = NA, #only for combinations
                              irs.insecticides = NA, #only for combinations
                              min.cross.selection = parameter.space.df$cross.selection[v],
                              max.cross.selection = parameter.space.df$cross.selection[v])


  MIX = run_simulation_advanced(irm.deployment.strategy = "mixtures",
                              irm.switch.strategy = "sequence",
                              number.of.insecticides = 2,
                              sd.scaled = FALSE, #SD will be fixed at 50 for polysmooth
                              exposure.scaling.factor = 10, #As calibrated
                              female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
                              male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
                              female.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
                              male.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
                              heritability = parameter.space.df$Heritability[v],
                              dispersal.rate = parameter.space.df$Dispersal[v],
                              coverage = parameter.space.df$Intervention.Coverage[v],
                              standard.deviation = 50, #Is fixed
                              vector.length = 250, #is good enough for model precision
                              maximum.bioassay.survival.proportion = 1,
                              michaelis.menten.slope = 1,
                              regression.coefficient = 0.48,
                              regression.intercept = 0.15,
                              maximum.generations = 500,
                              half.population.bioassay.survival.resistance = 900,
                              withdrawal.threshold.value = 0.1,
                              return.threshold.value = 0.08,
                              deployment.frequency = 10, #Use only 10
                              maximum.resistance.value = 25000,
                              starting.refugia.resistance.score = 0,
                              starting.intervention.resistance.score = 0,
                              applied.insecticide.dose = 1, #Deployed at full dose
                              recommended.insecticide.dose = 1, #Deployed at full dose
                              threshold.generations = 10,#No Decay
                              base.efficacy.decay.rate = 0,#No Decay
                              rapid.decay.rate = 0,#No Decay
                              deployment.interval.llin = NA, #only for combinations
                              deployment.interval.irs = NA, #only for combinations
                              probability.only.i.male = NA, #only for combinations
                              probability.only.j.male = NA, #only for combinations
                              probability.both.i.j.male = NA, #only for combinations
                              probability.only.i.female = NA, #only for combinations
                              probability.only.j.female = NA, #only for combinations
                              probability.both.i.j.female = NA, #only for combinations
                              n.cycles = 1, #Only one gonotrophic cycle needed
                              intervention.coverage.1 = NA,#only for combinations/mosaics
                              intervention.coverage.2 = NA, #only for combinations/mosaics
                              intervention.coverage.1.2 =  NA, #only for combinations/mosaics
                              z.sd.intercept =  NA, #SD is FIXED
                              z.sd.coefficient =  NA, #SD is FIXED
                              mixture.strategy = "mix.sequential.discrete", # Not in Mixture
                              llin.insecticides = NA, #only for combinations
                              irs.insecticides = NA, #only for combinations
                              min.cross.selection = parameter.space.df$cross.selection[v],
                              max.cross.selection = parameter.space.df$cross.selection[v])


  sequence.duration[v] = max(SEQ$time.in.generations)
  sequence.peak[v] = max(SEQ$resistance.score)
  rotation.duration[v] = max(ROT$time.in.generations)
  rotation.peak[v] = max(ROT$resistance.score)
  mixture.duration[v] = max(MIX$time.in.generations)
  mixture.peak[v] = max(MIX$resistance.score)

 print(v)
}


df = cbind(parameter.space.df, sequence.duration, sequence.peak,
           rotation.duration, rotation.peak, mixture.duration,
           mixture.peak)

write.csv(df, ".//sequence.rotation.mixture.smooth.fixedsd.csv")




#Due to the way that fitness costs are implemented, it is possible for fitness costs to prevent
  #resistane taking off. Therefore we remove these simulations from the analysis.

df = read.csv("sequence.rotation.mixture.smooth.fixedsd.csv")

df.1 = df%>%
  dplyr::filter(sequence.peak > 1)%>%
  dplyr::filter(mixture.peak > 1)%>%
  dplyr::filter(rotation.peak > 1)

df.1$seqrot = ifelse(df.1$sequence.duration > df.1$rotation.duration,
                     yes = "Sequence",
                     no = ifelse(df.1$sequence.duration < df.1$rotation.duration,
                                 yes = "Rotation",
                                 no = "Draw"))

df.1$mixrot = ifelse(df.1$mixture.duration > df.1$rotation.duration,
                     yes = "Mixture",
                     no = ifelse(df.1$mixture.duration < df.1$rotation.duration,
                                 yes = "Rotation",
                                 no = "Draw"))

df.1$mixseq = ifelse(df.1$mixture.duration > df.1$sequence.duration,
                     yes = "Mixture",
                     no = ifelse(df.1$mixture.duration < df.1$sequence.duration,
                                 yes = "Sequence",
                                 no = "Draw"))


library(ggplot2)
library(patchwork)
#Sequences vs Rotations
seq.rot.plot = ggplot(df.1, aes(x=sequence.duration - rotation.duration,
                 fill = seqrot))+
  geom_histogram(binwidth = 10)+
  scale_fill_manual(values = c("grey", "red", "blue"))+
  xlim(-210, 380)+
  xlab("Difference in Simulation Duration (Generations)")+
  ggtitle("Sequence vs Rotation")+
  facet_grid(cross.selection ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  guides(fill=guide_legend(title="Outcome"))+
  theme_bw()+
  theme(legend.position = "bottom")

#Mixtures vs Rotations
rot.mix.plot = ggplot(df.1, aes(x=mixture.duration - rotation.duration,
                 fill = mixrot))+
  scale_fill_manual(values = c("grey", "purple", "red"))+
  geom_histogram(binwidth = 10)+
  xlim(-210, 380)+
  facet_grid(cross.selection ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  xlab("Difference in Simulation Duration (Generations)")+
  ggtitle("Mixture vs Rotation")+
  guides(fill=guide_legend(title="Outcome"))+
  theme_bw()+
  theme(legend.position = "bottom")

#Mixtures vs Sequences
seq.mix.plot = ggplot(df.1, aes(x=mixture.duration - sequence.duration,
                 fill = mixseq))+
  geom_histogram(binwidth = 10)+
  xlim(-210, 380)+
  scale_fill_manual(values = c("grey", "purple", "blue"))+
  xlab("Difference in Simulation Duration (Generations")+
  ggtitle("Mixture vs Sequence")+
  guides(fill=guide_legend(title="Outcome"))+
  facet_grid(cross.selection ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "bottom")


seq.rot.plot + rot.mix.plot + seq.mix.plot + plot_annotation(title = "polysmooth")




colnames(df.1)

ggplot(df.1, aes(x=Intervention.Coverage))+
  geom_histogram()

ggplot(df.1, aes(x=Female.Insecticide.Exposure))+
  geom_histogram()

ggplot(df.1, aes(x=Male.Insecticide.Exposure))+
  geom_histogram()

ggplot(df.1, aes(x=Dispersal))+
  geom_histogram()

ggplot(df.1, aes(x=Female.Fitness.Cost))+
  geom_histogram()

ggplot(df.1, aes(x=Male.Fitness.Cost))+
  geom_histogram()

ggplot(df.1, aes(x=Heritability))+
  geom_histogram()








