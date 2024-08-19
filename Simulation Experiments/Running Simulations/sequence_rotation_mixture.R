#Comparing sequences, rotations and mixtures under the smooth selection paradigm.

library(devtools)
load_all()

parameter.space.df = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv")



parameter.space.df = rbind(parameter.space.df, parameter.space.df, parameter.space.df, parameter.space.df,
                           parameter.space.df, parameter.space.df, parameter.space.df)


##Add in Cross Selection too:::
cross.selection = c(rep(-0.3, 5000), rep(-0.2, 5000), rep(-0.1, 5000),
                    rep(0, 5000), rep(0.1, 5000), rep(0.2, 5000), rep(0.3, 5000))


# parameter.space.df$cross.selection = cross.selection
#
#
# sequence.duration = c()
# sequence.peak = c()
# rotation.peak = c()
# rotation.duration = c()
# mixture.peak = c()
# mixture.duration = c()
#
# for(v in 1:nrow(parameter.space.df)){
#
#   SEQ = run_simulation_advanced(irm.deployment.strategy = "singles",
#                               irm.switch.strategy = "sequence",
#                               number.of.insecticides = 2,
#                               sd.scaled = FALSE, #SD will be fixed at 50 for polysmooth
#                               exposure.scaling.factor = 10, #As calibrated
#                               female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
#                               male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
#                               female.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                               male.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                               heritability = parameter.space.df$Heritability[v],
#                               dispersal.rate = parameter.space.df$Dispersal[v],
#                               coverage = parameter.space.df$Intervention.Coverage[v],
#                               standard.deviation = 50, #Is fixed
#                               vector.length = 250, #is good enough for model precision
#                               maximum.bioassay.survival.proportion = 1,
#                               michaelis.menten.slope = 1,
#                               regression.coefficient = 0.48,
#                               regression.intercept = 0.15,
#                               maximum.generations = 500,
#                               half.population.bioassay.survival.resistance = 900,
#                               withdrawal.threshold.value = 0.1,
#                               return.threshold.value = 0.08,
#                               deployment.frequency = 10, #Use only 10
#                               maximum.resistance.value = 25000,
#                               starting.refugia.resistance.score = 0,
#                               starting.intervention.resistance.score = 0,
#                               applied.insecticide.dose = 1, #Deployed at full dose
#                               recommended.insecticide.dose = 1, #Deployed at full dose
#                               threshold.generations = 10,#No Decay
#                               base.efficacy.decay.rate = 0,#No Decay
#                               rapid.decay.rate = 0,#No Decay
#                               deployment.interval.llin = NA, #only for combinations
#                               deployment.interval.irs = NA, #only for combinations
#                               probability.only.i.male = NA, #only for combinations
#                               probability.only.j.male = NA, #only for combinations
#                               probability.both.i.j.male = NA, #only for combinations
#                               probability.only.i.female = NA, #only for combinations
#                               probability.only.j.female = NA, #only for combinations
#                               probability.both.i.j.female = NA, #only for combinations
#                               n.cycles = 1, #Only one gonotrophic cycle needed
#                               intervention.coverage.1 = NA,#only for combinations/mosaics
#                               intervention.coverage.2 = NA, #only for combinations/mosaics
#                               intervention.coverage.1.2 =  NA, #only for combinations/mosaics
#                               z.sd.intercept =  NA, #SD is FIXED
#                               z.sd.coefficient =  NA, #SD is FIXED
#                               mixture.strategy = NA, # Not in Mixture
#                               llin.insecticides = NA, #only for combinations
#                               irs.insecticides = NA, #only for combinations
#                               min.cross.selection = parameter.space.df$cross.selection[v],
#                               max.cross.selection = parameter.space.df$cross.selection[v])
#
#
#   ROT = run_simulation_advanced(irm.deployment.strategy = "singles",
#                               irm.switch.strategy = "rotation",
#                               number.of.insecticides = 2,
#                               sd.scaled = FALSE, #SD will be fixed at 50 for polysmooth
#                               exposure.scaling.factor = 10, #As calibrated
#                               female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
#                               male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
#                               female.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                               male.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                               heritability = parameter.space.df$Heritability[v],
#                               dispersal.rate = parameter.space.df$Dispersal[v],
#                               coverage = parameter.space.df$Intervention.Coverage[v],
#                               standard.deviation = 50, #Is fixed
#                               vector.length = 250, #is good enough for model precision
#                               maximum.bioassay.survival.proportion = 1,
#                               michaelis.menten.slope = 1,
#                               regression.coefficient = 0.48,
#                               regression.intercept = 0.15,
#                               maximum.generations = 500,
#                               half.population.bioassay.survival.resistance = 900,
#                               withdrawal.threshold.value = 0.1,
#                               return.threshold.value = 0.08,
#                               deployment.frequency = 10, #Use only 10
#                               maximum.resistance.value = 25000,
#                               starting.refugia.resistance.score = 0,
#                               starting.intervention.resistance.score = 0,
#                               applied.insecticide.dose = 1, #Deployed at full dose
#                               recommended.insecticide.dose = 1, #Deployed at full dose
#                               threshold.generations = 10,#No Decay
#                               base.efficacy.decay.rate = 0,#No Decay
#                               rapid.decay.rate = 0,#No Decay
#                               deployment.interval.llin = NA, #only for combinations
#                               deployment.interval.irs = NA, #only for combinations
#                               probability.only.i.male = NA, #only for combinations
#                               probability.only.j.male = NA, #only for combinations
#                               probability.both.i.j.male = NA, #only for combinations
#                               probability.only.i.female = NA, #only for combinations
#                               probability.only.j.female = NA, #only for combinations
#                               probability.both.i.j.female = NA, #only for combinations
#                               n.cycles = 1, #Only one gonotrophic cycle needed
#                               intervention.coverage.1 = NA,#only for combinations/mosaics
#                               intervention.coverage.2 = NA, #only for combinations/mosaics
#                               intervention.coverage.1.2 =  NA, #only for combinations/mosaics
#                               z.sd.intercept =  NA, #SD is FIXED
#                               z.sd.coefficient =  NA, #SD is FIXED
#                               mixture.strategy = NA, # Not in Mixture
#                               llin.insecticides = NA, #only for combinations
#                               irs.insecticides = NA, #only for combinations
#                               min.cross.selection = parameter.space.df$cross.selection[v],
#                               max.cross.selection = parameter.space.df$cross.selection[v])
#
#
#   MIX = run_simulation_advanced(irm.deployment.strategy = "mixtures",
#                               irm.switch.strategy = "sequence",
#                               number.of.insecticides = 2,
#                               sd.scaled = FALSE, #SD will be fixed at 50 for polysmooth
#                               exposure.scaling.factor = 10, #As calibrated
#                               female.fitness.cost = parameter.space.df$Female.Fitness.Cost[v],
#                               male.fitness.cost = parameter.space.df$Male.Fitness.Cost[v],
#                               female.exposure = parameter.space.df$Female.Insecticide.Exposure[v],
#                               male.exposure = parameter.space.df$Male.Insecticide.Exposure[v],
#                               heritability = parameter.space.df$Heritability[v],
#                               dispersal.rate = parameter.space.df$Dispersal[v],
#                               coverage = parameter.space.df$Intervention.Coverage[v],
#                               standard.deviation = 50, #Is fixed
#                               vector.length = 250, #is good enough for model precision
#                               maximum.bioassay.survival.proportion = 1,
#                               michaelis.menten.slope = 1,
#                               regression.coefficient = 0.48,
#                               regression.intercept = 0.15,
#                               maximum.generations = 500,
#                               half.population.bioassay.survival.resistance = 900,
#                               withdrawal.threshold.value = 0.1,
#                               return.threshold.value = 0.08,
#                               deployment.frequency = 10, #Use only 10
#                               maximum.resistance.value = 25000,
#                               starting.refugia.resistance.score = 0,
#                               starting.intervention.resistance.score = 0,
#                               applied.insecticide.dose = 1, #Deployed at full dose
#                               recommended.insecticide.dose = 1, #Deployed at full dose
#                               threshold.generations = 10,#No Decay
#                               base.efficacy.decay.rate = 0,#No Decay
#                               rapid.decay.rate = 0,#No Decay
#                               deployment.interval.llin = NA, #only for combinations
#                               deployment.interval.irs = NA, #only for combinations
#                               probability.only.i.male = NA, #only for combinations
#                               probability.only.j.male = NA, #only for combinations
#                               probability.both.i.j.male = NA, #only for combinations
#                               probability.only.i.female = NA, #only for combinations
#                               probability.only.j.female = NA, #only for combinations
#                               probability.both.i.j.female = NA, #only for combinations
#                               n.cycles = 1, #Only one gonotrophic cycle needed
#                               intervention.coverage.1 = NA,#only for combinations/mosaics
#                               intervention.coverage.2 = NA, #only for combinations/mosaics
#                               intervention.coverage.1.2 =  NA, #only for combinations/mosaics
#                               z.sd.intercept =  NA, #SD is FIXED
#                               z.sd.coefficient =  NA, #SD is FIXED
#                               mixture.strategy = "mix.sequential.discrete", # Not in Mixture
#                               llin.insecticides = NA, #only for combinations
#                               irs.insecticides = NA, #only for combinations
#                               min.cross.selection = parameter.space.df$cross.selection[v],
#                               max.cross.selection = parameter.space.df$cross.selection[v])
#
#
#   sequence.duration[v] = max(SEQ$time.in.generations)
#   sequence.peak[v] = max(SEQ$resistance.score)
#   rotation.duration[v] = max(ROT$time.in.generations)
#   rotation.peak[v] = max(ROT$resistance.score)
#   mixture.duration[v] = max(MIX$time.in.generations)
#   mixture.peak[v] = max(MIX$resistance.score)
#
#  print(v)
# }
#
#
# df = cbind(parameter.space.df, sequence.duration, sequence.peak,
#            rotation.duration, rotation.peak, mixture.duration,
#            mixture.peak)
#
# write.csv(df, ".//sequence.rotation.mixture.smooth.fixedsd.csv")
#



#Due to the way that fitness costs are implemented, it is possible for fitness costs to prevent
  #resistane taking off. Therefore we remove these simulations from the analysis.

df = read.csv("sequence.rotation.mixture.smooth.fixedsd.csv")
library(dplyr)
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
library(ggh4x)

#Sequences vs Rotations

seq.rot.plot = ggplot(subset(df.1, seqrot != "Draw"), aes(x=sequence.duration - rotation.duration,
                 fill = seqrot))+
  geom_histogram(binwidth = 20)+
  scale_fill_manual(values = c("red", "blue", "grey"))+
  ggtitle("Sequences vs Rotations")+
  ylab("Frequency")+
  xlab("Difference in Simulation Duration (Generations)")+
  guides(fill=guide_legend(title="Outcome"))+
  facet_grid(cross.selection ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.y.right = element_text(size = 14),
        axis.title.y.left = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.text = element_text(size =10,
                                 colour = "black"),
        axis.title.x = element_text(size = 12))

#Mixtures vs Rotations
rot.mix.plot = ggplot(subset(df.1, mixrot != "Draw"), aes(x=mixture.duration - rotation.duration,
                 fill = mixrot))+
  scale_fill_manual(values = c("purple", "red", "grey"))+
  geom_histogram(binwidth = 20)+

  ggtitle("Mixtures vs Rotations")+
  ylab("Frequency")+
  xlab("Difference in Simulation Duration (Generations)")+
  guides(fill=guide_legend(title="Outcome"))+
  facet_grid(cross.selection ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.y.right = element_text(size = 14),
        axis.title.y.left = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.text = element_text(size =10,
                                 colour = "black"),
        axis.title.x = element_text(size = 12))

#Mixtures vs Sequences
seq.mix.plot = ggplot(subset(df.1, mixseq != "Draw"), aes(x=mixture.duration - sequence.duration,
                 fill = mixseq))+
  geom_histogram(binwidth = 20)+
  scale_fill_manual(values = c("purple", "blue", "grey"))+
ggtitle("Mixtures vs Sequences")+
  ylab("Frequency")+
  xlab("Difference in Simulation Duration (Generations)")+
  guides(fill=guide_legend(title="Outcome"))+
  facet_grid(cross.selection ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.y.right = element_text(size = 14),
        axis.title.y.left = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.text = element_text(size =10,
                                 colour = "black"),
        axis.title.x = element_text(size = 12))

seq.rot.plot + rot.mix.plot + seq.mix.plot + plot_annotation(title = "Smooth Selection - polysmooth",
                                                             theme = theme(plot.title = element_text(size = 16)))



ggsave(
  filename = "chapter3_figure8.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 600,
  height = 300,
  units = "px",
  dpi = 200)


seq.rot.draw = subset(data.frame(table(df.1$seqrot, df.1$cross.selection)), Var1 == "Draw")
seq.rot.draw$cross.selection = seq.rot.draw$Var2
seq.rot.draw$number = paste0("Draws: \n", seq.rot.draw$Freq)
seq.rot.draw$seqrot = seq.rot.draw$Var1


seq.rot.plot = ggplot(subset(df.1, seqrot != "Draw"), aes(x=(sequence.duration - rotation.duration)/10,
                                                          fill = seqrot))+
  geom_histogram(binwidth = 1)+

   geom_label(data = seq.rot.draw, mapping = aes(x = 10, y = 200, label = number),
              size = 2)+
  scale_fill_manual(values = c("grey","#ef6548", "#3690c0" ))+
  scale_x_continuous(expand = c(0, 0), limits = c(-40, 40),
                     breaks = seq(-40, 40, 10))+
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+
  ggtitle(paste0("Monotherapy Sequences\nvs\nMonotherapy Rotations"))+
  ylab("Frequency")+
  xlab("Difference in Simulation Duration\n(Years)")+
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+

  guides(fill=guide_legend(title="Outcome"))+
  facet_grid2(cross.selection ~ .,
              strip =   strip_themed(
                # Horizontal strips
                background_y = elem_list_rect(fill = rev(c("#b2182b",
                                                           "#ef8a62",
                                                           "#fddbc7",
                                                           "#f7f7f7",
                                                           "#d1e5f0",
                                                           "#67a9cf",
                                                           "#2166ac")),
                                              by_layer_x = FALSE)))+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL),
                     expand = c(0,0))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.y.right = element_text(size = 14),
        axis.title.y.left = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.text = element_text(size =8,
                                 colour = "black"),
        axis.title.x = element_text(size = 12))



#Mixtures vs Rotations
rot.mix.draw = subset(data.frame(table(df.1$mixrot, df.1$cross.selection)), Var1 == "Draw")
rot.mix.draw$cross.selection = rot.mix.draw$Var2
rot.mix.draw$number = paste0("Draws: \n", rot.mix.draw$Freq)
rot.mix.draw$mixrot = rot.mix.draw$Var1


rot.mix.plot = ggplot(subset(df.1, mixrot != "Draw"), aes(x=(mixture.duration - rotation.duration)/10,
                                                          fill = mixrot))+
  scale_fill_manual(values = c("grey", "#88419d", "#ef6548"))+  geom_histogram(binwidth = 1)+
  geom_label(data = rot.mix.draw, mapping = aes(x = -20, y = 30, label = number),
             size = 2)+
  geom_vline(xintercept = 0, colour = "darkgrey", linetype = "dashed")+

  scale_x_continuous(expand = c(0, 0), limits = c(-40, 40),
                     breaks = seq(-40, 40, 10))+
  ggtitle(paste0("Mixtures\nvs\nMonotherapy Rotations"))+
  ylab("Frequency")+
  xlab("Difference in Simulation Duration\n(Years)")+
  guides(fill=guide_legend(title="Outcome"))+
  facet_grid2(cross.selection ~ .,
              strip =   strip_themed(
                # Horizontal strips
                background_y = elem_list_rect(fill = rev(c("#b2182b",
                                                           "#ef8a62",
                                                           "#fddbc7",
                                                           "#f7f7f7",
                                                           "#d1e5f0",
                                                           "#67a9cf",
                                                           "#2166ac")),
                                              by_layer_x = FALSE)))+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL),
                     expand = c(0,0))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.y.right = element_text(size = 14),
        axis.title.y.left = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.text = element_text(size =8,
                                 colour = "black"),
        axis.title.x = element_text(size = 12))

#Mixtures vs Sequences
seq.mix.draw = subset(data.frame(table(df.1$seqrot, df.1$cross.selection)), Var1 == "Draw")
seq.mix.draw$cross.selection = seq.mix.draw$Var2
seq.mix.draw$number = paste0("Draws: \n", seq.mix.draw$Freq)
seq.mix.draw$mixseq = seq.mix.draw$Var1


seq.mix.plot = ggplot(subset(df.1, mixseq != "Draw"), aes(x=(mixture.duration - sequence.duration)/10,
                                                          fill = mixseq))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("grey", "#88419d", "#ef6548"))+
  scale_x_continuous(expand = c(0, 0), limits = c(-40, 40),
                     breaks = seq(-40, 40, 10))+
  geom_label(data = seq.mix.draw, mapping = aes(x = -20, y = 30, label = number),
             size = 2)+
  ggtitle(paste0("Mixtures\nvs\nMonotherapy Sequences"))+
  ylab("Frequency")+
  xlab("Difference in Simulation Duration\n(Years)")+
  guides(fill=guide_legend(title="Outcome"))+
  facet_grid2(cross.selection ~ .,
              strip =   strip_themed(
                # Horizontal strips
                background_y = elem_list_rect(fill = rev(c("#b2182b",
                                                           "#ef8a62",
                                                           "#fddbc7",
                                                           "#f7f7f7",
                                                           "#d1e5f0",
                                                           "#67a9cf",
                                                           "#2166ac")),
                                              by_layer_x = FALSE)))+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL),
                     expand = c(0,0))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.y.right = element_text(size = 14),
        axis.title.y.left = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.text = element_text(size =8,
                                 colour = "black"),
        axis.title.x = element_text(size = 12))



seq.rot.plot + rot.mix.plot + seq.mix.plot + plot_annotation(title = "Smooth Selection - polysmooth",
                                                             theme = theme(plot.title = element_text(size = 16)))



ggsave(
  filename = "dynamicmethods_figure8.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 1200,
  height = 700,
  units = "px",
  dpi = 600)




















