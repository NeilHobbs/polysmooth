##Script of examples of IRM strategies and how they are implemented in the model and what the strategies mean etc.

library(devtools)
load_all()

monotherapy.sequence.simulation.df = run_simulation_advanced(irm.deployment.strategy = "singles",
                                                             irm.switch.strategy = "sequence",
                                                             number.of.insecticides = 3,
                                                             sd.scaled = FALSE,
                                                             exposure.scaling.factor = 10,
                                                             female.fitness.cost = 0.7,
                                                             male.fitness.cost = 0.7,
                                                             female.exposure = 0.7,
                                                             male.exposure = 0.7,
                                                             heritability = 0.3,
                                                             dispersal.rate = 0.3,
                                                             coverage = 0.8,
                                                             standard.deviation = 50,
                                                             vector.length = 1000,
                                                             maximum.bioassay.survival.proportion = 1,
                                                             michaelis.menten.slope = 1,
                                                             regression.coefficient = 0.48,
                                                             regression.intercept = 0.15,
                                                             maximum.generations = 500,
                                                             half.population.bioassay.survival.resistance = 900,
                                                             withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                             return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                             deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                             maximum.resistance.value = 25000,
                                                             starting.refugia.resistance.score = 0,
                                                             starting.intervention.resistance.score = 0,
                                                             applied.insecticide.dose = 1,
                                                             recommended.insecticide.dose = 1,
                                                             threshold.generations = 15,
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
                                                             z.sd.intercept = 18,
                                                             z.sd.coefficient = 0.4,
                                                             mixture.strategy = "pyrethroid.plus",
                                                             llin.insecticides,
                                                             irs.insecticides,
                                                             min.cross.selection = 0,
                                                             max.cross.selection = 0,
                                                             gonotrophic.cycle.length = 3,
                                                             natural.daily.survival = 1)



ggplot(subset(monotherapy.sequence.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked,
           fill = insecticide.deployed))+
    geom_line(aes(x=time.in.generations,
                y = 11,
                colour = insecticide.deployed,
                linewidth = 8, alpha = 0.5))+
  geom_line(linewidth = 1.2, alpha = 0.5)+

  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
              colour = "grey")+
  geom_text(aes(x=101,
                y=5, label = paste0("Insecticide 1 Reaches Withdrawal Threshold")),
            colour = "#1b9e77",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=201,
                y=5, label = paste0("Insecticide 2 Reaches Withdrawal Threshold")),
            colour = "#d95f02",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=297,
                y=5, label = paste0("Insecticide 3 Reaches Withdrawal Threshold")),
            colour = "#7570b3",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=303,
                y=5, label = paste0("Insecticide 1 Re-Deployed: Return Threshold Reached")),
            colour = "#1b9e77",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=337,
                y=5, label = paste0("Insecticide 1 Reaches Withdrawal Threshold")),
            colour = "#1b9e77",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=343,
                y=5, label = paste0("Insecticide 2 Re-Deployed: Return Threshold Reached")),
            colour = "#d95f02",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=375,
                y=5, label = paste0("No Insecticides Available: SIMULATION TERMINATES")),
            colour = "red",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=500,
                y=5, label = paste0("500 Generation Maximum")),
            colour = "orange",
            angle = 270,
            size = 3.7)+
  geom_text(data = monotherapy.sequence.simulation.df, aes(x=50,
                y=10.2, label = paste0("Withdrawal Threshold")),
            colour = "black",
            size = 3.7)+
  geom_text(aes(x=50,
                y=8.2, label = paste0("Return Threshold")),
            colour = "black",
            size = 3.7)+
  geom_text(aes(x=150,
                y=11.3, label = paste0("Insecticide Deployed")),
            colour = "black",
            size = 4.5)+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))+
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))+
  ggtitle("Monotherapy Sequences")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 505))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12),
                     breaks = c(0, 2, 4, 6, 8, 10))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

######
monotherapy.rotation.simulation.df = run_simulation_advanced(irm.deployment.strategy = "singles",
                                                             irm.switch.strategy = "rotation",
                                                             number.of.insecticides = 3,
                                                             sd.scaled = FALSE,
                                                             exposure.scaling.factor = 10,
                                                             female.fitness.cost = 0.6,
                                                             male.fitness.cost = 0.6,
                                                             female.exposure = 0.7,
                                                             male.exposure = 0.7,
                                                             heritability = 0.3,
                                                             dispersal.rate = 0.3,
                                                             coverage = 0.8,
                                                             standard.deviation = 50,
                                                             vector.length = 1000,
                                                             maximum.bioassay.survival.proportion = 1,
                                                             michaelis.menten.slope = 1,
                                                             regression.coefficient = 0.48,
                                                             regression.intercept = 0.15,
                                                             maximum.generations = 500,
                                                             half.population.bioassay.survival.resistance = 900,
                                                             withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                             return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                             deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                             maximum.resistance.value = 25000,
                                                             starting.refugia.resistance.score = 0,
                                                             starting.intervention.resistance.score = 0,
                                                             applied.insecticide.dose = 1,
                                                             recommended.insecticide.dose = 1,
                                                             threshold.generations = 15,
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
                                                             z.sd.intercept = 18,
                                                             z.sd.coefficient = 0.4,
                                                             mixture.strategy = "pyrethroid.plus",
                                                             llin.insecticides,
                                                             irs.insecticides,
                                                             min.cross.selection = 0,
                                                             max.cross.selection = 0,
                                                             gonotrophic.cycle.length = 3,
                                                             natural.daily.survival = 1)


ggplot(subset(monotherapy.rotation.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked,
           fill = insecticide.deployed))+
  geom_line(aes(x=time.in.generations,
                y = 11,
                colour = insecticide.deployed,
                linewidth = 8, alpha = 0.5))+
  geom_line(linewidth = 1.2, alpha = 0.5)+

  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
             colour = "grey")+
  geom_text(aes(x=432,
                y=5, label = paste0("Insecticide 1 Reaches Withdrawal Threshold")),
            colour = "#1b9e77",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=442,
                y=5, label = paste0("Insecticide 2 Reaches Withdrawal Threshold")),
            colour = "#d95f02",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=452,
                y=5, label = paste0("Insecticide 3 Reaches Withdrawal Threshold")),
            colour = "#7570b3",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=500,
                y=5, label = paste0("500 Generation Maximum")),
            colour = "orange",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=50,
                y=10.2, label = paste0("Withdrawal Threshold")),
            colour = "black",
            size = 3.7)+
  geom_text(aes(x=50,
                y=8.2, label = paste0("Return Threshold")),
            colour = "black",
            size = 3.7)+
  geom_text(aes(x=150,
                y=11.3, label = paste0("Insecticide Deployed")),
            colour = "black",
            size = 4.5)+
  geom_text(aes(x=458,
                y=5, label = paste0("No Insecticides Available: SIMULATION TERMINATES")),
            colour = "red",
            angle = 270,
            size = 3.7)+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))+
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))+
  ggtitle("Monotherapy Rotations")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 510))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12),
                     breaks = c(0, 2, 4, 6, 8, 10))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

###

monotherapy.adaptive.rotation.simulation.df = run_simulation_advanced(irm.deployment.strategy = "singles",
                                                             irm.switch.strategy = "adaptive.rotation",
                                                             number.of.insecticides = 3,
                                                             sd.scaled = FALSE,
                                                             exposure.scaling.factor = 10,
                                                             female.fitness.cost = c(0.05, 0.1, 0.4),
                                                             male.fitness.cost = c(0.05, 0.1, 0.4),
                                                             female.exposure = 0.7,
                                                             male.exposure = 0.7,
                                                             heritability = c(0.3, 0.3, 0.25),
                                                             dispersal.rate = 0.3,
                                                             coverage = 0.8,
                                                             standard.deviation = 50,
                                                             vector.length = 1000,
                                                             maximum.bioassay.survival.proportion = 1,
                                                             michaelis.menten.slope = 1,
                                                             regression.coefficient = 0.48,
                                                             regression.intercept = 0.15,
                                                             maximum.generations = 500,
                                                             half.population.bioassay.survival.resistance = 900,
                                                             withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                             return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                             deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                             maximum.resistance.value = 25000,
                                                             starting.refugia.resistance.score = c(20, 30, 50),
                                                             starting.intervention.resistance.score = c(20, 30, 50),
                                                             applied.insecticide.dose = 1,
                                                             recommended.insecticide.dose = 1,
                                                             threshold.generations = 15,
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
                                                             z.sd.intercept = 18,
                                                             z.sd.coefficient = 0.4,
                                                             mixture.strategy = "pyrethroid.plus",
                                                             llin.insecticides,
                                                             irs.insecticides,
                                                             min.cross.selection = 0,
                                                             max.cross.selection = 0,
                                                             gonotrophic.cycle.length = 3,
                                                             natural.daily.survival = 1)


ggplot(subset(monotherapy.adaptive.rotation.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked,
           fill = insecticide.deployed))+
  geom_line(aes(x=time.in.generations,
                y = 11,
                colour = insecticide.deployed,
                linewidth = 8, alpha = 0.5))+
  geom_line(linewidth = 1.2, alpha = 0.5)+

  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
             colour = "grey")+
  geom_text(aes(x=120,
                y=5, label = paste0("Insecticide 3 Reaches Withdrawal Threshold")),
            colour = "#7570b3",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=300,
                y=5, label = paste0("Insecticide 3 Reaches Return Threshold")),
            colour = "#7570b3",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=350,
                y=5, label = paste0("Insecticide 3 Reaches Withdrawal Threshold")),
            colour = "#7570b3",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=406,
                y=5, label = paste0("Only Insecticide 1 Available: Deployed in Sequence")),
            colour = "#1b9e77",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=398,
                y=5, label = paste0("Insecticide 2 Reaches Withdrawal Threshold")),
            colour = "#d95f02",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=500,
                y=5, label = paste0("500 Generation Maximum")),
            colour = "orange",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=50,
                                                           y=10.2, label = paste0("Withdrawal Threshold")),
            colour = "black",
            size = 3.7)+
  geom_text(aes(x=50,
                y=8.2, label = paste0("Return Threshold")),
            colour = "black",
            size = 3.7)+
  geom_text(aes(x=150,
                y=11.3, label = paste0("Insecticide Deployed")),
            colour = "black",
            size = 4.5)+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))+
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))+
  ggtitle("Monotherapy Adaptive Rotations")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 510))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12),
                     breaks = c(0, 2, 4, 6, 8, 10))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

#####
mixtures.novel.sequence.simulation.df = run_simulation_advanced(irm.deployment.strategy = "mixtures",
                                                                      irm.switch.strategy = "novel.sequence",
                                                                      number.of.insecticides = 3,
                                                                      sd.scaled = FALSE,
                                                                      exposure.scaling.factor = 10,
                                                                      female.fitness.cost = 0,
                                                                      male.fitness.cost = 0,
                                                                      female.exposure = 0.7,
                                                                      male.exposure = 0.7,
                                                                      heritability = 0.3,
                                                                      dispersal.rate = 0.3,
                                                                      coverage = 0.8,
                                                                      standard.deviation = 50,
                                                                      vector.length = 1000,
                                                                      maximum.bioassay.survival.proportion = 1,
                                                                      michaelis.menten.slope = 1,
                                                                      regression.coefficient = 0.48,
                                                                      regression.intercept = 0.15,
                                                                      maximum.generations = 500,
                                                                      half.population.bioassay.survival.resistance = 900,
                                                                      withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                      return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                      deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                      maximum.resistance.value = 25000,
                                                                      starting.refugia.resistance.score = c(80,0, 0),
                                                                      starting.intervention.resistance.score = c(80, 0, 0, 50),
                                                                      applied.insecticide.dose = 1,
                                                                      recommended.insecticide.dose = 1,
                                                                      threshold.generations = 15,
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
                                                                      z.sd.intercept = 18,
                                                                      z.sd.coefficient = 0.4,
                                                                      mixture.strategy = "pyrethroid.plus",
                                                                      llin.insecticides,
                                                                      irs.insecticides,
                                                                      min.cross.selection = 0,
                                                                      max.cross.selection = 0,
                                                                      gonotrophic.cycle.length = 3,
                                                                      natural.daily.survival = 1)



ggplot(subset(mixtures.novel.sequence.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked))+
  geom_line(aes(x=time.in.generations,
                y = 19,
                colour = deployed.mixture.part.1,
                linewidth = 8, alpha = 0.5))+
  geom_line(aes(x=time.in.generations,
                y = 18.6,
                colour = deployed.mixture.part.2,
                linewidth = 8, alpha = 0.5))+
  geom_line(linewidth = 1.2, alpha = 0.5)+

  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
             colour = "grey")+
  geom_text(aes(x=150,
                y=19.6, label = paste0("Mixture Deployed")),
            colour = "black",
            size = 4.5)+
  geom_text(aes(x=102,
                y=8, label = paste0("Insecticide 1 Remains Deployed in Mixture")),
            colour = "#1b9e77",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=385,
                y=8, label = paste0("Insecticide 2 Reaches Withdrawal Threshold")),
            colour = "#d95f02",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=395,
                y=8, label = paste0("Mixture with Insecticide 3 Deployed")),
            colour = "#7570b3",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=50,
                y=10.2, label = paste0("Withdrawal Threshold")),
            colour = "black",
            size = 3)+
  geom_text(aes(x=50,
                y=8.2, label = paste0("Return Threshold")),
            colour = "black",
            size = 3)+
  geom_text(aes(x=500,
                y=8, label = paste0("500 Generation Maximum")),
            colour = "orange",
            angle = 270,
            size = 3.7)+
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Mixtures: Sequence Novel Partner")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 510))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20),
                     breaks = c(0, 2, 4, 6, 8, 10, 15))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


####

mixtures.novel.rotation.simulation.df = run_simulation_advanced(irm.deployment.strategy = "mixtures",
                                                                irm.switch.strategy = "novel.rotation",
                                                                number.of.insecticides = 3,
                                                                sd.scaled = FALSE,
                                                                exposure.scaling.factor = 10,
                                                                female.fitness.cost = 0,
                                                                male.fitness.cost = 0,
                                                                female.exposure = 0.7,
                                                                male.exposure = 0.7,
                                                                heritability = 0.3,
                                                                dispersal.rate = 0.3,
                                                                coverage = 0.8,
                                                                standard.deviation = 50,
                                                                vector.length = 1000,
                                                                maximum.bioassay.survival.proportion = 1,
                                                                michaelis.menten.slope = 1,
                                                                regression.coefficient = 0.48,
                                                                regression.intercept = 0.15,
                                                                maximum.generations = 500,
                                                                half.population.bioassay.survival.resistance = 900,
                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                maximum.resistance.value = 25000,
                                                                starting.refugia.resistance.score = c(80,0, 0),
                                                                starting.intervention.resistance.score = c(80, 0, 0, 50),
                                                                applied.insecticide.dose = 1,
                                                                recommended.insecticide.dose = 1,
                                                                threshold.generations = 15,
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
                                                                z.sd.intercept = 18,
                                                                z.sd.coefficient = 0.4,
                                                                mixture.strategy = "pyrethroid.plus",
                                                                llin.insecticides,
                                                                irs.insecticides,
                                                                min.cross.selection = 0,
                                                                max.cross.selection = 0,
                                                                gonotrophic.cycle.length = 3,
                                                                natural.daily.survival = 1)



ggplot(subset(mixtures.novel.rotation.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked))+
  geom_line(aes(x=time.in.generations,
                y = 19,
                colour = deployed.mixture.part.1,
                linewidth = 8, alpha = 0.5))+
  geom_line(aes(x=time.in.generations,
                y = 18.6,
                colour = deployed.mixture.part.2,
                linewidth = 8, alpha = 0.5))+
  geom_line(linewidth = 1.2, alpha = 0.5)+

  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
             colour = "grey")+
  geom_text(aes(x=150,
                y=19.6, label = paste0("Mixture Deployed")),
            colour = "black",
            size = 4.5)+
  geom_text(aes(x=102,
                y=8, label = paste0("Insecticide 1 Remains Deployed in Mixture")),
            colour = "#1b9e77",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=50,
                y=10.2, label = paste0("Withdrawal Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=50,
                y=8.2, label = paste0("Return Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=500,
                y=8, label = paste0("500 Generation Maximum")),
            colour = "orange",
            angle = 270,
            size = 3.7)+
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Mixtures: Rotate Novel Partner")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 510))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20),
                     breaks = c(0, 2, 4, 6, 8, 10, 15))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


#####
mixtures.standard.sequence.simulation.df = run_simulation_advanced(irm.deployment.strategy = "mixtures",
                                                                irm.switch.strategy = "sequence",
                                                                number.of.insecticides = 4,
                                                                sd.scaled = FALSE,
                                                                exposure.scaling.factor = 10,
                                                                female.fitness.cost = 0,
                                                                male.fitness.cost = 0,
                                                                female.exposure = 0.7,
                                                                male.exposure = 0.7,
                                                                heritability = c(0.3, 0.25, 0.3, 0.25),
                                                                dispersal.rate = 0.2,
                                                                coverage = 0.9,
                                                                standard.deviation = 50,
                                                                vector.length = 1000,
                                                                maximum.bioassay.survival.proportion = 1,
                                                                michaelis.menten.slope = 1,
                                                                regression.coefficient = 0.48,
                                                                regression.intercept = 0.15,
                                                                maximum.generations = 500,
                                                                half.population.bioassay.survival.resistance = 900,
                                                                withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                maximum.resistance.value = 25000,
                                                                starting.refugia.resistance.score = 0,
                                                                starting.intervention.resistance.score = 0,
                                                                applied.insecticide.dose = 1,
                                                                recommended.insecticide.dose = 1,
                                                                threshold.generations = 15,
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
                                                                z.sd.intercept = 18,
                                                                z.sd.coefficient = 0.4,
                                                                mixture.strategy = "mix.sequential.discrete",
                                                                llin.insecticides,
                                                                irs.insecticides,
                                                                min.cross.selection = 0,
                                                                max.cross.selection = 0,
                                                                gonotrophic.cycle.length = 3,
                                                                natural.daily.survival = 1)



ggplot(subset(mixtures.standard.sequence.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked))+
  geom_line(aes(x=time.in.generations,
                y = 11,
                colour = deployed.mixture.part.1,
                linewidth = 8, alpha = 0.5))+
  geom_line(aes(x=time.in.generations,
                y = 11.2,
                colour = deployed.mixture.part.2,
                linewidth = 8, alpha = 0.5))+
  geom_line(linewidth = 1.2, alpha = 0.5)+
  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
             colour = "grey")+
  geom_text(aes(x=150,
                y=11.4, label = paste0("Mixture Deployed")),
            colour = "black",
            size = 4.5)+
  geom_text(aes(x=300,
                y=5, label = paste0("Insecticide 1 Reaches Withdrawal Threshold \nMixture A is Withdrawn")),
            colour = "#1b9e77",
            angle = 270,
            size = 3.7)+

  geom_text(aes(x=50,
                y=10.2, label = paste0("Withdrawal Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=50,
                y=8.2, label = paste0("Return Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=500,
                y=5, label = paste0("500 Generation Maximum")),
            colour = "orange",
            angle = 270,
            size = 3.7)+
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"))+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Mixtures in Sequence")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 510))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12),
                     breaks = c(0, 2, 4, 6, 8, 10))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))



#####
mixtures.standard.rotation.simulation.df = run_simulation_advanced(irm.deployment.strategy = "mixtures",
                                                                   irm.switch.strategy = "rotation",
                                                                   number.of.insecticides = 4,
                                                                   sd.scaled = FALSE,
                                                                   exposure.scaling.factor = 10,
                                                                   female.fitness.cost = 0,
                                                                   male.fitness.cost = 0,
                                                                   female.exposure = 0.7,
                                                                   male.exposure = 0.7,
                                                                   heritability = c(0.3, 0.25, 0.3, 0.25),
                                                                   dispersal.rate = 0.2,
                                                                   coverage = 0.9,
                                                                   standard.deviation = 50,
                                                                   vector.length = 1000,
                                                                   maximum.bioassay.survival.proportion = 1,
                                                                   michaelis.menten.slope = 1,
                                                                   regression.coefficient = 0.48,
                                                                   regression.intercept = 0.15,
                                                                   maximum.generations = 500,
                                                                   half.population.bioassay.survival.resistance = 900,
                                                                   withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                   return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                   deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                   maximum.resistance.value = 25000,
                                                                   starting.refugia.resistance.score = 0,
                                                                   starting.intervention.resistance.score = 0,
                                                                   applied.insecticide.dose = 1,
                                                                   recommended.insecticide.dose = 1,
                                                                   threshold.generations = 15,
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
                                                                   z.sd.intercept = 18,
                                                                   z.sd.coefficient = 0.4,
                                                                   mixture.strategy = "mix.sequential.discrete",
                                                                   llin.insecticides,
                                                                   irs.insecticides,
                                                                   min.cross.selection = 0,
                                                                   max.cross.selection = 0,
                                                                   gonotrophic.cycle.length = 3,
                                                                   natural.daily.survival = 1)



ggplot(subset(mixtures.standard.rotation.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked))+
  geom_line(aes(x=time.in.generations,
                y = 11,
                colour = deployed.mixture.part.1,
                linewidth = 8, alpha = 0.5))+
  geom_line(aes(x=time.in.generations,
                y = 11.2,
                colour = deployed.mixture.part.2,
                linewidth = 8, alpha = 0.5))+
  geom_line(linewidth = 1.2, alpha = 0.5)+
  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
             colour = "grey")+
  geom_text(aes(x=150,
                y=11.4, label = paste0("Mixture Deployed")),
            colour = "black",
            size = 4.5)+
  geom_text(aes(x=50,
                y=10.2, label = paste0("Withdrawal Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=50,
                y=8.2, label = paste0("Return Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=500,
                y=5, label = paste0("500 Generation Maximum")),
            colour = "orange",
            angle = 270,
            size = 3.7)+
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"))+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Mixtures in Rotation")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 510))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12),
                     breaks = c(0, 2, 4, 6, 8, 10))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


#####
#####
micromosaics.sequence.simulation.df = run_simulation_advanced(irm.deployment.strategy = "micromosaics",
                                                                   irm.switch.strategy = "sequence",
                                                                   number.of.insecticides = 4,
                                                                   sd.scaled = FALSE,
                                                                   exposure.scaling.factor = 10,
                                                                   female.fitness.cost = 0.5,
                                                                   male.fitness.cost = 0.5,
                                                                   female.exposure = 0.7,
                                                                   male.exposure = 0.7,
                                                                   heritability = c(0.3, 0.2, 0.2, 0.3),
                                                                   dispersal.rate = 0.2,
                                                                   coverage = 0.9,
                                                                   standard.deviation = 50,
                                                                   vector.length = 1000,
                                                                   maximum.bioassay.survival.proportion = 1,
                                                                   michaelis.menten.slope = 1,
                                                                   regression.coefficient = 0.48,
                                                                   regression.intercept = 0.15,
                                                                   maximum.generations = 500,
                                                                   half.population.bioassay.survival.resistance = 900,
                                                                   withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                   return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                   deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                   maximum.resistance.value = 25000,
                                                                   starting.refugia.resistance.score = 0,
                                                                   starting.intervention.resistance.score = 0,
                                                                   applied.insecticide.dose = 1,
                                                                   recommended.insecticide.dose = 1,
                                                                   threshold.generations = 15,
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
                                                                   n.cycles = 3,
                                                                   intervention.coverage.1 = 0.5,
                                                                   intervention.coverage.2 = 0.5,
                                                                   intervention.coverage.1.2 = NA,
                                                                   z.sd.intercept = 18,
                                                                   z.sd.coefficient = 0.4,
                                                                   mixture.strategy = "mix.sequential.discrete",
                                                                   llin.insecticides,
                                                                   irs.insecticides,
                                                                   min.cross.selection = 0,
                                                                   max.cross.selection = 0,
                                                                   gonotrophic.cycle.length = 3,
                                                                   natural.daily.survival = 0.8)


ggplot(subset(micromosaics.sequence.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked))+
  geom_line(aes(x=time.in.generations,
                y = 11,
                colour = as.character(insecticide.deployed.1),
                linewidth = 8, alpha = 0.5))+
  geom_line(aes(x=time.in.generations,
                y = 11.2,
                colour = as.character(insecticide.deployed.2),
                linewidth = 8, alpha = 0.5))+
  geom_line(linewidth = 1.2, alpha = 0.5)+
  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
             colour = "grey")+
  geom_text(aes(x=150,
                y=11.4, label = paste0("Micromosaic Deployed")),
            colour = "black",
            size = 4.5)+
  geom_text(aes(x=50,
                y=10.2, label = paste0("Withdrawal Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=50,
                y=8.2, label = paste0("Return Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=500,
                y=5, label = paste0("500 Generation Maximum")),
            colour = "orange",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=110,
                y=5, label = paste0("Insecticide 1 Withdrawn")),
            colour = "#1b9e77",
            angle = 270,
            size = 3.7)+

  geom_text(aes(x=120,
                y=5, label = paste0("Insecticide 3 Deployed")),
            colour = "#7570b3",
            angle = 270,
            size = 3.7)+

  geom_text(aes(x=180,
                y=5, label = paste0("Insecticide 2 Withdrawn")),
            colour = "#d95f02",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=190,
                y=5, label = paste0("Insecticide 4 Deployed")),
            colour = "#e7298a",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=300,
                y=5, label = paste0("No Insecticides Available: SIMULATION TERMINATES")),
            colour = "red",
            angle = 270,
            size = 3.7)+
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"))+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Micro-Mosaics: Sequence")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 510))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12),
                     breaks = c(0, 2, 4, 6, 8, 10))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))



#####
micromosaics.full.rot.simulation.df = run_simulation_advanced(irm.deployment.strategy = "micromosaics",
                                                              irm.switch.strategy = "full.rotation",
                                                              number.of.insecticides = 4,
                                                              sd.scaled = FALSE,
                                                              exposure.scaling.factor = 10,
                                                              female.fitness.cost = c(0.1, 0.2, 0.1, 0.2),
                                                              male.fitness.cost = 0.1,
                                                              female.exposure = 0.7,
                                                              male.exposure = 0.7,
                                                              heritability = c(0.3, 0.2, 0.3, 0.2),
                                                              dispersal.rate = 0.2,
                                                              coverage = 1,
                                                              standard.deviation = 50,
                                                              vector.length = 100,
                                                              maximum.bioassay.survival.proportion = 1,
                                                              michaelis.menten.slope = 1,
                                                              regression.coefficient = 0.48,
                                                              regression.intercept = 0.15,
                                                              maximum.generations =500,
                                                              half.population.bioassay.survival.resistance = 900,
                                                              withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                              return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                              deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                              maximum.resistance.value = 25000,
                                                              starting.refugia.resistance.score = 0,
                                                              starting.intervention.resistance.score = 0,
                                                              applied.insecticide.dose = 1,
                                                              recommended.insecticide.dose = 1,
                                                              threshold.generations = 15,
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
                                                              n.cycles = 2,
                                                              intervention.coverage.1 = 0.5,
                                                              intervention.coverage.2 = 0.5,
                                                              intervention.coverage.1.2 = 0,
                                                              z.sd.intercept = 18,
                                                              z.sd.coefficient = 0.4,
                                                              mixture.strategy = NA,
                                                              llin.insecticides,
                                                              irs.insecticides,
                                                              min.cross.selection = 0,
                                                              max.cross.selection = 0,
                                                              gonotrophic.cycle.length = 3,
                                                              natural.daily.survival = 0.8)


ggplot(subset(micromosaics.full.rot.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked))+
  geom_line(aes(x=time.in.generations,
                y = 11,
                colour = as.character(insecticide.deployed.1),
                linewidth = 8, alpha = 0.5))+
  geom_line(aes(x=time.in.generations,
                y = 11.2,
                colour = as.character(insecticide.deployed.2),
                linewidth = 8, alpha = 0.5))+
  geom_line(linewidth = 1.2, alpha = 0.5)+
  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
             colour = "grey")+
  geom_text(aes(x=150,
                y=11.4, label = paste0("Micromosaic Deployed")),
            colour = "black",
            size = 4.5)+
  geom_text(aes(x=55,
                y=10.2, label = paste0("Withdrawal Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=50,
                y=8.2, label = paste0("Return Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=500,
                y=5, label = paste0("500 Generation Maximum")),
            colour = "orange",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=215,
                y=5, label = paste0("Insecticides 1 and 2 Withdrawn")),
            colour = "black",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=225,
                y=5, label = paste0("Rotation Unavailable: SIMULATION TERMINATES")),
            colour = "red",
            angle = 270,
            size = 3.7)+
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"))+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Micro-Mosaics: Full Rotation")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 510))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12),
                     breaks = c(0, 2, 4, 6, 8, 10))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))



#####
micromosaics.rot.exp.simulation.df = run_simulation_advanced(irm.deployment.strategy = "micromosaics",
                                                              irm.switch.strategy = "rotate.expensive",
                                                              number.of.insecticides = 4,
                                                              sd.scaled = FALSE,
                                                              exposure.scaling.factor = 10,
                                                              female.fitness.cost = 0.1,
                                                              male.fitness.cost = 0.1,
                                                              female.exposure = 0.7,
                                                              male.exposure = 0.7,
                                                              heritability = 0.3,
                                                              dispersal.rate = 0.2,
                                                              coverage = 0.7,
                                                              standard.deviation = 50,
                                                              vector.length = 100,
                                                              maximum.bioassay.survival.proportion = 1,
                                                              michaelis.menten.slope = 1,
                                                              regression.coefficient = 0.48,
                                                              regression.intercept = 0.15,
                                                              maximum.generations =500,
                                                              half.population.bioassay.survival.resistance = 900,
                                                              withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                              return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                              deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                              maximum.resistance.value = 25000,
                                                              starting.refugia.resistance.score = 0,
                                                              starting.intervention.resistance.score = 0,
                                                              applied.insecticide.dose = 1,
                                                              recommended.insecticide.dose = 1,
                                                              threshold.generations = 15,
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
                                                              n.cycles = 2,
                                                              intervention.coverage.1 = 0.5,
                                                              intervention.coverage.2 = 0.5,
                                                              intervention.coverage.1.2 = 0,
                                                              z.sd.intercept = 18,
                                                              z.sd.coefficient = 0.4,
                                                              mixture.strategy = NA,
                                                              llin.insecticides,
                                                              irs.insecticides,
                                                              min.cross.selection = 0,
                                                              max.cross.selection = 0,
                                                              gonotrophic.cycle.length = 3,
                                                              natural.daily.survival = 0.8)


ggplot(subset(micromosaics.rot.exp.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked))+
  geom_line(aes(x=time.in.generations,
                y = 11,
                colour = as.character(insecticide.deployed.1),
                linewidth = 8, alpha = 0.5))+
  geom_line(aes(x=time.in.generations,
                y = 11.2,
                colour = as.character(insecticide.deployed.2),
                linewidth = 8, alpha = 0.5))+
  geom_line(linewidth = 1.2, alpha = 0.5)+
  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
             colour = "grey")+
geom_text(aes(x=150,
              y=11.4, label = paste0("Micromosaic Deployed")),
          colour = "black",
          size = 4.5)+
geom_text(aes(x=50,
              y=10.2, label = paste0("Withdrawal Threshold")),
          colour = "black",
          size = 4)+
geom_text(aes(x=50,
              y=8.2, label = paste0("Return Threshold")),
          colour = "black",
          size = 4)+
geom_text(aes(x=500,
              y=5, label = paste0("500 Generation Maximum")),
          colour = "orange",
          angle = 270,
          size = 3.7)+
geom_text(aes(x=230,
              y=5, label = paste0("Insecticide 1 Withdrawn")),
          colour = "#1b9e77",
          angle = 270,
          size = 3.7)+
geom_text(aes(x=240,
              y=5, label = paste0("SIMULATION TERMINATES")),
          colour = "red",
          angle = 270,
          size = 3.7)+
scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"))+
xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Micro-Mosaics: Partial Rotation")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 510))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12),
                     breaks = c(0, 2, 4, 6, 8, 10))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))



#####
combinations.sequence.irs.simulation.df = run_simulation_advanced(irm.deployment.strategy = "combinations",
                                                              irm.switch.strategy = "sequence.irs",
                                                              number.of.insecticides = 3,
                                                              sd.scaled = FALSE,
                                                              exposure.scaling.factor = 10,
                                                              female.fitness.cost = 0,
                                                              male.fitness.cost = 0,
                                                              female.exposure = 0.7,
                                                              male.exposure = 0.7,
                                                              heritability = 0.3,
                                                              dispersal.rate = 0.2,
                                                              coverage = 0.9,
                                                              standard.deviation = 50,
                                                              vector.length = 1000,
                                                              maximum.bioassay.survival.proportion = 1,
                                                              michaelis.menten.slope = 1,
                                                              regression.coefficient = 0.48,
                                                              regression.intercept = 0.15,
                                                              maximum.generations = 500,
                                                              half.population.bioassay.survival.resistance = 900,
                                                              withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                              return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                              deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                              maximum.resistance.value = 25000,
                                                              starting.refugia.resistance.score = 0,
                                                              starting.intervention.resistance.score = 0,
                                                              applied.insecticide.dose = 1,
                                                              recommended.insecticide.dose = 1,
                                                              threshold.generations = 15,
                                                              base.efficacy.decay.rate = 0,
                                                              rapid.decay.rate = 0,
                                                              deployment.interval.llin = 10, #only for combinations
                                                              deployment.interval.irs = 10, #only for combinations
                                                              probability.only.i.male = 0.2, #only for combinations
                                                              probability.only.j.male = 0.1, #only for combinations
                                                              probability.both.i.j.male = 0.7, #only for combinations
                                                              probability.only.i.female = 0.2, #only for combinations
                                                              probability.only.j.female = 0.1, #only for combinations
                                                              probability.both.i.j.female = 0.7, #only for combinations
                                                              n.cycles = 3,
                                                              intervention.coverage.1 = 0.2,
                                                              intervention.coverage.2 = 0.2,
                                                              intervention.coverage.1.2 = 0.6,
                                                              z.sd.intercept = 18,
                                                              z.sd.coefficient = 1,
                                                              mixture.strategy = "mix.sequential.discrete",
                                                              llin.insecticides = 1,
                                                              irs.insecticides = c(2, 3),
                                                              min.cross.selection = 0,
                                                              max.cross.selection = 0,
                                                              gonotrophic.cycle.length = 3,
                                                              natural.daily.survival = 0.8)



ggplot(subset(combinations.sequence.irs.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked))+

  geom_line(linewidth = 1.2, alpha = 0.5)+
  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
             colour = "grey")+
    geom_line(aes(x=time.in.generations,
                y = 24,
                colour = as.character(llin.insecticide.deployed),
                linewidth = 8, alpha = 0.5))+
  geom_text(aes(x=150,
                y=24.5, label = paste0("LLIN Insecticide Deployed")),
            colour = "black",
            size = 4.5)+
  geom_line(aes(x=time.in.generations,
                y = 23,
                colour = as.character(irs.insecticide.deployed),
                linewidth = 8,
                alpha = 0.5))+
  geom_text(aes(x=150,
                y=23.5, label = paste0("IRS Insecticide Deployed")),
            colour = "black",
            size = 4.5)+
  geom_text(aes(x=50,
                y=10.2, label = paste0("Withdrawal Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=50,
                y=8.2, label = paste0("Return Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=500,
                y=10, label = paste0("500 Generation Maximum")),
            colour = "orange",
            angle = 270,
            size = 3.7)+
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"))+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Combinations: Sequence IRS")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 510))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25),
                     breaks = c(0, 2, 4, 6, 8, 10))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))





######
#####

combinations.rotate.irs.simulation.df = run_simulation_advanced(irm.deployment.strategy = "combinations",
                                                                  irm.switch.strategy = "rotate.irs",
                                                                  number.of.insecticides = 3,
                                                                  sd.scaled = FALSE,
                                                                  exposure.scaling.factor = 10,
                                                                  female.fitness.cost = 0,
                                                                  male.fitness.cost = 0,
                                                                  female.exposure = 0.7,
                                                                  male.exposure = 0.7,
                                                                  heritability = 0.3,
                                                                  dispersal.rate = 0.2,
                                                                  coverage = 0.9,
                                                                  standard.deviation = 50,
                                                                  vector.length = 1000,
                                                                  maximum.bioassay.survival.proportion = 1,
                                                                  michaelis.menten.slope = 1,
                                                                  regression.coefficient = 0.48,
                                                                  regression.intercept = 0.15,
                                                                  maximum.generations = 500,
                                                                  half.population.bioassay.survival.resistance = 900,
                                                                  withdrawal.threshold.value = 0.1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                  return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                  deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                  maximum.resistance.value = 25000,
                                                                  starting.refugia.resistance.score = 0,
                                                                  starting.intervention.resistance.score = 0,
                                                                  applied.insecticide.dose = 1,
                                                                  recommended.insecticide.dose = 1,
                                                                  threshold.generations = 15,
                                                                  base.efficacy.decay.rate = 0,
                                                                  rapid.decay.rate = 0,
                                                                  deployment.interval.llin = 10, #only for combinations
                                                                  deployment.interval.irs = 10, #only for combinations
                                                                  probability.only.i.male = 0.2, #only for combinations
                                                                  probability.only.j.male = 0.1, #only for combinations
                                                                  probability.both.i.j.male = 0.7, #only for combinations
                                                                  probability.only.i.female = 0.2, #only for combinations
                                                                  probability.only.j.female = 0.1, #only for combinations
                                                                  probability.both.i.j.female = 0.7, #only for combinations
                                                                  n.cycles = 3,
                                                                  intervention.coverage.1 = 0.2,
                                                                  intervention.coverage.2 = 0.2,
                                                                  intervention.coverage.1.2 = 0.6,
                                                                  z.sd.intercept = 18,
                                                                  z.sd.coefficient = 1,
                                                                  mixture.strategy = "mix.sequential.discrete",
                                                                  llin.insecticides = 1,
                                                                  irs.insecticides = c(2, 3),
                                                                  min.cross.selection = 0,
                                                                  max.cross.selection = 0,
                                                                  gonotrophic.cycle.length = 3,
                                                                  natural.daily.survival = 0.8)



ggplot(subset(combinations.rotate.irs.simulation.df, site == "intervention"),
       aes(x=time.in.generations,
           y = bioassay.survival*100,
           group = insecticide.tracked,
           colour = insecticide.tracked))+

  geom_line(linewidth = 1.2, alpha = 0.5)+
  geom_hline(yintercept = 10,
             linetype = "dashed")+
  geom_hline(yintercept = 8,
             linetype = "dashed",
             colour = "grey")+
  geom_line(aes(x=time.in.generations,
                y = 24,
                colour = as.character(llin.insecticide.deployed),
                linewidth = 8, alpha = 0.5))+
  geom_text(aes(x=150,
                y=24.5, label = paste0("LLIN Insecticide Deployed")),
            colour = "black",
            size = 4.5)+
  geom_line(aes(x=time.in.generations,
                y = 23,
                colour = as.character(irs.insecticide.deployed),
                linewidth = 8,
                alpha = 0.5))+
  geom_text(aes(x=150,
                y=23.5, label = paste0("IRS Insecticide Deployed")),
            colour = "black",
            size = 4.5)+
  geom_text(aes(x=50,
                y=10.2, label = paste0("Withdrawal Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=50,
                y=8.2, label = paste0("Return Threshold")),
            colour = "black",
            size = 4)+
  geom_text(aes(x=500,
                y=10, label = paste0("500 Generation Maximum")),
            colour = "orange",
            angle = 270,
            size = 3.7)+
  geom_text(aes(x=330,
                y=12, label = paste0("No IRS Insecticides Available: SIMULATION TERMINATES")),
            colour = "red",
            angle = 270,
            size = 3.7)+
  scale_colour_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"))+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Combinations: Rotate IRS")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 510))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25),
                     breaks = c(0, 2, 4, 6, 8, 10))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

