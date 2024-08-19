#Example Simulations to Understand Model Interpretation::
library(devtools)
load_all()

novel.solo = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                        irm.switch.strategy = "sequence", #"rotation", "sequence", "novel.sequence"
                        mixture.strategy = NA,
                        number.of.insecticides = 1,
                        sd.scaled = TRUE, ##TRUE or FALSE
                        exposure.scaling.factor = 10,
                        female.fitness.cost = 0,
                        male.fitness.cost = 0,
                        female.exposure = 0.7,
                        male.exposure = 0.7,
                        heritability = 0.3,
                        dispersal.rate = 0.2,
                        coverage = 0.8,
                        standard.deviation = 50,
                        vector.length = 200,
                        maximum.bioassay.survival.proportion = 1,
                        michaelis.menten.slope = 1,
                        regression.coefficient = 0.48,
                        regression.intercept = 0.15,
                        maximum.generations = 200,
                        half.population.bioassay.survival.resistance = 900,
                        withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                        return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                        deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                        maximum.resistance.value = 90000,
                        starting.refugia.resistance.score = 0,
                        starting.intervention.resistance.score = 0,
                        applied.insecticide.dose = 1,
                        recommended.insecticide.dose = 1,
                        threshold.generations = 20,
                        base.efficacy.decay.rate = 0.015,
                        rapid.decay.rate = 0.08,
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
                        z.sd.coefficient = 0.4,
                        z.sd.intercept = 18,
                        llin.insecticides = NA,
                        irs.insecticides = NA,
                        min.cross.selection = 0,
                        max.cross.selection = 0)


pyr.solo = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                                     irm.switch.strategy = "sequence", #"rotation", "sequence", "novel.sequence"
                                     mixture.strategy = NA,
                                     number.of.insecticides = 1,
                                     sd.scaled = TRUE, ##TRUE or FALSE
                                     exposure.scaling.factor = 10,
                                     female.fitness.cost = 0,
                                     male.fitness.cost = 0,
                                     female.exposure = 0.7,
                                     male.exposure = 0.7,
                                     heritability = 0.3,
                                     dispersal.rate = 0.2,
                                     coverage = 0.8,
                                     standard.deviation = 50,
                                     vector.length = 200,
                                     maximum.bioassay.survival.proportion = 1,
                                     michaelis.menten.slope = 1,
                                     regression.coefficient = 0.48,
                                     regression.intercept = 0.15,
                                     maximum.generations = 200,
                                     half.population.bioassay.survival.resistance = 900,
                                     withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                     return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                     deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                     maximum.resistance.value = 90000,
                                     starting.refugia.resistance.score = 100,
                                     starting.intervention.resistance.score = 100,
                                     applied.insecticide.dose = 1,
                                     recommended.insecticide.dose = 1,
                                     threshold.generations = 20,
                                     base.efficacy.decay.rate = 0.015,
                                     rapid.decay.rate = 0.08,
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
                                     z.sd.coefficient = 0.4,
                                     z.sd.intercept = 18,
                                     llin.insecticides = NA,
                                     irs.insecticides = NA,
                                     min.cross.selection = 0,
                                     max.cross.selection = 0)



mix.hdhd = run_simulation_advanced(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                   irm.switch.strategy = "sequence", #"rotation", "sequence", "novel.sequence"
                                   mixture.strategy = "mix.sequential.discrete",
                                   number.of.insecticides = 2,
                                   sd.scaled = TRUE, ##TRUE or FALSE
                                   exposure.scaling.factor = 10,
                                   female.fitness.cost = 0,
                                   male.fitness.cost = 0,
                                   female.exposure =0.7,
                                   male.exposure = 0.7,
                                   heritability = 0.3,
                                   dispersal.rate = 0.2,
                                   coverage = 0.8,
                                   standard.deviation = 50,
                                   vector.length = 200,
                                   maximum.bioassay.survival.proportion = 1,
                                   michaelis.menten.slope = 1,
                                   regression.coefficient = 0.48,
                                   regression.intercept = 0.15,
                                   maximum.generations = 200,
                                   half.population.bioassay.survival.resistance = 900,
                                   withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                   return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                   deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                   maximum.resistance.value = 90000,
                                   starting.refugia.resistance.score = c(0, 100),
                                   starting.intervention.resistance.score = c(0, 100),
                                   applied.insecticide.dose = c(0.5, 0.5),
                                   recommended.insecticide.dose = 1,
                                   threshold.generations = 20,
                                   base.efficacy.decay.rate = c(0.015, 0.015),
                                   rapid.decay.rate = 0.08,
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
                                   z.sd.coefficient = 0.4,
                                   z.sd.intercept = 18,
                                   llin.insecticides = NA,
                                   irs.insecticides = NA,
                                   min.cross.selection = 0,
                                   max.cross.selection = 0)


mix.hdhd.novel = subset(mix.hdhd, site == "intervention" & insecticide.tracked == 1)

mix.hdhd.pyr = subset(mix.hdhd, site == "intervention" & insecticide.tracked == 2)


novel.solo = subset(novel.solo, site == "intervention")
pyr.solo = subset(pyr.solo, site == "intervention")


sim.plot = ggplot(novel.solo, aes(x=time.in.generations/10, y = bioassay.survival*100))+
    geom_ribbon(aes(ymin=mix.hdhd.pyr$bioassay.survival *100,ymax=pyr.solo$bioassay.survival * 100),
              fill="blue", alpha=0.2) +

  geom_ribbon(aes(ymin=mix.hdhd.novel$bioassay.survival *100,ymax=novel.solo$bioassay.survival * 100),
              fill="red", alpha=0.2) +
  geom_line(colour = "coral", linewidth = 2)+
  geom_line(data = pyr.solo, mapping = aes(x=time.in.generations/10,
                                           y = bioassay.survival*100),
            colour = "skyblue", linewidth = 2)+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  xlab("Time in Years")+
  ylab("Bioassay Survival (%)")+
  geom_line(data = mix.hdhd.pyr, mapping = aes(x=time.in.generations/10,
                                           y = bioassay.survival*100),
            colour = "darkblue", linewidth = 2)+
  geom_line(data = mix.hdhd.novel, mapping = aes(x=time.in.generations/10,
                                           y = bioassay.survival*100),
            colour = "darkred", linewidth = 2)+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(vjust = 1.5,
                                   hjust = 0.7))

legend.df = data.frame(
description = c("Pyrethroid\nSolo",
                "Novel\nSolo",
                "Pyrethroid\nin Mixture",
                "Novel\nin Mixture"),
x.vals = 1:4
)

legend.plot = ggplot(legend.df, aes(x=x.vals, y = 1,
                      fill = description))+
  geom_tile()+
  geom_text(aes(label = description,
                colour = description), size = 3)+
  scale_fill_manual(values = c("darkred",
                               "coral",
                               "darkblue",
                               "skyblue"))+
  scale_colour_manual(values = c("white",
                                 "black",
                                 "white",
                                 "black"))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")


library(patchwork)

the.layout = "
A
A
A
A
A
A
A
A
A
A
A
A
A
B"

sim.plot + legend.plot + plot_layout(design = the.layout)

ggsave(filename = "example_scenario1_simulation.jpeg",
       plot = last_plot(),
       dpi = 600,
       scale = 5,
       height = 600,
       width = 600,
       units = "px")

















































