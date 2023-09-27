###How to Set up Simulations for Combinations...

###Coverages:
c.LLIN = rep(rep(c(0, 0, 0, 0, 0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.75, 0.75), 13), 3)
c.IRS = rep(rep(c(0, 0.25, 0.5, 0.75, 0, 0.25, 0.5, 0.75, 0, 0.25, 0.5, 0, 0.25), 13), 3)
c.LLIN.IRS = rep(rep(c(1, 0.75, 0.5, 0.25, 0.75, 0.5, 0.25, 0, 0.5, 0.25, 0, 0.25, 0), 13), 3)


#Encounters:
e.LLIN = rep(c(rep(0, 13),
           rep(0.25, 13),
           rep(0.5, 13),
           rep(0.75, 13),
           rep(0, 13),
           rep(0, 13),
           rep(0, 13),
           rep(0.25, 13),
           rep(0.5, 13),
           rep(0.25, 13),
           rep(0.25, 13),
           rep(0.5, 13),
           rep(0.75, 13)
  ), 3)

e.IRS = rep(c(rep(0, 13),
          rep(0, 13),
          rep(0, 13),
          rep(0, 13),
          rep(0.25, 13),
          rep(0.5, 13),
          rep(0.75, 13),
          rep(0.25, 13),
          rep(0.5, 13),
          rep(0.75, 13),
          rep(0.5, 13),
          rep(0.25, 13),
          rep(0.25, 13)
), 3)

e.LLIN.IRS = rep(c(rep(1, 13),
               rep(0.75, 13),
               rep(0.5, 13),
               rep(0.25, 13),
               rep(0.75, 13),
               rep(0.5, 13),
               rep(0.25, 13),
               rep(0.5, 13),
               rep(0, 13),
               rep(0, 13),
               rep(0.25, 13),
               rep(0.25, 13),
               rep(0, 13)
), 3)


gonotrophic.cycles = rep(c(rep(1, 169), rep(3, 169), rep(5, 169)))





sim.list = list()
for(i in 1:507){

sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                        irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                        number.of.insecticides = 2,
                        sd.scaled = FALSE, ##TRUE or FALSE
                        exposure.scaling.factor = 10,
                        female.fitness.cost = 0,
                        male.fitness.cost = 0,
                        female.exposure = 0.7,
                        male.exposure = 0.7,
                        heritability = 0.3,
                        dispersal.rate = 0.3,
                        coverage = 0.7,
                        standard.deviation = 50,
                        vector.length = 1000,
                        maximum.bioassay.survival.proportion = 1,
                        michaelis.menten.slope = 1,
                        regression.coefficient = 0.48,
                        regression.intercept = 0.15,
                        maximum.generations = 100,
                        half.population.bioassay.survival.resistance = 900,
                        withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                        return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                        deployment.frequency = 100, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                        maximum.resistance.value = 25000,
                        starting.refugia.resistance.score = 0,
                        starting.intervention.resistance.score = 0,
                        applied.insecticide.dose = 1,
                        recommended.insecticide.dose = 1,
                        threshold.generations = 10,
                        base.efficacy.decay.rate = 0,
                        rapid.decay.rate = 0,
                        deployment.interval.llin = 100, #only for combinations
                        deployment.interval.irs = 100, #only for combinations
                        probability.only.i.male = e.LLIN[i], #only for combinations
                        probability.only.j.male = e.IRS[i], #only for combinations
                        probability.both.i.j.male = e.LLIN.IRS[i], #only for combinations
                        probability.only.i.female = e.LLIN[i], #only for combinations
                        probability.only.j.female =  e.IRS[i], #only for combinations
                        probability.both.i.j.female = e.LLIN.IRS[i], #only for combinations
                        n.cycles = gonotrophic.cycles[i],
                        intervention.coverage.1 = c.LLIN[i],
                        intervention.coverage.2 = c.IRS[i],
                        intervention.coverage.1.2 = c.LLIN.IRS[i],
                        z.sd.intercept = 18,
                        z.sd.coefficient = 0.4,
                        mixture.strategy = "pyrethroid.plus",
                        llin.insecticides = 1,
                        irs.insecticides = 2,
                        min.cross.selection = 0,
                        max.cross.selection = 0,
                        gonotrophic.cycle.length = 3,
                        natural.daily.survival = 0.8)

print(i)
}


peak.llin = c()
peak.irs = c()
for(i in 1:507){

 peak.llin[i] = max(subset(sim.list[[i]], insecticide.tracked == 1)$bioassay.survival)
 peak.irs[i] = max(subset(sim.list[[i]], insecticide.tracked == 2)$bioassay.survival)



}


sim.list = list()
for(i in 1:507){

  sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                          irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                                          number.of.insecticides = 2,
                                          sd.scaled = FALSE, ##TRUE or FALSE
                                          exposure.scaling.factor = 10,
                                          female.fitness.cost = 0,
                                          male.fitness.cost = 0,
                                          female.exposure = 0.7,
                                          male.exposure = 0.7,
                                          heritability = 0.3,
                                          dispersal.rate = 0.3,
                                          coverage = 0.7,
                                          standard.deviation = 50,
                                          vector.length = 1000,
                                          maximum.bioassay.survival.proportion = 1,
                                          michaelis.menten.slope = 1,
                                          regression.coefficient = 0.48,
                                          regression.intercept = 0.15,
                                          maximum.generations = 100,
                                          half.population.bioassay.survival.resistance = 900,
                                          withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                          return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                          deployment.frequency = 100, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                          maximum.resistance.value = 25000,
                                          starting.refugia.resistance.score = 0,
                                          starting.intervention.resistance.score = 0,
                                          applied.insecticide.dose = c(1, 0),
                                          recommended.insecticide.dose = 1,
                                          threshold.generations = 10,
                                          base.efficacy.decay.rate = 0,
                                          rapid.decay.rate = 0,
                                          deployment.interval.llin = 100, #only for combinations
                                          deployment.interval.irs = 100, #only for combinations
                                          probability.only.i.male = 1, #only for combinations
                                          probability.only.j.male = 0, #only for combinations
                                          probability.both.i.j.male = 0, #only for combinations
                                          probability.only.i.female = 1, #only for combinations
                                          probability.only.j.female =  0, #only for combinations
                                          probability.both.i.j.female = 0, #only for combinations
                                          n.cycles = gonotrophic.cycles[i],
                                          intervention.coverage.1 = c.LLIN[i],
                                          intervention.coverage.2 = c.IRS[i],
                                          intervention.coverage.1.2 = c.LLIN.IRS[i],
                                          z.sd.intercept = 18,
                                          z.sd.coefficient = 0.4,
                                          mixture.strategy = "pyrethroid.plus",
                                          llin.insecticides = 1,
                                          irs.insecticides = 2,
                                          min.cross.selection = 0,
                                          max.cross.selection = 0,
                                          gonotrophic.cycle.length = 3,
                                          natural.daily.survival = 0.8)

  print(i)
}

solo.peak.llin = c()
solo.peak.irs = c()
for(i in 1:507){

  solo.peak.llin[i] = max(subset(sim.list[[i]], insecticide.tracked == 1)$bioassay.survival)
  solo.peak.irs[i] = max(subset(sim.list[[i]], insecticide.tracked == 2)$bioassay.survival)



}

range(solo.peak.llin)


df = data.frame(peak.llin, peak.irs, e.LLIN, e.IRS, e.LLIN.IRS,
                c.LLIN, c.IRS, c.LLIN.IRS, gonotrophic.cycles, solo.peak.llin)


df$coverage.llin = df$c.LLIN + df$c.LLIN.IRS
df$coverage.irs = df$c.IRS + c.LLIN.IRS
df$encounter.llin = df$e.LLIN + df$e.LLIN.IRS
df$encounter.irs = df$e.IRS + df$e.LLIN.IRS


df$llin.percentage.change = (((peak.llin- solo.peak.llin)/solo.peak.llin)*100)

range(df$llin.percentage.change)

ggplot(df, aes(x=peak.irs, y=peak.llin- solo.peak.llin,
               colour = as.factor(e.LLIN.IRS),
               shape = as.factor(e.LLIN)))+
  xlab("Proportion IRS Coverage Only")+
  ylab("Proportion LLIN Coverage Only")+
  geom_point()+
  facet_grid(c.IRS~c.LLIN)


g.1 = ggplot(subset(df, gonotrophic.cycles == 1), aes(x=encounter.irs,
               y=encounter.llin,
               fill = llin.percentage.change))+
  geom_tile(colour = "black")+
  scale_fill_viridis_c(limits = c(-100, 30))+
  facet_grid(coverage.llin ~ coverage.irs, drop = TRUE)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Coverage LLIN", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Coverage IRS", breaks = NULL, labels = NULL))+
  theme_bw()+
  xlab("Encounter IRS")+
  ylab("Encounter LLIN")+
  #guides(fill=guide_legend(title="Percentage Difference"))+
  ggtitle("1 gonotrophic cycle")+
  theme(legend.position = "none")


g.3 = ggplot(subset(df, gonotrophic.cycles == 3), aes(x=encounter.irs,
                                                      y=encounter.llin,
                                                      fill = llin.percentage.change))+
  geom_tile(colour = "black")+
  scale_fill_viridis_c(limits = c(-100, 30))+
  facet_grid(coverage.llin ~ coverage.irs, drop = TRUE)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Coverage LLIN", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Coverage IRS", breaks = NULL, labels = NULL))+
  theme_bw()+
 # guides(fill=guide_legend(title="Percentage Difference"))+
  xlab("Encounter IRS")+
  ylab("Encounter LLIN")+
  ggtitle("3 gonotrophic cycles")+
  theme(legend.position = "none")


g.5 = ggplot(subset(df, gonotrophic.cycles == 5), aes(x=encounter.irs,
                                                      y=encounter.llin,
                                                      fill = llin.percentage.change))+
  geom_tile(colour = "black")+
  scale_fill_viridis_c(limits = c(-100, 30))+
  facet_grid(coverage.llin ~ coverage.irs, drop = TRUE)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Coverage LLIN", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Coverage IRS", breaks = NULL, labels = NULL))+
  theme_bw()+
  #guides(fill=guide_legend(title="Percentage Difference"))+
  xlab("Encounter IRS")+
  ylab("Encounter LLIN")+
  ggtitle("5 gonotrophic cycles")+
  theme(legend.position = "none")

figure.legend = cowplot::get_legend(ggplot(subset(df, gonotrophic.cycles == 5), aes(x=encounter.irs,
                                                                    y=encounter.llin,
                                                                    fill = llin.percentage.change))+
                      geom_tile(colour = "black")+
                      scale_fill_viridis_c(limits = c(-100, 30))+
                      facet_grid(coverage.llin ~ coverage.irs, drop = TRUE)+
                      scale_y_continuous(sec.axis = sec_axis(~ . , name = "Coverage LLIN", breaks = NULL, labels = NULL)) +
                      scale_x_continuous(sec.axis = sec_axis(~ . , name = "Coverage IRS", breaks = NULL, labels = NULL))+
                      theme_bw()+
                      guides(fill=guide_legend(title="Percentage Difference"))+
                      xlab("Encounter IRS")+
                      ylab("Encounter LLIN")+
                      ggtitle("5 gonotrophic cycles")+
                      theme(legend.position = "bottom"))

library(patchwork)
the.layout = "
AAABBBCCC
AAABBBCCC
AAABBBCCC
###DDD###
"

g.1 + g.3 + g.5 + figure.legend + plot_layout(design = the.layout)



hist((peak.llin - solo.peak.llin))
