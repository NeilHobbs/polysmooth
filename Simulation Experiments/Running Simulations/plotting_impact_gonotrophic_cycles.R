#Importance of Multiple Gonotrophic Cycles and Natural Mortality for Micro-Mosaics vs Rotations:
library(devtools)
load_all()
library(patchwork)
quick_micro_mosaic = function(n.cycles,
                              gonotrophic.cycle.length,
                              natural.daily.survival,
                              female.exposure,
                              male.exposure,
                              heritability,
                              dispersal.rate,
                              coverage,
                              fitness,
                              threshold){

  A = run_simulation_advanced(irm.deployment.strategy = "micromosaics", #singles, mixtures, micromosaics, combinations
                              irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                              number.of.insecticides = 2,
                              sd.scaled = FALSE, ##TRUE or FALSE
                              exposure.scaling.factor = 10,
                              female.fitness.cost = fitness,
                              male.fitness.cost = fitness,
                              female.exposure = female.exposure,
                              male.exposure = male.exposure,
                              heritability = heritability,
                              dispersal.rate = dispersal.rate,
                              coverage = coverage,
                              standard.deviation = 50,
                              vector.length = 250,
                              maximum.bioassay.survival.proportion = 1,
                              michaelis.menten.slope = 1,
                              regression.coefficient = 0.48,
                              regression.intercept = 0.15,
                              maximum.generations = 500,
                              half.population.bioassay.survival.resistance = 900,
                              withdrawal.threshold.value = threshold,
                              return.threshold.value = 0.05,
                              deployment.frequency = 10,
                              maximum.resistance.value = 25000,
                              starting.refugia.resistance.score = 0,
                              starting.intervention.resistance.score = 0,
                              applied.insecticide.dose = 1,
                              recommended.insecticide.dose = 1,
                              threshold.generations = 15,
                              base.efficacy.decay.rate = 0.015,
                              rapid.decay.rate = 0.08,
                              deployment.interval.llin = 30,
                              deployment.interval.irs = 10,
                              probability.only.i.male = 0.7,
                              probability.only.j.male = 0.2,
                              probability.both.i.j.male = 0.1,
                              probability.only.i.female = 0.4,
                              probability.only.j.female = 0.2,
                              probability.both.i.j.female = 0.4,
                              n.cycles = n.cycles,
                              intervention.coverage.1 = 0.5,
                              intervention.coverage.2 = 0.5,
                              intervention.coverage.1.2 = 0,
                              z.sd.intercept = 18,
                              z.sd.coefficient = 0.4,
                              mixture.strategy = "pyrethroid.plus",
                              llin.insecticides = NA,
                              irs.insecticides = NA,
                              min.cross.selection = 0,
                              max.cross.selection = 0,
                              gonotrophic.cycle.length = gonotrophic.cycle.length,
                              natural.daily.survival = natural.daily.survival)

  return(A)
}



quick_rotations = function(n.cycles,
                           gonotrophic.cycle.length,
                           natural.daily.survival,
                           female.exposure,
                           male.exposure,
                           heritability,
                           dispersal.rate,
                           coverage,
                           threshold,
                           fitness){

  A = run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
                              irm.switch.strategy = "rotation", #"rotation", "sequence", "insecticide.1"
                              number.of.insecticides = 2,
                              sd.scaled = FALSE, ##TRUE or FALSE
                              exposure.scaling.factor = 10,
                              female.fitness.cost = fitness,
                              male.fitness.cost = fitness,
                              female.exposure = female.exposure,
                              male.exposure = male.exposure,
                              heritability = heritability,
                              dispersal.rate = dispersal.rate,
                              coverage = coverage,
                              standard.deviation = 50,
                              vector.length = 250,
                              maximum.bioassay.survival.proportion = 1,
                              michaelis.menten.slope = 1,
                              regression.coefficient = 0.48,
                              regression.intercept = 0.15,
                              maximum.generations = 500,
                              half.population.bioassay.survival.resistance = 900,
                              withdrawal.threshold.value = threshold,
                              return.threshold.value = 0.05,
                              deployment.frequency = 10,
                              maximum.resistance.value = 25000,
                              starting.refugia.resistance.score = 0,
                              starting.intervention.resistance.score = 0,
                              applied.insecticide.dose = 1,
                              recommended.insecticide.dose = 1,
                              threshold.generations = 15,
                              base.efficacy.decay.rate = 0.015,
                              rapid.decay.rate = 0.08,
                              deployment.interval.llin = 30,
                              deployment.interval.irs = 10,
                              probability.only.i.male = 0.7,
                              probability.only.j.male = 0.2,
                              probability.both.i.j.male = 0.1,
                              probability.only.i.female = 0.4,
                              probability.only.j.female = 0.2,
                              probability.both.i.j.female = 0.4,
                              n.cycles = n.cycles,
                              intervention.coverage.1 = 0.5,
                              intervention.coverage.2 = 0.5,
                              intervention.coverage.1.2 = 0,
                              z.sd.intercept = 18,
                              z.sd.coefficient = 0.4,
                              mixture.strategy = "pyrethroid.plus",
                              llin.insecticides = NA,
                              irs.insecticides = NA,
                              min.cross.selection = 0,
                              max.cross.selection = 0,
                              gonotrophic.cycle.length = gonotrophic.cycle.length,
                              natural.daily.survival = natural.daily.survival)

  return(A)
}


mm.single.g = subset(quick_micro_mosaic(n.cycles = 1,
                                        gonotrophic.cycle.length = 3,
                                        natural.daily.survival = 1,
                                        female.exposure = 0.7,
                                        male.exposure = 0.5,
                                        heritability = 0.3,
                                        dispersal.rate = 0.2,
                                        coverage = 0.8,
                                        threshold = 1,
                                        fitness = 0.4), site == "intervention" &
                       insecticide.tracked == 2)
rotation.single.g = subset(quick_rotations(n.cycles = 1,
                                           gonotrophic.cycle.length = 3,
                                           natural.daily.survival = 1,
                                           female.exposure = 0.7,
                                           male.exposure = 0.5,
                                           heritability = 0.3,
                                           dispersal.rate = 0.2,
                                           coverage = 0.8,
                                           threshold = 1,
                                           fitness= 0.4), site == "intervention"&
                             insecticide.tracked == 2)


mm.single.g.duration = max(quick_micro_mosaic(n.cycles = 1,
                                              gonotrophic.cycle.length = 3,
                                              natural.daily.survival = 1,
                                              female.exposure = 0.7,
                                              male.exposure = 0.5,
                                              heritability = 0.3,
                                              dispersal.rate = 0.2,
                                              coverage = 0.8,
                                              threshold = 0.1,
                                              fitness= 0.4)$time.in.generations)


rotation.single.g.duration = max(quick_rotations(n.cycles = 1,
                                                 gonotrophic.cycle.length = 3,
                                                 natural.daily.survival = 1,
                                                 female.exposure = 0.7,
                                                 male.exposure = 0.5,
                                                 heritability = 0.3,
                                                 dispersal.rate = 0.2,
                                                 coverage = 0.8,
                                                 threshold = 0.1,
                                                 fitness= 0.4)$time.in.generations)
###
mm.multi.g = subset(quick_micro_mosaic(n.cycles = 5,
                                       gonotrophic.cycle.length = 3,
                                       natural.daily.survival = 1,
                                       female.exposure = 0.7,
                                       male.exposure = 0.5,
                                       heritability = 0.3,
                                       dispersal.rate = 0.2,
                                       coverage = 0.8,
                                       threshold = 1,
                                       fitness= 0.4), site == "intervention"&
                      insecticide.tracked == 2)

rotation.multi.g = subset(quick_rotations(n.cycles = 5,
                                          gonotrophic.cycle.length = 3,
                                          natural.daily.survival = 1,
                                          female.exposure = 0.7,
                                          male.exposure = 0.5,
                                          heritability = 0.3,
                                          dispersal.rate = 0.2,
                                          coverage = 0.8,
                                          threshold = 1,
                                          fitness= 0.4), site == "intervention"&
                            insecticide.tracked == 2)


mm.multi.g.duration = max(quick_micro_mosaic(n.cycles = 5,
                                             gonotrophic.cycle.length = 3,
                                             natural.daily.survival = 1,
                                             female.exposure = 0.7,
                                             male.exposure = 0.5,
                                             heritability = 0.3,
                                             dispersal.rate = 0.2,
                                             coverage = 0.8,
                                             threshold = 0.1,
                                             fitness= 0.4)$time.in.generations)

rotation.multi.g.duration = max(quick_rotations(n.cycles = 5,
                                                gonotrophic.cycle.length = 3,
                                                natural.daily.survival = 1,
                                                female.exposure = 0.7,
                                                male.exposure = 0.5,
                                                heritability = 0.3,
                                                dispersal.rate = 0.2,
                                                coverage = 0.8,
                                                threshold = 0.1,
                                                fitness= 0.4)$time.in.generations)



mm.multi.g.nat.mort = subset(quick_micro_mosaic(n.cycles = 5,
                                                gonotrophic.cycle.length = 3,
                                                natural.daily.survival = 0.8,
                                                female.exposure = 0.7,
                                                male.exposure = 0.5,
                                                heritability = 0.3,
                                                dispersal.rate = 0.2,
                                                coverage = 0.8,
                                                threshold = 1,
                                                fitness= 0.4), site == "intervention"&
                               insecticide.tracked == 2)



rotation.multi.g.nat.mort  = subset(quick_rotations(n.cycles = 5,
                                                    gonotrophic.cycle.length = 3,
                                                    natural.daily.survival = 0.8,
                                                    female.exposure = 0.7,
                                                    male.exposure = 0.5,
                                                    heritability = 0.3,
                                                    dispersal.rate = 0.2,
                                                    coverage = 0.8,
                                                    threshold = 1,
                                                    fitness= 0.4), site == "intervention"& insecticide.tracked == 2)



mm.multi.g.nat.mort.duration = max(quick_micro_mosaic(n.cycles = 5,
                                                      gonotrophic.cycle.length = 3,
                                                      natural.daily.survival = 0.8,
                                                      female.exposure = 0.7,
                                                      male.exposure = 0.5,
                                                      heritability = 0.3,
                                                      dispersal.rate = 0.2,
                                                      coverage = 0.8,
                                                      threshold = 0.1,
                                                      fitness= 0.4)$time.in.generations)



rotation.multi.g.nat.mort.duration  = max(quick_rotations(n.cycles = 5,
                                                          gonotrophic.cycle.length = 3,
                                                          natural.daily.survival = 0.8,
                                                          female.exposure = 0.7,
                                                          male.exposure = 0.5,
                                                          heritability = 0.3,
                                                          dispersal.rate = 0.2,
                                                          coverage = 0.8,
                                                          threshold = 0.1,
                                                          fitness= 0.4)$time.in.generations)




p.1 = ggplot(mm.single.g, aes(x=time.in.generations,
                              y=bioassay.survival*100))+
  geom_line(colour = "red",
            size = 2)+
  geom_line(data = rotation.single.g, aes(x=time.in.generations,
                                          y=bioassay.survival*100),
            colour = "blue",
            size = 2)+
  geom_vline(xintercept = rotation.single.g.duration,
             colour = "darkblue",
             linetype = "dashed", size = 1.5)+
  geom_vline(xintercept = mm.single.g.duration,
             colour = "darkred",
             linetype = "dashed", size = 1.5)+
  geom_hline(yintercept = 10, colour = "grey", linetype = "dotted",
             size = 1.3)+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 500))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 27),
                     breaks = seq(0, 25, 5))+
  xlab("Time in Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Single Gonotrophic Cycle")+
  geom_text(aes(x=400, y=5, label = paste0("Rotations\nperform\nbetter than\nmicromosaics")),
                size = 6)+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))


p.2 = ggplot(mm.multi.g, aes(x=time.in.generations,
                             y=bioassay.survival*100))+
  geom_line(colour = "red", size = 2)+
  geom_line(data = rotation.multi.g, aes(x=time.in.generations,
                                         y=bioassay.survival*100),
            colour = "blue",
            size = 2)+
  geom_vline(xintercept = rotation.multi.g.duration,
             colour = "darkblue",
             linetype = "dashed", size = 1.5)+
  geom_vline(xintercept = mm.multi.g.duration,
             colour = "darkred",
             linetype = "dashed", size = 1.5)+
  geom_hline(yintercept = 10, colour = "grey", linetype = "dotted",
             size = 1.3)+
  geom_text(aes(x=400, y=5, label = paste0("Rotations\nperform\nmarginally\nbetter than\nmicromosaics")),
                size = 6)+
  theme_bw()+
  xlab("Time in Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Five Gonotrophic Cycles; No Natural Mortality")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))


p.3 = ggplot(mm.multi.g.nat.mort, aes(x=time.in.generations,
                                      y=bioassay.survival*100))+
  geom_line(colour = "red", size = 2)+
  geom_line(data = rotation.multi.g.nat.mort, aes(x=time.in.generations,
                                                  y=bioassay.survival*100),
            colour = "blue", size = 2)+
  geom_vline(xintercept = rotation.multi.g.nat.mort.duration,
             colour = "darkblue",
             linetype = "dashed", size = 1.5)+
  geom_vline(xintercept = mm.multi.g.nat.mort.duration,
             colour = "darkred",
             linetype = "dashed", size = 1.5)+
  geom_hline(yintercept = 10, colour = "grey", linetype = "dotted",
             size = 1.3)+
  geom_text(aes(x=400, y=5, label = paste0("Micromosaics\nperform\nbetter than\nrotations")),
                size = 6)+
  theme_bw()+
  xlab("Time in Generations")+
  ylab("Bioassay Survival (%)")+
  ggtitle("Five Gonotrophic Cycles; With Natural Mortality")+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

#requires patchwork
p.1 +p.2 +p.3
