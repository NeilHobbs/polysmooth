#Chapter 4:: mixtures - Cross Resistance and Dosing
library(devtools)
load_all()
library(patchwork)
library(ggridges)


#Read in the datasets: polysmooth
smooth.mixtures.df = read.csv(".//part.3.mixture.cross.resistance.polysmooth.csv")
smooth.solo.df = read.csv(".//part.3.solo.cross.resistance.polysmooth.csv")
smooth.rotation.df = read.csv(".//part.3.rotation.cross.resistance.polysmooth.csv")

#Add in model name:
smooth.mixtures.df$model = "polysmooth"
smooth.solo.df$model = "polysmooth"
smooth.rotation.df$model = "polysmooth"

#read in the datasets: polytruncate

truncation.mixtures.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/part.3.mixture.cross.resistance.csv")
truncation.solo.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/part.3.solo.cross.resistance.csv")
truncation.rotation.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/part.3.rotation.cross.resistance.csv")

#Add in model name:
truncation.mixtures.df$model = "polytruncate"
truncation.solo.df$model = "polytruncate"
truncation.rotation.df$model = "polytruncate"


#then group them to give a single dataframe per IRM strategy
mixtures.df = rbind(smooth.mixtures.df, truncation.mixtures.df)
solo.df = rbind(smooth.solo.df, truncation.solo.df)
rotation.df = rbind(smooth.rotation.df, truncation.rotation.df)


##convert all resistance scores to bioassay survival:::
mixtures.df$insecticide.i.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                   michaelis.menten.slope = 1,
                                                                                   trait.mean = mixtures.df$insecticide.i)
##convert all resistance scores to bioassay survival:::
mixtures.df$insecticide.j.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                   michaelis.menten.slope = 1,
                                                                                   trait.mean = mixtures.df$insecticide.j)


#Subset dataset into the three mixture types
full.dose.df = subset(mixtures.df, dose == 1)
half.dose.50.df = subset(mixtures.df, dose == 0.5)
half.dose.75.df = subset(mixtures.df, dose == 0.75)

##convert all resistance scores to bioassay survival:::
solo.df$deployed.insecticide.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                      half.population.bioassay.survival.resistance = 900,
                                                                                      michaelis.menten.slope = 1,
                                                                                      trait.mean = solo.df$deployed.insecticide)


solo.df$not.deployed.insecticide.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                          half.population.bioassay.survival.resistance = 900,
                                                                                          michaelis.menten.slope = 1,
                                                                                          trait.mean = solo.df$not.deployed.insecticide)



rotation.df$insecticide.i.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                   michaelis.menten.slope = 1,
                                                                                   trait.mean = rotation.df$insecticide.i)
##convert all resistance scores to bioassay survival:::
rotation.df$insecticide.j.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                   michaelis.menten.slope = 1,
                                                                                   trait.mean = rotation.df$insecticide.j)












#Compare versus solo deployments:
#As heritability and dosing (no decay) the same rate of evolution in both halves of the mixture will be the same


difference.deployed.fd = (full.dose.df$insecticide.i.bioassay - solo.df$deployed.insecticide.bioassay)
difference.deployed.fd.percent = ((full.dose.df$insecticide.i.bioassay - solo.df$deployed.insecticide.bioassay)/(solo.df$deployed.insecticide.bioassay))*100
difference.not.deployed.fd = (full.dose.df$insecticide.j.bioassay - solo.df$not.deployed.insecticide.bioassay)
difference.not.deployed.fd.percent = ((full.dose.df$insecticide.j.bioassay - solo.df$not.deployed.insecticide.bioassay)/(solo.df$not.deployed.insecticide.bioassay))*100


difference.deployed.50.hd = (half.dose.50.df$insecticide.i.bioassay - solo.df$deployed.insecticide.bioassay)
difference.deployed.50.hd.percent = ((half.dose.50.df$insecticide.i.bioassay - solo.df$deployed.insecticide.bioassay)/(solo.df$deployed.insecticide.bioassay))*100
difference.not.deployed.50.hd = (half.dose.50.df$insecticide.j.bioassay - solo.df$not.deployed.insecticide.bioassay)
difference.not.deployed.50.hd.percent = ((half.dose.50.df$insecticide.j.bioassay - solo.df$not.deployed.insecticide.bioassay)/(solo.df$not.deployed.insecticide.bioassay))*100

difference.deployed.75.hd = (half.dose.75.df$insecticide.i.bioassay - solo.df$deployed.insecticide.bioassay)
difference.deployed.75.hd.percent = ((half.dose.75.df$insecticide.i.bioassay - solo.df$deployed.insecticide.bioassay)/(solo.df$deployed.insecticide.bioassay))*100
difference.not.deployed.75.hd = (half.dose.75.df$insecticide.j.bioassay - solo.df$not.deployed.insecticide.bioassay)
difference.not.deployed.75.hd.percent = ((half.dose.75.df$insecticide.j.bioassay - solo.df$not.deployed.insecticide.bioassay)/(solo.df$not.deployed.insecticide.bioassay))*100


total.difference.fd = (full.dose.df$insecticide.i.bioassay + full.dose.df$insecticide.j.bioassay) - (solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay)
total.difference.50.hd = (half.dose.50.df$insecticide.i.bioassay + half.dose.50.df$insecticide.j.bioassay) - (solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay)
total.difference.75.hd = (half.dose.75.df$insecticide.i.bioassay + half.dose.75.df$insecticide.j.bioassay) - (solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay)

total.difference.fd.percent = (((full.dose.df$insecticide.i.bioassay + full.dose.df$insecticide.j.bioassay) - (solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay))/(solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay))*100
total.difference.50.hd.percent = (((half.dose.50.df$insecticide.i.bioassay + half.dose.50.df$insecticide.j.bioassay) - (solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay))/(solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay))*100
total.difference.75.hd.percent = (((half.dose.75.df$insecticide.i.bioassay + half.dose.75.df$insecticide.j.bioassay) - (solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay))/(solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay))*100


fd.outcome.deployed = ifelse(difference.deployed.fd < 0,
                             yes = "mixtures better",
                             no = "mixtures worse")

fd.outcome.not.deployed = ifelse(difference.not.deployed.fd < 0,
                                 yes = "mixtures better",
                                 no = "mixtures worse")

hd.50.outcome.deployed = ifelse(difference.deployed.50.hd < 0,
                                yes = "mixtures better",
                                no = "mixtures worse")

hd.50.outcome.not.deployed = ifelse(difference.not.deployed.50.hd < 0,
                                    yes = "mixtures better",
                                    no = "mixtures worse")


hd.75.outcome.deployed = ifelse(difference.deployed.75.hd < 0,
                                yes = "mixtures better",
                                no = "mixtures worse")

hd.75.outcome.not.deployed = ifelse(difference.not.deployed.75.hd < 0,
                                    yes = "mixtures better",
                                    no = "mixtures worse")


overall.fd = ifelse(total.difference.fd < 0,
                    yes = "mixtures better",
                    no = "mixtures worse")

overall.50.hd = ifelse(total.difference.50.hd < 0,
                       yes = "mixtures better",
                       no = "mixtures worse")

overall.75.hd = ifelse(total.difference.75.hd < 0,
                       yes = "mixtures better",
                       no = "mixtures worse")

solo.diff.mix.df = data.frame(difference.deployed.fd,
                              difference.not.deployed.fd,
                              difference.deployed.50.hd,
                              difference.not.deployed.50.hd,
                              difference.deployed.75.hd,
                              difference.not.deployed.75.hd,
                              difference.deployed.fd.percent,
                              difference.not.deployed.fd.percent,
                              difference.deployed.50.hd.percent,
                              difference.not.deployed.50.hd.percent,
                              difference.deployed.75.hd.percent,
                              difference.not.deployed.75.hd.percent,
                              cross.resistance = full.dose.df$cross.resistance,
                              fd.outcome.deployed,
                              fd.outcome.not.deployed,
                              hd.50.outcome.deployed,
                              hd.50.outcome.not.deployed,
                              hd.75.outcome.deployed,
                              hd.75.outcome.not.deployed,
                              total.difference.fd,
                              total.difference.50.hd,
                              total.difference.75.hd,
                              total.difference.fd.percent,
                              total.difference.50.hd.percent,
                              total.difference.75.hd.percent,
                              overall.fd,
                              overall.50.hd,
                              overall.75.hd)


i.difference = c(solo.diff.mix.df$difference.deployed.fd,
                 solo.diff.mix.df$difference.deployed.75.hd,
                 solo.diff.mix.df$difference.deployed.50.hd)
j.difference = c(solo.diff.mix.df$difference.not.deployed.fd,
                 solo.diff.mix.df$difference.not.deployed.75.hd,
                 solo.diff.mix.df$difference.not.deployed.50.hd)
total.difference = i.difference + j.difference

strat = c(rep("FD_FD", 27500), rep("HD_HD retains 75%", 27500), rep("HD_HD retains 50%", 27500))
cross.resistance = rep(solo.diff.mix.df$cross.resistance, 3)


temp.df = data.frame(i.difference,
                     j.difference,
                     total.difference,
                     strat,
                     cross.resistance,
                     heritability = solo.df$Heritability,
                     dispersal= solo.df$Dispersal,
                     female.exposure = solo.df$Female.Insecticide.Exposure,
                     male.exposure = solo.df$Male.Insecticide.Exposure,
                     coverage = solo.df$Intervention.Coverage)


A = ggplot(temp.df, aes(x=i.difference*100,
                        fill=strat))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00"#orange = HD_HD 75%
  ))+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  xlab(paste0("Difference in End Bioassay Survival\nAfter 200 Generations"))+
  ggtitle("Insecticide i")+
  facet_grid(cross.resistance ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))


B = ggplot(temp.df, aes(x=j.difference*100,
                        fill=strat))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00"#orange = HD_HD 75%
  ))+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  xlab(paste0("Difference in End Bioassay Survival\nAfter 200 Generations"))+
  ggtitle("Insecticide j")+
  facet_grid(cross.resistance ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))


C = ggplot(temp.df, aes(x=total.difference*100,
                        fill=strat))+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00"#orange = HD_HD 75%
  ))+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  ggtitle("Total (Insecticide i and Insecticide j)")+
  xlab(paste0("Difference in End Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(cross.resistance ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))


A + B + C

solo.diff.mix.df = data.frame(difference.deployed.fd,
                              difference.not.deployed.fd,
                              difference.deployed.50.hd,
                              difference.not.deployed.50.hd,
                              difference.deployed.75.hd,
                              difference.not.deployed.75.hd,
                              cross.resistance = full.dose.df$cross.resistance,
                              fd.outcome.deployed,
                              fd.outcome.not.deployed,
                              hd.50.outcome.deployed,
                              hd.50.outcome.not.deployed,
                              hd.75.outcome.deployed,
                              hd.75.outcome.not.deployed,
                              total.difference.fd,
                              total.difference.50.hd,
                              total.difference.75.hd,
                              overall.fd,
                              overall.50.hd,
                              overall.75.hd)



colnames(temp.df)

plot.i = ggplot(temp.df, aes(x=female.exposure,
                             y=i.difference*100,
                             colour = strat,
                             fill = strat))+
  geom_hline(yintercept = 0,
             linetype = "dashed")+
  facet_grid(.~cross.resistance)+
  xlab("Female Insecticide Exposure")+
  ylab("Difference in Bioassay Survival")+
  ggtitle("Insecticide i")+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot.j = ggplot(temp.df, aes(x=female.exposure,
                             y=j.difference*100,
                             colour = strat,
                             fill = strat))+
  geom_hline(yintercept = 0,
             linetype = "dashed")+
  facet_grid(.~cross.resistance)+
  xlab("Female Insecticide Exposure")+
  ylab("Difference in Bioassay Survival")+
  ggtitle("Insecticide j")+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot.ij = ggplot(temp.df, aes(x=female.exposure,
                              y=total.difference*100,
                              colour = strat,
                              fill = strat))+
  geom_hline(yintercept = 0,
             linetype = "dashed")+
  xlab("Female Insecticide Exposure")+
  ylab("Difference in Bioassay Survival")+
  ggtitle("Both Insecticides")+
  facet_grid(.~cross.resistance)+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


plot.i + plot.j + plot.ij



##Now compare against rotations:::
difference.i.fd = (full.dose.df$insecticide.i.bioassay - rotation.df$insecticide.i.bioassay)
difference.j.fd = (full.dose.df$insecticide.j.bioassay - rotation.df$insecticide.j.bioassay)
difference.i.50.hd = (half.dose.50.df$insecticide.i.bioassay - rotation.df$insecticide.i.bioassay)
difference.j.50.hd = (half.dose.50.df$insecticide.j.bioassay - rotation.df$insecticide.j.bioassay)
difference.i.75.hd = (half.dose.75.df$insecticide.i.bioassay - rotation.df$insecticide.i.bioassay)
difference.j.75.hd = (half.dose.75.df$insecticide.j.bioassay - rotation.df$insecticide.j.bioassay)

difference.i.fd.percent = (((full.dose.df$insecticide.i.bioassay - rotation.df$insecticide.i.bioassay))/rotation.df$insecticide.i.bioassay)*100
difference.j.fd.percent = (((full.dose.df$insecticide.j.bioassay - rotation.df$insecticide.j.bioassay))/rotation.df$insecticide.j.bioassay)*100
difference.i.50.hd.percent = (((half.dose.50.df$insecticide.i.bioassay - rotation.df$insecticide.i.bioassay))/rotation.df$insecticide.i.bioassay)*100
difference.j.50.hd.percent = (((half.dose.50.df$insecticide.j.bioassay - rotation.df$insecticide.j.bioassay))/rotation.df$insecticide.j.bioassay)*100
difference.i.75.hd.percent = (((half.dose.75.df$insecticide.i.bioassay - rotation.df$insecticide.i.bioassay))/rotation.df$insecticide.i.bioassay)*100
difference.j.75.hd.percent = (((half.dose.75.df$insecticide.j.bioassay - rotation.df$insecticide.j.bioassay))/rotation.df$insecticide.j.bioassay)*100

rot.total.difference.fd = (full.dose.df$insecticide.i.bioassay + full.dose.df$insecticide.j.bioassay) - (rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay)
rot.total.difference.50.hd = (half.dose.50.df$insecticide.i.bioassay + half.dose.50.df$insecticide.j.bioassay) - (rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay)
rot.total.difference.75.hd = (half.dose.75.df$insecticide.i.bioassay + half.dose.75.df$insecticide.j.bioassay) - (rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay)

rot.total.difference.fd.percent = ((full.dose.df$insecticide.i.bioassay + full.dose.df$insecticide.j.bioassay) - (rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay))/(rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay)*100
rot.total.difference.50.hd.percent = ((half.dose.50.df$insecticide.i.bioassay + half.dose.50.df$insecticide.j.bioassay) - (rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay))/(rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay)*100
rot.total.difference.75.hd.percent = ((half.dose.75.df$insecticide.i.bioassay + half.dose.75.df$insecticide.j.bioassay) - (rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay))/(rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay)*100


rot.fd.outcome.i = ifelse(difference.i.fd < 0,
                          yes = "mixtures better",
                          no = "mixtures worse")

rot.fd.outcome.j = ifelse(difference.j.fd < 0,
                          yes = "mixtures better",
                          no = "mixtures worse")

rot.hd.50.outcome.i = ifelse(difference.i.50.hd < 0,
                             yes = "mixtures better",
                             no = "mixtures worse")

rot.hd.50.outcome.j = ifelse(difference.j.50.hd < 0,
                             yes = "mixtures better",
                             no = "mixtures worse")


rot.hd.75.outcome.i = ifelse(difference.i.75.hd < 0,
                             yes = "mixtures better",
                             no = "mixtures worse")

rot.hd.75.outcome.j = ifelse(difference.j.75.hd < 0,
                             yes = "mixtures better",
                             no = "mixtures worse")





cross.resistance = full.dose.df$cross.resistance

rot.overall.fd = ifelse(rot.total.difference.fd < 0,
                        yes = "mixtures better",
                        no = "mixtures worse")

rot.overall.50.hd = ifelse(rot.total.difference.50.hd < 0,
                           yes = "mixtures better",
                           no = "mixtures worse")

rot.overall.75.hd = ifelse(rot.total.difference.75.hd < 0,
                           yes = "mixtures better",
                           no = "mixtures worse")

rot.mix.df = data.frame(difference.i.fd,
                        difference.j.fd,
                        difference.i.50.hd,
                        difference.j.50.hd,
                        difference.i.75.hd,
                        difference.j.75.hd,
                        difference.i.fd.percent,
                        difference.j.fd.percent,
                        difference.i.50.hd.percent,
                        difference.j.50.hd.percent,
                        difference.i.75.hd.percent,
                        difference.j.75.hd.percent,
                        rot.total.difference.fd,
                        rot.total.difference.50.hd,
                        rot.total.difference.75.hd,
                        rot.total.difference.fd.percent,
                        rot.total.difference.50.hd.percent,
                        rot.total.difference.75.hd.percent,
                        rot.fd.outcome.i,
                        rot.fd.outcome.j,
                        rot.hd.50.outcome.i,
                        rot.hd.50.outcome.j,
                        rot.hd.75.outcome.i,
                        rot.hd.75.outcome.j,
                        cross.resistance,
                        rot.overall.fd,
                        rot.overall.50.hd,
                        rot.overall.75.hd
)







i.difference = c(rot.mix.df$difference.i.fd,
                 rot.mix.df$difference.i.75.hd,
                 rot.mix.df$difference.i.50.hd)
j.difference = c(rot.mix.df$difference.j.fd,
                 rot.mix.df$difference.j.75.hd,
                 rot.mix.df$difference.j.50.hd)
total.difference = i.difference + j.difference

strat = rep(c(rep("FD_FD", 27500), rep("HD_HD retains 75%", 27500), rep("HD_HD retains 50%", 27500)), 2)
cross.resistance = rep(solo.diff.mix.df$cross.resistance, 3)


temp.df = data.frame(i.difference,
                     j.difference,
                     total.difference,
                     strat,
                     cross.resistance)


A = ggplot(temp.df, aes(x=i.difference*100,
                        fill=strat))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00"#orange = HD_HD 75%
  ))+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  xlab(paste0("Difference in End Bioassay Survival\nAfter 200 Generations"))+
  ggtitle("Insecticide i")+
  facet_grid(cross.resistance ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))


B = ggplot(temp.df, aes(x=j.difference*100,
                        fill=strat))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00"#orange = HD_HD 75%
  ))+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  xlab(paste0("Difference in End Bioassay Survival\nAfter 200 Generations"))+
  ggtitle("Insecticide j")+
  facet_grid(cross.resistance ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))


C = ggplot(temp.df, aes(x=total.difference*100,
                        fill=strat))+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00"#orange = HD_HD 75%
  ))+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  xlab(paste0("Difference in End Bioassay Survival\nAfter 200 Generations"))+
  ggtitle("Total (Insecticide i and Insecticide j)")+
  facet_grid(cross.resistance ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 12),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

A+B+C


median.values = subset(temp.df, cross.resistance %in%c(-0.5, -0.3,
                                                       -0.1, 0, 0.1, 0.3, 0.5)) %>%
  dplyr::group_by(strat, cross.resistance) %>%
  dplyr::summarize(median=median(total.difference*100))


rot.vs.mix.cross.plot = ggplot(subset(temp.df, cross.resistance %in%c(-0.5, -0.3,
                                                                      -0.1, 0, 0.1, 0.3, 0.5))
                               , aes(x=total.difference*100,
                                            fill=strat))+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  geom_histogram(binwidth = 1)+
  geom_vline(data=median.values, aes(xintercept = median,
                                    colour = strat),
             alpha = 0.5,
             size = 1)+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00"#orange = HD_HD 75%
  ))+
  scale_colour_manual(values = c("red", #red = FD_FD
                               "purple", #purple = HD_HD 50%
                               "orange"#orange = HD_HD 75%
  ))+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = paste("Cross Resistance"),
                                         breaks = NULL,
                                         labels = NULL),
                     expand= c(0, 0))+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  ggtitle("Total (Insecticide i and Insecticide j)")+
  xlab(paste0("Difference in  End Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(cross.resistance ~ .)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x.bottom = element_text(size = 12),
        axis.title.x.top = element_text(size = 12),
        axis.title.y.right = element_text(size = 12),
        axis.title.y.left = element_text(size = 8),
        axis.text.x = element_text(size = 8,
                                   colour = "black"),
        axis.text.y = element_text(colour = "black",
                                   size = 6),
        strip.background =element_rect(fill="#dadaeb"),
        title = element_text(size = 12))





###Directly compare FD and HD
rot.mix.df$total.bioassay.change.fd = (full.dose.df$insecticide.i.bioassay + full.dose.df$insecticide.j.bioassay)
rot.mix.df$total.bioassay.change.50.hd = (half.dose.50.df$insecticide.i.bioassay + half.dose.50.df$insecticide.j.bioassay)
rot.mix.df$fdvs50hd = rot.mix.df$total.bioassay.change.fd - rot.mix.df$total.bioassay.change.50.hd

rot.mix.df$total.bioassay.change.75.hd = (half.dose.75.df$insecticide.i.bioassay + half.dose.75.df$insecticide.j.bioassay)
rot.mix.df$fdvs75hd = rot.mix.df$total.bioassay.change.fd - rot.mix.df$total.bioassay.change.75.hd

full.dose.df$total.bioassay.change.fd = (full.dose.df$insecticide.i.bioassay + full.dose.df$insecticide.j.bioassay)
half.dose.50.df$total.bioassay.change.hd = (half.dose.50.df$insecticide.i.bioassay + half.dose.50.df$insecticide.j.bioassay)
half.dose.75.df$total.bioassay.change.hd = (half.dose.75.df$insecticide.i.bioassay + half.dose.75.df$insecticide.j.bioassay)

full.dose.df$`Cross Resistance` = as.factor(full.dose.df$cross.resistance)
half.dose.50.df$`Cross Resistance` = as.factor(half.dose.50.df$cross.resistance)
half.dose.75.df$`Cross Resistance` = as.factor(half.dose.75.df$cross.resistance)


rotation.df$total.bioassay.change.rot = rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay
rotation.df$`Cross Resistance` = as.factor(rotation.df$cross.resistance)

colnames(full.dose.df)

parameter.vals = full.dose.df[ , c(3, 4, 5, 6, 7, 8, 9, 17)]

rotv50hdmix = half.dose.50.df$total.bioassay.change.hd - rotation.df$total.bioassay.change.rot
rotv75hdmix = half.dose.75.df$total.bioassay.change.hd - rotation.df$total.bioassay.change.rot
rotvdfmix = full.dose.df$total.bioassay.change.fd - rotation.df$total.bioassay.change.rot
fdmixv50hdmix = half.dose.50.df$total.bioassay.change.hd - full.dose.df$total.bioassay.change.fd
fdmixv75hdmix = half.dose.75.df$total.bioassay.change.hd - full.dose.df$total.bioassay.change.fd
hd50mixvhd75mix = half.dose.75.df$total.bioassay.change.hd - half.dose.50.df$total.bioassay.change.hd



rot.v.50.hd.outcome = ifelse(rotation.df$total.bioassay.change.rot < half.dose.50.df$total.bioassay.change.hd,
                             yes = "favours rotation",
                             no = "favours HD mixture")

rot.v.75.hd.outcome = ifelse(rotation.df$total.bioassay.change.rot < half.dose.75.df$total.bioassay.change.hd,
                             yes = "favours rotation",
                             no = "favours HD mixture")

rot.v.fd.outcome = ifelse(rotation.df$total.bioassay.change.rot < full.dose.df$total.bioassay.change.fd,
                          yes = "favours rotation",
                          no = "favours FD mixture")


hd.50.v.fd.outcome = ifelse(half.dose.50.df$total.bioassay.change.hd < full.dose.df$total.bioassay.change.fd,
                            yes = "favours HD Mixture",
                            no = "favours FD mixture")

hd.75.v.fd.outcome = ifelse(half.dose.75.df$total.bioassay.change.hd < full.dose.df$total.bioassay.change.fd,
                            yes = "favours HD Mixture",
                            no = "favours FD mixture")

hd.75.v.hd.50.outcome = ifelse(half.dose.50.df$total.bioassay.change.hd < half.dose.75.df$total.bioassay.change.hd,
                               yes = "favours 50% HD Mixture",
                               no = "favours 75% HD mixture")


comparison.df = data.frame(rotv50hdmix,
                           rotv75hdmix,
                           rotvdfmix,
                           fdmixv50hdmix,
                           fdmixv75hdmix,
                           parameter.vals,
                           rot.v.50.hd.outcome,
                           rot.v.75.hd.outcome,
                           rot.v.fd.outcome,
                           hd.50.v.fd.outcome,
                           hd.75.v.fd.outcome,
                           hd50mixvhd75mix,
                           hd.75.v.hd.50.outcome)

colnames(comparison.df)


##Generalised additive models.
#compare vs rotations
strategy.df = data.frame(bioassay.change = c(full.dose.df$total.bioassay.change.fd,
                                             half.dose.50.df$total.bioassay.change.hd,
                                             half.dose.75.df$total.bioassay.change.hd,
                                             rotation.df$total.bioassay.change.rot),
                         strategy = rep(c("full dose", "half dose 50%",
                                          "half dose 75%", "rotation"), each = 27500),
                         heritability = c(full.dose.df$Heritability,
                                          half.dose.50.df$Heritability,
                                          half.dose.75.df$Heritability,
                                          rotation.df$Heritability),
                         female.exposure = c(full.dose.df$Female.Insecticide.Exposure,
                                             half.dose.50.df$Female.Insecticide.Exposure,
                                             half.dose.75.df$Female.Insecticide.Exposure,
                                             rotation.df$Female.Insecticide.Exposure),
                         male.exposure = c(full.dose.df$Male.Insecticide.Exposure,
                                           half.dose.50.df$Male.Insecticide.Exposure,
                                           half.dose.75.df$Male.Insecticide.Exposure,
                                           rotation.df$Male.Insecticide.Exposure),
                         coverage = c(full.dose.df$Intervention.Coverage,
                                      half.dose.50.df$Intervention.Coverage,
                                      half.dose.75.df$Intervention.Coverage,
                                      rotation.df$Intervention.Coverage),
                         dispersal = c(full.dose.df$Dispersal,
                                       half.dose.50.df$Dispersal,
                                       half.dose.75.df$Dispersal,
                                       rotation.df$Dispersal),
                         cross.resistance = c(full.dose.df$cross.resistance,
                                              half.dose.50.df$cross.resistance,
                                              half.dose.75.df$cross.resistance,
                                              rotation.df$cross.resistance))


ggplot(strategy.df, aes(x=female.exposure,
                        y=bioassay.change*100,
                        colour = strategy,
                        fill = strategy))+
  geom_smooth(method="gam")+
  facet_grid(.~cross.resistance)+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("red", "purple", "orange", "black"))+
  scale_colour_manual(values = c("red", "purple", "orange", "black"))+
  ylab("Total Change in Bioassay Survival")+
  xlab("Female Insecticide Exposure")+
  ggtitle("Polysmooth")+
  theme_bw()+
  theme(legend.position = "bottom")


####

colnames(solo.diff.mix.df)

differences.i = c(difference.deployed.fd,
                  difference.deployed.50.hd,
                  difference.deployed.75.hd)

differences.j = c(difference.not.deployed.fd,
                  difference.not.deployed.50.hd,
                  difference.not.deployed.75.hd)

differences.ij = c(total.difference.fd,
                   total.difference.50.hd,
                   total.difference.fd)

cross.resistance.values = cross.resistance
strategies = rep(c("full dose", "half dose 50%", "half dose 75%"), each =  27500)

cross.resistance.values = rep(full.dose.df$cross.resistance, 3)

df.1 = data.frame(differences.i,
                  differences.j,
                  differences.ij,
                  cross.resistance.values,
                  strategies,
                  f.exposure = rep(full.dose.df$Female.Insecticide.Exposure, 3))


ggplot(df.1, aes(x=f.exposure,
                 y=(differences.i + differences.j)*100,
                 colour = strategies,
                 fill = strategies))+
  geom_smooth()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  facet_grid(.~cross.resistance.values)+
  theme_bw()


table(df.1$strategies)






