library(patchwork)
library(dplyr)
library(devtools)
load_all()
##Read in the datasets:::
set.1 = read.csv(".//mixtures.smooth.set.1.csv")
set.2 = read.csv(".//mixtures.smooth.set.2.csv")
set.3 = read.csv(".//mixtures.smooth.set.3.csv")
set.4 = read.csv(".//mixtures.smooth.set.4.csv")
set.5 = read.csv(".//mixtures.smooth.set.5.csv")
set.6 = read.csv(".//mixtures.smooth.set.6.csv")
set.7 = read.csv(".//mixtures.smooth.set.7.csv")
set.8 = read.csv(".//mixtures.smooth.set.8.csv")
set.9 = read.csv(".//mixtures.smooth.set.9.csv")
set.10 = read.csv(".//mixtures.smooth.set.10.csv")


smooth.scaled.df = rbind(set.1,
                         set.2,
                         set.3,
                         set.4,
                         set.5,
                         set.6,
                         set.7,
                         set.8,
                         set.9,
                         set.10)
#Remove to clear space
rm(set.1,
   set.2,
   set.3,
   set.4,
   set.5,
   set.6,
   set.7,
   set.8,
   set.9,
   set.10)


##Issue with the naming of the variables --> make sure it is just where threshold gens are the same for all sims
smooth.scaled.df = subset(smooth.scaled.df, threshold.gens == threshold.gens.1)


set1.75 = read.csv("75_efficacy_mixtures.smooth.set.1.csv")
set2.75 = read.csv("75_efficacy_mixtures.smooth.set.2.csv")
set3.75 = read.csv("75_efficacy_mixtures.smooth.set.3.csv")
set4.75 = read.csv("75_efficacy_mixtures.smooth.set.4.csv")
set5.75 = read.csv("75_efficacy_mixtures.smooth.set.5.csv")
set6.75 = read.csv("75_efficacy_mixtures.smooth.set.6.csv")
set7.75 = read.csv("75_efficacy_mixtures.smooth.set.7.csv")
set8.75 = read.csv("75_efficacy_mixtures.smooth.set.8.csv")
set9.75 = read.csv("75_efficacy_mixtures.smooth.set.9.csv")
set10.75 = read.csv("75_efficacy_mixtures.smooth.set.10.csv")

set.75s = rbind(set1.75,
                set2.75,
                set3.75,
                set4.75,
                set5.75,
                set6.75,
                set7.75,
                set8.75,
                set9.75,
                set10.75)
rm(set1.75,
   set2.75,
   set3.75,
   set4.75,
   set5.75,
   set6.75,
   set7.75,
   set8.75,
   set9.75,
   set10.75)


smooth.df.1 = subset(smooth.scaled.df, dose.1 == 1 &
                          dose.2 == 1)


set.75s$novel.solo = smooth.df.1$novel.solo
set.75s$pyrethroid.solo = smooth.df.1$pyrethroid.solo

#add solo columns

#make one dataset
smooth.scaled.df = dplyr::bind_rows(smooth.scaled.df,
                                set.75s)

rm(set.75s, smooth.df, smooth.df.1)


#Raw change between the mixture and the solo.
smooth.scaled.df$change.novel.peak = smooth.scaled.df$novel.solo - smooth.scaled.df$mixture.novel
smooth.scaled.df$change.pyrethroid.peak = smooth.scaled.df$pyrethroid.solo - smooth.scaled.df$mixture.pyrethroid


#Convert PRS scores to Bioassay Survival
smooth.scaled.df$start.bioassay.pyrethroid = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                           trait.mean = smooth.scaled.df$start.resistance.2,
                                                                                           half.population.bioassay.survival.resistance = 900,
                                                                                           michaelis.menten.slope = 1)*100

smooth.scaled.df$novel.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                          trait.mean = smooth.scaled.df$novel.solo,
                                                                                          half.population.bioassay.survival.resistance = 900,
                                                                                          michaelis.menten.slope = 1)


smooth.scaled.df$pyrethroid.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                               trait.mean = smooth.scaled.df$pyrethroid.solo,
                                                                                               half.population.bioassay.survival.resistance = 900,
                                                                                               michaelis.menten.slope = 1)



smooth.scaled.df$novel.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                         trait.mean = smooth.scaled.df$mixture.novel,
                                                                                         half.population.bioassay.survival.resistance = 900,
                                                                                         michaelis.menten.slope = 1)



smooth.scaled.df$pyrethroid.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = smooth.scaled.df$mixture.pyrethroid,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)



#Changes in the bioassay survival mixture vs solo
smooth.scaled.df$change.novel.peak.bioassay = smooth.scaled.df$novel.solo.peak.bioassay - smooth.scaled.df$novel.mix.peak.bioassay
smooth.scaled.df$change.pyrethroid.peak.bioassay = smooth.scaled.df$pyrethroid.solo.peak.bioassay - smooth.scaled.df$pyrethroid.mix.peak.bioassay


#Categorise the dosing strategies;  Novel_Pyrethroid
#FD --> Full Dose
# HD --> Half dose (retains 50% efficacy of full dose)
smooth.scaled.df$dosing.strategy = ifelse(smooth.scaled.df$dose.1 == 1 &
                                            smooth.scaled.df$dose.2 == 1,
                                          yes = "FD_FD",
                                          no = ifelse(smooth.scaled.df$dose.1 == 1 &
                                                        smooth.scaled.df$dose.2 == 0.5,
                                                      yes = "FD_HD",
                                                      no = ifelse(smooth.scaled.df$dose.1 == 0.5 &
                                                                    smooth.scaled.df$dose.2 == 1,
                                                                  yes = "HD_FD",
                                                                  no = ifelse(smooth.scaled.df$dose.1 == 0.5 &
                                                                                smooth.scaled.df$dose.2 == 0.5,
                                                                  yes = "HD_HD retains 50%",
                                                                  no = "HD_HD retains 75%"))))



table(smooth.scaled.df$dosing.strategy)


#Turn decay rates into categories for easier interpretation
smooth.scaled.df$decay.rate = factor(ifelse(smooth.scaled.df$base.decay.1 == 0.025 &
                                              smooth.scaled.df$base.decay.2 == 0.005,
                                            yes = "much faster",
                                            no = ifelse(smooth.scaled.df$base.decay.1 == 0.025 &
                                                          smooth.scaled.df$base.decay.2 == 0.015,
                                                        yes = "faster",
                                                        no = ifelse(smooth.scaled.df$base.decay.1 == 0.015 &
                                                                      smooth.scaled.df$base.decay.2 == 0.005,
                                                                    yes = "faster",
                                                                    no = ifelse(smooth.scaled.df$base.decay.1 == 0.005 &
                                                                                  smooth.scaled.df$base.decay.2 == 0.025,
                                                                                yes = "much slower",
                                                                                no = ifelse(smooth.scaled.df$base.decay.1 == 0.005 &
                                                                                              smooth.scaled.df$base.decay.2 == 0.015,
                                                                                            yes = "slower",
                                                                                            no = ifelse(smooth.scaled.df$base.decay.1 == 0.015 &
                                                                                                          smooth.scaled.df$base.decay.2 == 0.025,
                                                                                                        yes = "slower",
                                                                                                        no = ifelse(smooth.scaled.df$base.decay.1 == smooth.scaled.df$base.decay.2,
                                                                                                                    yes = "same",
                                                                                                                    no = NA))))))), levels = c("much faster", "faster", "same", "slower", "much slower"))




#calculate changes in rates of evolution:::
smooth.scaled.df$novel.solo.rate = smooth.scaled.df$novel.solo.peak.bioassay/200
smooth.scaled.df$pyrethroid.solo.rate = (smooth.scaled.df$pyrethroid.solo.peak.bioassay - (smooth.scaled.df$start.bioassay.pyrethroid/100))/200
smooth.scaled.df$novel.mix.rate = smooth.scaled.df$novel.mix.peak.bioassay/200
smooth.scaled.df$pyrethroid.mix.rate = (smooth.scaled.df$pyrethroid.mix.peak.bioassay - (smooth.scaled.df$start.bioassay.pyrethroid/100))/200

smooth.scaled.df$rate.change.novel.percent = ((smooth.scaled.df$novel.mix.rate - smooth.scaled.df$novel.solo.rate)/smooth.scaled.df$novel.solo.rate) * 100
smooth.scaled.df$rate.change.pyrethroid.percent = ((smooth.scaled.df$pyrethroid.mix.rate - smooth.scaled.df$pyrethroid.solo.rate)/smooth.scaled.df$pyrethroid.solo.rate) * 100


#First simply look at the impact of only Dosing / Resistance / Decay

smooth.scaled.df$novel.rate.change = ifelse(smooth.scaled.df$rate.change.novel.percent > 0,
                                            yes = "faster",
                                            no = ifelse(smooth.scaled.df$rate.change.novel.percent < 0,
                                                        yes = "slower",
                                                        no = "no change"))

smooth.scaled.df$pyrethroid.rate.change = ifelse(smooth.scaled.df$rate.change.pyrethroid.percent > 0,
                                                 yes = "faster",
                                                 no = ifelse(smooth.scaled.df$rate.change.pyrethroid.percent < 0,
                                                             yes = "slower",
                                                             no = "no change"))





heritability.gam.novel = ggplot(smooth.scaled.df, aes(x=heritability,
                             y=rate.change.novel.percent,
                             colour = dosing.strategy,
                             fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a", #green = HD_FD
                                 "#984ea3", #purple = HD_HD 50%
                                 "#ff7f00" #orange = HD_HD 75%
                                 ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a", #green = HD_FD
                                 "#984ea3", #purple = HD_HD 50%
                                 "#ff7f00" #orange = HD_HD 75%
  ))+
  facet_grid(.~start.bioassay.pyrethroid)+
  ggtitle("polysmooth: Novel")+
  xlab("Heritability")+
  ylab("Percentage Change in Rate of Evolution")+
  theme_bw()+
  theme(legend.position = "bottom")

heritability.gam.pyrethroid = ggplot(smooth.scaled.df, aes(x=heritability,
                                                      y=rate.change.pyrethroid.percent,
                                                      colour = dosing.strategy,
                                                      fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a", #green = HD_FD
                                 "#984ea3", #purple = HD_HD 50%
                                 "#ff7f00" #orange = HD_HD 75%
  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#377eb8", #blue = FD_HD
                               "#4daf4a", #green = HD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00" #orange = HD_HD 75%
  ))+
  facet_grid(.~start.bioassay.pyrethroid)+
  ggtitle("polysmooth: Pyrethroid")+
  xlab("Heritability")+
  ylab("Percentage Change in Rate of Evolution")+
  theme_bw()+
  theme(legend.position = "bottom")


heritability.gam.novel + heritability.gam.pyrethroid
rm(heritability.gam.novel, heritability.gam.pyrethroid)



female.gam.novel = ggplot(smooth.scaled.df, aes(x=female.exposure,
                                                      y=rate.change.novel.percent,
                                                      colour = dosing.strategy,
                                                      fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a", #green = HD_FD
                                 "#984ea3", #purple = HD_HD 50%
                                 "#ff7f00" #orange = HD_HD 75%
  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#377eb8", #blue = FD_HD
                               "#4daf4a", #green = HD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00" #orange = HD_HD 75%
  ))+
  facet_grid(.~start.bioassay.pyrethroid)+
  ggtitle("polysmooth: Novel")+
  xlab("Female Insecticide Exposure")+
  ylab("Percentage Change in Rate of Evolution")+
  theme_bw()+
  theme(legend.position = "bottom")

female.gam.pyrethroid = ggplot(smooth.scaled.df, aes(x=female.exposure,
                                                           y=rate.change.pyrethroid.percent,
                                                           colour = dosing.strategy,
                                                           fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a", #green = HD_FD
                                 "#984ea3", #purple = HD_HD 50%
                                 "#ff7f00" #orange = HD_HD 75%
  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#377eb8", #blue = FD_HD
                               "#4daf4a", #green = HD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00" #orange = HD_HD 75%
  ))+
  facet_grid(.~start.bioassay.pyrethroid)+
  ggtitle("polysmooth: Pyrethroid")+
  xlab("Female Insecticide Exposure")+
  ylab("Percentage Change in Rate of Evolution")+
  theme_bw()+
  theme(legend.position = "bottom")

female.gam.novel + female.gam.pyrethroid
rm(female.gam.novel, female.gam.pyrethroid)


male.gam.novel = ggplot(smooth.scaled.df, aes(x=male.exposure,
                                                y=rate.change.novel.percent,
                                                colour = dosing.strategy,
                                                fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a", #green = HD_FD
                                 "#984ea3", #purple = HD_HD 50%
                                 "#ff7f00" #orange = HD_HD 75%
  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#377eb8", #blue = FD_HD
                               "#4daf4a", #green = HD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00" #orange = HD_HD 75%
  ))+
  facet_grid(.~start.bioassay.pyrethroid)+
  ggtitle("polysmooth: Novel")+
  xlab("Male Insecticide Exposure")+
  ylab("Percentage Change in Rate of Evolution")+
  theme_bw()+
  theme(legend.position = "bottom")

male.gam.pyrethroid = ggplot(smooth.scaled.df, aes(x=male.exposure,
                                                     y=rate.change.pyrethroid.percent,
                                                     colour = dosing.strategy,
                                                     fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a", #green = HD_FD
                                 "#984ea3", #purple = HD_HD 50%
                                 "#ff7f00" #orange = HD_HD 75%
  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#377eb8", #blue = FD_HD
                               "#4daf4a", #green = HD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00" #orange = HD_HD 75%
  ))+
  facet_grid(.~start.bioassay.pyrethroid)+
  ggtitle("polysmooth: Pyrethroid")+
  xlab("Male Insecticide Exposure")+
  ylab("Percentage Change in Rate of Evolution")+
  theme_bw()+
  theme(legend.position = "bottom")

male.gam.novel + male.gam.pyrethroid
rm(male.gam.novel, male.gam.pyrethroid)



coverage.gam.novel = ggplot(smooth.scaled.df, aes(x=intervention.coverage,
                                              y=rate.change.novel.percent,
                                              colour = dosing.strategy,
                                              fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a", #green = HD_FD
                                 "#984ea3", #purple = HD_HD 50%
                                 "#ff7f00" #orange = HD_HD 75%
  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#377eb8", #blue = FD_HD
                               "#4daf4a", #green = HD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00" #orange = HD_HD 75%
  ))+
  facet_grid(.~start.bioassay.pyrethroid)+
  ggtitle("polysmooth: Novel")+
  xlab("Intervention Coverage")+
  ylab("Percentage Change in Rate of Evolution")+
  theme_bw()+
  theme(legend.position = "bottom")

coverage.gam.pyrethroid = ggplot(smooth.scaled.df, aes(x=intervention.coverage,
                                                   y=rate.change.pyrethroid.percent,
                                                   colour = dosing.strategy,
                                                   fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a", #green = HD_FD
                                 "#984ea3", #purple = HD_HD 50%
                                 "#ff7f00" #orange = HD_HD 75%
  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#377eb8", #blue = FD_HD
                               "#4daf4a", #green = HD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00" #orange = HD_HD 75%
  ))+
  facet_grid(.~start.bioassay.pyrethroid)+
  ggtitle("polysmooth: Pyrethroid")+
  xlab("Intervention Coverage")+
  ylab("Percentage Change in Rate of Evolution")+
  theme_bw()+
  theme(legend.position = "bottom")

coverage.gam.novel + coverage.gam.pyrethroid
rm(coverage.gam.novel, coverage.gam.pyrethroid)

dispersal.gam.novel = ggplot(smooth.scaled.df, aes(x=dispersal,
                                                  y=rate.change.novel.percent,
                                                  colour = dosing.strategy,
                                                  fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a", #green = HD_FD
                                 "#984ea3", #purple = HD_HD 50%
                                 "#ff7f00" #orange = HD_HD 75%
  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#377eb8", #blue = FD_HD
                               "#4daf4a", #green = HD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00" #orange = HD_HD 75%
  ))+
  facet_grid(.~start.bioassay.pyrethroid)+
  ggtitle("polysmooth: Novel")+
  xlab("Dispersal Rate")+
  ylab("Percentage Change in Rate of Evolution")+
  theme_bw()+
  theme(legend.position = "bottom")

dispersal.gam.pyrethroid = ggplot(smooth.scaled.df, aes(x=dispersal,
                                                       y=rate.change.pyrethroid.percent,
                                                       colour = dosing.strategy,
                                                       fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a", #green = HD_FD
                                 "#984ea3", #purple = HD_HD 50%
                                 "#ff7f00" #orange = HD_HD 75%
  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#377eb8", #blue = FD_HD
                               "#4daf4a", #green = HD_FD
                               "#984ea3", #purple = HD_HD 50%
                               "#ff7f00" #orange = HD_HD 75%
  ))+
  facet_grid(.~start.bioassay.pyrethroid)+
  ggtitle("polysmooth: Pyrethroid")+
  xlab("Intervention Coverage")+
  ylab("Percentage Change in Rate of Evolution")+
  theme_bw()+
  theme(legend.position = "bottom")

dispersal.gam.novel + dispersal.gam.pyrethroid
rm(dispersal.gam.novel, dispersal.gam.pyrethroid)
