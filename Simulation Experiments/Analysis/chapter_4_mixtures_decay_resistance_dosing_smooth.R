library(devtools)
load_all()
library(ggplot2)
library(dplyr)
library(patchwork)


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


smooth.scaled.df.1 = subset(smooth.scaled.df, dose.1 == 1 &
                           dose.2 == 1)

#add solo columns
set.75s$novel.solo = smooth.scaled.df.1$novel.solo
set.75s$pyrethroid.solo = smooth.scaled.df.1$pyrethroid.solo


#make one dataset
smooth.scaled.df = dplyr::bind_rows(smooth.scaled.df,
                                        set.75s)

rm(set.75s, smooth.scaled.df.1)


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
smooth.scaled.df$dosing.strategy = factor(ifelse(smooth.scaled.df$dose.1 == 1 &
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
                                                                                         no = "HD_HD retains 75%")))))






#put dosings into a more logical order for plotting etc
smooth.scaled.df$dosing.strategy = factor(smooth.scaled.df$dosing.strategy,
                                              levels = c("FD_FD",
                                                         "HD_HD retains 75%",
                                                         "HD_HD retains 50%",
                                                         "FD_HD",
                                                         "HD_FD"))

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


##Importance of Dosing
novel.median.value = smooth.scaled.df %>%
  group_by(dosing.strategy) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.scaled.df %>%
  group_by(dosing.strategy) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = smooth.scaled.df %>%
  group_by(dosing.strategy) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.scaled.df %>%
  group_by(dosing.strategy) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))


dosing.plot.novel = ggplot(smooth.scaled.df,
                           aes(x=rate.change.novel.percent,
                               fill = novel.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value,
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  geom_vline(data= novel.mean.value,
             aes(xintercept = mean),
             colour = "green",
             size = 2,
             alpha = 0.3)+
  facet_grid(dosing.strategy~.)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

dosing.plot.pyr = ggplot(smooth.scaled.df,
                         aes(x=rate.change.pyrethroid.percent,
                             fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("grey", "red", "blue"))+
  facet_grid(dosing.strategy~.)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value,
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  geom_vline(data= pyrethroid.mean.value,
             aes(xintercept = mean),
             colour = "green",
             size = 2,
             alpha =0.3)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")


dosing.plot.novel + dosing.plot.pyr

rm(dosing.plot.novel, dosing.plot.pyr)



#Importance of Resistance
novel.median.value = smooth.scaled.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.scaled.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = smooth.scaled.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.scaled.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))

resistance.plot.novel = ggplot(smooth.scaled.df, aes(x=rate.change.novel.percent,
                                                         fill = novel.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= novel.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

resistance.plot.pyr = ggplot(smooth.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                                       fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("grey", "red", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= pyrethroid.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

resistance.plot.novel + resistance.plot.pyr
rm(resistance.plot.novel, resistance.plot.pyr)

#Importance of Decay
novel.median.value = smooth.scaled.df %>%
  group_by(decay.rate) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.scaled.df %>%
  group_by(decay.rate) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = smooth.scaled.df %>%
  group_by(decay.rate) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.scaled.df %>%
  group_by(decay.rate) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))
decay.plot.novel = ggplot(smooth.scaled.df, aes(x=rate.change.novel.percent,
                                                    fill = novel.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= novel.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  facet_grid(decay.rate ~ .)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

decay.plot.pyr = ggplot(smooth.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                                  fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("grey", "red", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= pyrethroid.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  facet_grid(decay.rate ~ .)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")


decay.plot.novel + decay.plot.pyr
rm(decay.plot.novel, decay.plot.pyr)



#Of course there will be interactions between all these aspects.
#Dosing and Resistance

novel.median.value = smooth.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = smooth.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))

resistance.dose.plot.novel = ggplot(smooth.scaled.df, aes(x=rate.change.novel.percent,
                                                              fill = novel.rate.change))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= novel.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(start.bioassay.pyrethroid ~ dosing.strategy)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

resistance.dose.plot.pr = ggplot(smooth.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                                           fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  geom_vline(data= pyrethroid.mean.value, aes(xintercept = mean),
             colour = "green",
             alpha = 0.3,
             size = 2)+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(start.bioassay.pyrethroid ~ dosing.strategy)+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none")

resistance.dose.plot.novel + resistance.dose.plot.pr
rm(resistance.dose.plot.novel, resistance.dose.plot.pr)

##Trying to put "everything" together...

novel.median.value = smooth.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid, decay.rate) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid, decay.rate) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = smooth.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid, decay.rate) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.scaled.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid, decay.rate) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))


resistance.dose.decay.plot.novel = ggplot(smooth.scaled.df, aes(x=rate.change.novel.percent,
                                                                    fill = dosing.strategy))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median,
                                           colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dashed")+
  geom_vline(data= novel.mean.value, aes(xintercept = mean,
                                         colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dotted")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                "#ff7f00",#orange = HD_HD 75%
                                "#984ea3", #purple = HD_HD 50%
                                "#377eb8", #blue = FD_HD
                                "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(decay.rate~start.bioassay.pyrethroid)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "The novel insecticide decays ____ than the pyrethroid insecticide",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")


resistance.dose.decay.plot.pyrethroid = ggplot(smooth.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                                                         fill = dosing.strategy))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median,
                                                colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dashed")+
  geom_vline(data= pyrethroid.mean.value, aes(xintercept = mean,
                                              colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dotted")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                "#ff7f00",#orange = HD_HD 75%
                                "#984ea3", #purple = HD_HD 50%
                                "#377eb8", #blue = FD_HD
                                "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(decay.rate~start.bioassay.pyrethroid)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "The novel insecticide decays ____ than the pyrethroid insecticide",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  xlab("Change in the Rate of Evolution (%)")+
  ylab("Frequency")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")

resistance.dose.decay.plot.novel + resistance.dose.decay.plot.pyrethroid

rm(resistance.dose.decay.plot.novel, resistance.dose.decay.plot.pyrethroid)

###Fit and Plot Generalised Additive Models:
# 1.Heritability
# 2.Female Exposure
# 3. Total Male Exposure
# 4. Coverage
# 5. Dispersal

#Heritability
heritability.novel.gam = ggplot(smooth.scaled.df, aes(x=heritability,
                                                          y=rate.change.novel.percent,
                                                          colour = dosing.strategy,
                                                          fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Heritability")+
  ggtitle("polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")


heritability.pyrethroid.gam = ggplot(smooth.scaled.df, aes(x=heritability,
                                                               y=rate.change.pyrethroid.percent,
                                                               colour = dosing.strategy,
                                                               fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Heritability")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")



heritability.novel.gam + heritability.pyrethroid.gam
rm(heritability.novel.gam, heritability.pyrethroid.gam)

female.exposure.novel.gam = ggplot(smooth.scaled.df, aes(x=female.exposure,
                                                             y=rate.change.novel.percent,
                                                             colour = dosing.strategy,
                                                             fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Female Insecticide Encounter Probability")+
  ggtitle("polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")

female.exposure.pyrethroid.gam = ggplot(smooth.scaled.df, aes(x=female.exposure,
                                                                  y=rate.change.pyrethroid.percent,
                                                                  colour = dosing.strategy,
                                                                  fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Female Insecticide Encounter Probability")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")



female.exposure.novel.gam + female.exposure.pyrethroid.gam
rm(female.exposure.novel.gam, female.exposure.pyrethroid.gam)

male.exposure.novel.gam = ggplot(smooth.scaled.df, aes(x=male.exposure,
                                                           y=rate.change.novel.percent,
                                                           colour = dosing.strategy,
                                                           fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Male Insecticide Encounter Probability")+
  ggtitle("polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")

male.exposure.pyrethroid.gam = ggplot(smooth.scaled.df, aes(x=male.exposure,
                                                                y=rate.change.pyrethroid.percent,
                                                                colour = dosing.strategy,
                                                                fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Male Insecticide Encounter Probability")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")



male.exposure.novel.gam + male.exposure.pyrethroid.gam
rm(male.exposure.novel.gam, male.exposure.pyrethroid.gam)


coverage.novel.gam = ggplot(smooth.scaled.df, aes(x=intervention.coverage,
                                                      y=rate.change.novel.percent,
                                                      colour = dosing.strategy,
                                                      fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Intervention Coverage")+
  ggtitle("polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")

coverage.pyrethroid.gam = ggplot(smooth.scaled.df, aes(x=intervention.coverage,
                                                           y=rate.change.pyrethroid.percent,
                                                           colour = dosing.strategy,
                                                           fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Intervention Coverage")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")


coverage.novel.gam + coverage.pyrethroid.gam
rm(coverage.novel.gam, coverage.pyrethroid.gam)


dispersal.novel.gam = ggplot(smooth.scaled.df, aes(x=dispersal,
                                                       y=rate.change.novel.percent,
                                                       colour = dosing.strategy,
                                                       fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Dispersal Rate")+
  ggtitle("polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")

dispersal.pyrethroid.gam = ggplot(smooth.scaled.df, aes(x=dispersal,
                                                            y=rate.change.pyrethroid.percent,
                                                            colour = dosing.strategy,
                                                            fill = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#e41a1c", #red = FD_FD
                                 "#ff7f00",#orange = HD_HD 75%
                                 "#984ea3", #purple = HD_HD 50%
                                 "#377eb8", #blue = FD_HD
                                 "#4daf4a" #green = HD_FD

  ))+
  scale_fill_manual(values = c("#e41a1c", #red = FD_FD
                               "#ff7f00",#orange = HD_HD 75%
                               "#984ea3", #purple = HD_HD 50%
                               "#377eb8", #blue = FD_HD
                               "#4daf4a" #green = HD_FD
  ))+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  ylab("Change in the Rate of Evolution (%)")+
  xlab("Dispersal Rate")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "bottom")



dispersal.novel.gam + dispersal.pyrethroid.gam
rm(dispersal.novel.gam, dispersal.pyrethroid.gam)

#####
smooth.scaled.df$change.novel.peak = smooth.scaled.df$novel.solo - smooth.scaled.df$mixture.novel
smooth.scaled.df$change.pyrethroid.peak = smooth.scaled.df$pyrethroid.solo - smooth.scaled.df$mixture.pyrethroid

##Convert PRS to Bioassays
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



#Calculate Differences between solo and mixture deployments
smooth.scaled.df$change.novel.peak.bioassay = smooth.scaled.df$novel.solo.peak.bioassay - smooth.scaled.df$novel.mix.peak.bioassay
smooth.scaled.df$change.pyrethroid.peak.bioassay = smooth.scaled.df$pyrethroid.solo.peak.bioassay - smooth.scaled.df$pyrethroid.mix.peak.bioassay

#calculate changes in rates of evolution:::
smooth.scaled.df$novel.solo.rate = smooth.scaled.df$novel.solo.peak.bioassay/200
smooth.scaled.df$pyrethroid.solo.rate = (smooth.scaled.df$pyrethroid.solo.peak.bioassay - (smooth.scaled.df$start.bioassay.pyrethroid/100))/200
smooth.scaled.df$novel.mix.rate = smooth.scaled.df$novel.mix.peak.bioassay/200
smooth.scaled.df$pyrethroid.mix.rate = (smooth.scaled.df$pyrethroid.mix.peak.bioassay - (smooth.scaled.df$start.bioassay.pyrethroid/100))/200

smooth.scaled.df$rate.change.novel.percent = ((smooth.scaled.df$novel.mix.rate - smooth.scaled.df$novel.solo.rate)/smooth.scaled.df$novel.solo.rate) * 100
smooth.scaled.df$rate.change.pyrethroid.percent = ((smooth.scaled.df$pyrethroid.mix.rate - smooth.scaled.df$pyrethroid.solo.rate)/smooth.scaled.df$pyrethroid.solo.rate) * 100

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


smooth.scaled.df$diff.end.bioassay.pyr.mix = (smooth.scaled.df$pyrethroid.mix.peak.bioassay*100) - smooth.scaled.df$start.bioassay.pyrethroid

smooth.df.1 = subset(smooth.scaled.df, dose.1 == 1 &
                           dose.2 == 1)

smooth.df.0.75 = subset(smooth.scaled.df, dose.1 == 0.75 &
                              dose.2 == 0.75)

smooth.df.0.5 = subset(smooth.scaled.df, dose.1 == 0.5 &
                             dose.2 == 0.5)



smooth.df.1$novel_75_vs_100 = ((smooth.df.0.75$novel.mix.peak.bioassay - smooth.df.1$novel.mix.peak.bioassay) / smooth.df.1$novel.mix.peak.bioassay)*100
smooth.df.1$novel_50_vs_100 = ((smooth.df.0.5$novel.mix.peak.bioassay - smooth.df.1$novel.mix.peak.bioassay) / smooth.df.1$novel.mix.peak.bioassay)*100
smooth.df.1$novel_50_vs_75 = ((smooth.df.0.5$novel.mix.peak.bioassay - smooth.df.0.75$novel.mix.peak.bioassay) / smooth.df.0.75$novel.mix.peak.bioassay)*100

smooth.df.1$novel_75_vs_100_outcome = ifelse(smooth.df.1$novel_75_vs_100 > 0,
                                                 yes = "75% Win",
                                                 no = "100% Win")

smooth.df.1$novel_50_vs_100_outcome = ifelse(smooth.df.1$novel_50_vs_100 > 0,
                                                 yes = "50% Win",
                                                 no = "100% Win")


smooth.df.1$novel_50_vs_75_outcome = ifelse(smooth.df.1$novel_50_vs_75 > 0,
                                                yes = "50% Win",
                                                no = "75% Win")




smooth.df.1$pyr_75_vs_100 = ((smooth.df.0.75$diff.end.bioassay.pyr.mix - smooth.df.1$diff.end.bioassay.pyr.mix) / smooth.df.1$diff.end.bioassay.pyr.mix)*100
smooth.df.1$pyr_50_vs_100 = ((smooth.df.0.5$diff.end.bioassay.pyr.mix - smooth.df.1$diff.end.bioassay.pyr.mix) / smooth.df.1$diff.end.bioassay.pyr.mix)*100
smooth.df.1$pyr_50_vs_75 = ((smooth.df.0.5$diff.end.bioassay.pyr.mix - smooth.df.0.75$diff.end.bioassay.pyr.mix) / smooth.df.0.75$diff.end.bioassay.pyr.mix)*100

smooth.df.1$pyr_75_vs_100_outcome = ifelse(smooth.df.1$pyr_75_vs_100 > 0,
                                               yes = "75% Win",
                                               no = "100% Win")

smooth.df.1$pyr_50_vs_100_outcome = ifelse(smooth.df.1$pyr_50_vs_100 > 0,
                                               yes = "50% Win",
                                               no = "100% Win")


smooth.df.1$pyr_50_vs_75_outcome = ifelse(smooth.df.1$pyr_50_vs_75 > 0,
                                              yes = "50% Win",
                                              no = "75% Win")



smooth.df.1$total_75_vs_100 = (((smooth.df.0.75$diff.end.bioassay.pyr.mix + smooth.df.0.75$novel.mix.peak.bioassay)- (smooth.df.1$novel.mix.peak.bioassay +smooth.df.1$diff.end.bioassay.pyr.mix)) / (smooth.df.1$novel.mix.peak.bioassay +smooth.df.1$diff.end.bioassay.pyr.mix))*100
smooth.df.1$total_50_vs_100 = (((smooth.df.0.5$diff.end.bioassay.pyr.mix + smooth.df.0.5$novel.mix.peak.bioassay)- (smooth.df.1$novel.mix.peak.bioassay +smooth.df.1$diff.end.bioassay.pyr.mix)) / (smooth.df.1$novel.mix.peak.bioassay +smooth.df.1$diff.end.bioassay.pyr.mix))*100
smooth.df.1$total_50_vs_75 = (((smooth.df.0.5$diff.end.bioassay.pyr.mix + smooth.df.0.5$novel.mix.peak.bioassay)- (smooth.df.0.75$novel.mix.peak.bioassay +smooth.df.0.75$diff.end.bioassay.pyr.mix)) / (smooth.df.0.75$novel.mix.peak.bioassay +smooth.df.0.75$diff.end.bioassay.pyr.mix))*100


smooth.df.1$total_75_vs_100_outcome = ifelse(smooth.df.1$total_75_vs_100 > 0,
                                                 yes = "75% Win",
                                                 no = "100% Win")

smooth.df.1$total_50_vs_100_outcome = ifelse(smooth.df.1$total_50_vs_100 > 0,
                                                 yes = "50% Win",
                                                 no = "100% Win")


smooth.df.1$total_50_vs_75_outcome = ifelse(smooth.df.1$total_50_vs_75 > 0,
                                                yes = "50% Win",
                                                no = "75% Win")



#Colour Scheme:
#   "#e41a1c", #red = FD_FD
#   "#ff7f00",#orange = HD_HD 75%
#   "#984ea3", #purple = HD_HD 50%


novel_75_vs_100_plot = ggplot(smooth.df.1, aes(x=novel_75_vs_100,
                                                   fill = novel_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")


novel_50_vs_100_plot = ggplot(smooth.df.1, aes(x=novel_50_vs_100,
                                                   fill = novel_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

novel_50_vs_75_plot= ggplot(smooth.df.1, aes(x=novel_50_vs_75,
                                                 fill = novel_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

pyr_75_vs_100_plot = ggplot(smooth.df.1, aes(x=pyr_75_vs_100,
                                                 fill = pyr_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

pyr_50_vs_100_plot = ggplot(smooth.df.1, aes(x=pyr_50_vs_100,
                                                 fill = pyr_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

pyr_50_vs_75_plot= ggplot(smooth.df.1, aes(x=pyr_50_vs_75,
                                               fill = pyr_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

total_75_vs_100_plot = ggplot(smooth.df.1, aes(x=total_75_vs_100,
                                                   fill = total_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Total Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

total_50_vs_100_plot = ggplot(smooth.df.1, aes(x=total_50_vs_100,
                                                   fill = total_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Total Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

total_50_vs_75_plot= ggplot(smooth.df.1, aes(x=total_50_vs_75,
                                                 fill = total_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Total Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  theme_classic()+
  theme(legend.position = "none")

library(patchwork)

plot.layout = "
ADG
BEH
CFI
"


novel_75_vs_100_plot +
  novel_50_vs_100_plot +
  novel_50_vs_75_plot +
  pyr_75_vs_100_plot +
  pyr_50_vs_100_plot +
  pyr_50_vs_75_plot +
  total_75_vs_100_plot +
  total_50_vs_100_plot +
  total_50_vs_75_plot+
  plot_layout(design = plot.layout)+
  plot_annotation(title = "polysmooth")

rm(novel_75_vs_100_plot,
   novel_50_vs_100_plot,
   novel_50_vs_75_plot,
   pyr_75_vs_100_plot,
   pyr_50_vs_100_plot,
   pyr_50_vs_75_plot,
   total_75_vs_100_plot,
   total_50_vs_100_plot,
   total_50_vs_75_plot)


library(rpart)
library(rpart.plot)

reg.tree = rpart(total.outcome ~
                   heritability +
                   start.bioassay.pyrethroid +
                   female.exposure+
                   male.exposure+
                   dispersal+
                   intervention.coverage,
                 data = compare.fdfd.hdhd)




rpart.plot(reg.tree)

##Can we identify the areas where fd fd mixtures should not be used






##Also need to indentify where and how polysmooth and polytruncate differ from one another

compare.fdfd.hdhd.smooth = read.csv("compare.fdfd.hdhd.smooth.csv")
compare.fdfd.hdhd.truncation = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/compare.fdfd.hdhd.truncation.csv")

compare.fdfd.hdhd.smooth$same.outcome.overall = compare.fdfd.hdhd.smooth$total.outcome == compare.fdfd.hdhd.smooth$total.outcome
compare.fdfd.hdhd.smooth$same.outcome.novel = compare.fdfd.hdhd.smooth$novel.outcome == compare.fdfd.hdhd.smooth$novel.outcome
compare.fdfd.hdhd.smooth$same.outcome.pyrethroidl = compare.fdfd.hdhd.smooth$pyr.outcome == compare.fdfd.hdhd.smooth$pyr.outcome


table(compare.fdfd.hdhd.smooth$same.outcome.overall)# 28744 241256
table(compare.fdfd.hdhd.smooth$same.outcome.novel) # 15847 254153
table(compare.fdfd.hdhd.smooth$same.outcome.pyrethroidl) # 34276 235724

library(ggridges)

colnames(compare.fdfd.hdhd.smooth)


smooth.trunc.different.df = subset(compare.fdfd.hdhd.smooth, same.outcome.overall == FALSE)

smooth.trunc.different.df = smooth.trunc.different.df%>%
  dplyr::select("dispersal", "heritability", "male.exposure", "female.exposure", "intervention.coverage",
         "start.bioassay.pyrethroid", "base.decay.1", "base.decay.2", "threshold.gens.1")

hist(smooth.trunc.different.df$dispersal)
hist(smooth.trunc.different.df$heritability)
hist(smooth.trunc.different.df$male.exposure)####
hist(smooth.trunc.different.df$female.exposure)####
hist(smooth.trunc.different.df$start.bioassay.pyrethroid)#####


ggplot(compare.fdfd.hdhd.smooth, aes(x=as.factor(start.bioassay.pyrethroid),
                                     fill=same.outcome.overall))+
  geom_bar(stat = "count",
           position = "dodge")+
  xlab("Initial Pyrethroid Bioassay Survival (%)")+
  scale_fill_manual(values = c("#d53e4f", "#3288bd"))+
  ylab("Frequency")+
  guides(fill=guide_legend(title="Same Outcome"))+
  theme_bw()+
  theme(legend.position = "bottom")


#Compare operational outcome
temp.df = data.frame(outcome.smooth = compare.fdfd.hdhd.smooth$total.outcome,
                     outcome.smooth = compare.fdfd.hdhd.smooth$total.outcome )


temp.df.2 = data.frame(table(temp.df))
print(temp.df.2)

temp.df.2$Percentage = (temp.df.2$Freq / 270000) * 100

temp.df.2


