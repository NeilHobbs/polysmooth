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
smooth.df = dplyr::bind_rows(smooth.scaled.df,
                             set.75s)

##have only 1:1 ; 0.75:0.75 and 0.5:0.5 simulations

smooth.df.1 = subset(smooth.scaled.df, dose.1 == 1 &
                       dose.2 == 1)

smooth.df.0.75 = subset(smooth.df, dose.1 == 0.75 &
                          dose.2 == 0.75)

smooth.df.0.5 = subset(smooth.df, dose.1 == 0.5 &
                         dose.2 == 0.5)



smooth.scaled.df = rbind(smooth.df.1,
                         smooth.df.0.75,
                         smooth.df.0.5)
rm(smooth.df)


##Now read in polytruncate data::

set.1 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.set.1.csv")
set.2 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.set.2.csv")
set.3 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.set.3.csv")
set.4 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.set.4.csv")
set.5 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.set.5.csv")
set.6 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.set.6.csv")
set.7 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.set.7.csv")
set.8 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.set.8.csv")
set.9 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.set.9.csv")
set.10 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.set.10.csv")

truncation.scaled.df = rbind(set.1,
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
truncation.scaled.df = subset(truncation.scaled.df, threshold.gens == threshold.gens.1)

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


set.1 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.75efficacy.set.1.csv")
set.2 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.75efficacy.set.2.csv")
set.3 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.75efficacy.set.3.csv")
set.4 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.75efficacy.set.4.csv")
set.5 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.75efficacy.set.5.csv")
set.6 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.75efficacy.set.6.csv")
set.7 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.75efficacy.set.7.csv")
set.8 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.75efficacy.set.8.csv")
set.9 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.75efficacy.set.9.csv")
set.10 = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/mixtures.truncation.75efficacy.set.10.csv")

set75s = rbind(set.1,
               set.2,
               set.3,
               set.4,
               set.5,
               set.6,
               set.7,
               set.8,
               set.9,
               set.10)
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


truncation.df.1 = subset(truncation.scaled.df, dose.1 == 1 &
                       dose.2 == 1)


set75s$novel.solo = truncation.df.1$novel.solo
set75s$pyrethroid.solo = truncation.df.1$pyrethroid.solo

truncation.scaled.df = truncation.scaled.df%>%
  dplyr::select(-"threshold.gens.1")

truncation.df = dplyr::bind_rows(truncation.scaled.df,
                             set75s)

truncation.df.1 = subset(truncation.scaled.df, dose.1 == 1 &
                       dose.2 == 1)

truncation.df.0.75 = subset(truncation.df, dose.1 == 0.75 &
                          dose.2 == 0.75)

truncation.df.0.5 = subset(truncation.df, dose.1 == 0.5 &
                         dose.2 == 0.5)



truncation.scaled.df = rbind(truncation.df.1,
                         truncation.df.0.75,
                         truncation.df.0.5)
rm(truncation.df)
rm(set75s)



#########

smooth.scaled.df$change.novel.peak = smooth.scaled.df$novel.solo - smooth.scaled.df$mixture.novel
smooth.scaled.df$change.pyrethroid.peak = smooth.scaled.df$pyrethroid.solo - smooth.scaled.df$mixture.pyrethroid

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




smooth.scaled.df$change.novel.peak.bioassay = smooth.scaled.df$novel.solo.peak.bioassay - smooth.scaled.df$novel.mix.peak.bioassay
smooth.scaled.df$change.pyrethroid.peak.bioassay = smooth.scaled.df$pyrethroid.solo.peak.bioassay - smooth.scaled.df$pyrethroid.mix.peak.bioassay


smooth.scaled.df$dosing.strategy = ifelse(smooth.scaled.df$dose.1 == 1 &
                                            smooth.scaled.df$dose.2 == 1,
                                          yes = "FD_FD",
                                          no = ifelse(smooth.scaled.df$dose.1 == 1 &
                                                        smooth.scaled.df$dose.2 == 0.5,
                                                      yes = "FD_HD",
                                                      no = ifelse(smooth.scaled.df$dose.1 == 0.5 &
                                                                    smooth.scaled.df$dose.2 == 1,
                                                                  yes = "HD_FD",
                                                                  no = "HD_HD")))





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

smooth.scaled.df$end.difference.pyrethroid = smooth.scaled.df$pyrethroid.mix.peak.bioassay - (smooth.scaled.df$start.bioassay.pyrethroid / 100)

###############END Polysmooth########################
truncation.scaled.df$change.novel.peak = truncation.scaled.df$novel.solo - truncation.scaled.df$mixture.novel
truncation.scaled.df$change.pyrethroid.peak = truncation.scaled.df$pyrethroid.solo - truncation.scaled.df$mixture.pyrethroid

truncation.scaled.df$start.bioassay.pyrethroid = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                               trait.mean = truncation.scaled.df$start.resistance.2,
                                                                                               half.population.bioassay.survival.resistance = 900,
                                                                                               michaelis.menten.slope = 1)*100

truncation.scaled.df$novel.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = truncation.scaled.df$novel.solo,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)


truncation.scaled.df$pyrethroid.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                   trait.mean = truncation.scaled.df$pyrethroid.solo,
                                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                                   michaelis.menten.slope = 1)



truncation.scaled.df$novel.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                             trait.mean = truncation.scaled.df$mixture.novel,
                                                                                             half.population.bioassay.survival.resistance = 900,
                                                                                             michaelis.menten.slope = 1)



truncation.scaled.df$pyrethroid.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                                  trait.mean = truncation.scaled.df$mixture.pyrethroid,
                                                                                                  half.population.bioassay.survival.resistance = 900,
                                                                                                  michaelis.menten.slope = 1)




truncation.scaled.df$change.novel.peak.bioassay = truncation.scaled.df$novel.solo.peak.bioassay - truncation.scaled.df$novel.mix.peak.bioassay
truncation.scaled.df$change.pyrethroid.peak.bioassay = truncation.scaled.df$pyrethroid.solo.peak.bioassay - truncation.scaled.df$pyrethroid.mix.peak.bioassay


truncation.scaled.df$dosing.strategy = ifelse(truncation.scaled.df$dose.1 == 1 &
                                                truncation.scaled.df$dose.2 == 1,
                                              yes = "FD_FD",
                                              no = ifelse(truncation.scaled.df$dose.1 == 1 &
                                                            truncation.scaled.df$dose.2 == 0.5,
                                                          yes = "FD_HD",
                                                          no = ifelse(truncation.scaled.df$dose.1 == 0.5 &
                                                                        truncation.scaled.df$dose.2 == 1,
                                                                      yes = "HD_FD",
                                                                      no = "HD_HD")))





truncation.scaled.df$decay.rate = factor(ifelse(truncation.scaled.df$base.decay.1 == 0.025 &
                                                  truncation.scaled.df$base.decay.2 == 0.005,
                                                yes = "much faster",
                                                no = ifelse(truncation.scaled.df$base.decay.1 == 0.025 &
                                                              truncation.scaled.df$base.decay.2 == 0.015,
                                                            yes = "faster",
                                                            no = ifelse(truncation.scaled.df$base.decay.1 == 0.015 &
                                                                          truncation.scaled.df$base.decay.2 == 0.005,
                                                                        yes = "faster",
                                                                        no = ifelse(truncation.scaled.df$base.decay.1 == 0.005 &
                                                                                      truncation.scaled.df$base.decay.2 == 0.025,
                                                                                    yes = "much slower",
                                                                                    no = ifelse(truncation.scaled.df$base.decay.1 == 0.005 &
                                                                                                  truncation.scaled.df$base.decay.2 == 0.015,
                                                                                                yes = "slower",
                                                                                                no = ifelse(truncation.scaled.df$base.decay.1 == 0.015 &
                                                                                                              truncation.scaled.df$base.decay.2 == 0.025,
                                                                                                            yes = "slower",
                                                                                                            no = ifelse(truncation.scaled.df$base.decay.1 == truncation.scaled.df$base.decay.2,
                                                                                                                        yes = "same",
                                                                                                                        no = NA))))))), levels = c("much faster", "faster", "same", "slower", "much slower"))




#calculate changes in rates of evolution:::
truncation.scaled.df$novel.solo.rate = truncation.scaled.df$novel.solo.peak.bioassay/200
truncation.scaled.df$pyrethroid.solo.rate = (truncation.scaled.df$pyrethroid.solo.peak.bioassay - (truncation.scaled.df$start.bioassay.pyrethroid/100))/200
truncation.scaled.df$novel.mix.rate = truncation.scaled.df$novel.mix.peak.bioassay/200
truncation.scaled.df$pyrethroid.mix.rate = (truncation.scaled.df$pyrethroid.mix.peak.bioassay - (truncation.scaled.df$start.bioassay.pyrethroid/100))/200

truncation.scaled.df$rate.change.novel.percent = ((truncation.scaled.df$novel.mix.rate - truncation.scaled.df$novel.solo.rate)/truncation.scaled.df$novel.solo.rate) * 100
truncation.scaled.df$rate.change.pyrethroid.percent = ((truncation.scaled.df$pyrethroid.mix.rate - truncation.scaled.df$pyrethroid.solo.rate)/truncation.scaled.df$pyrethroid.solo.rate) * 100


#First simply look at the impact of only Dosing / Resistance / Decay

truncation.scaled.df$novel.rate.change = ifelse(truncation.scaled.df$rate.change.novel.percent > 0,
                                                yes = "faster",
                                                no = ifelse(truncation.scaled.df$rate.change.novel.percent < 0,
                                                            yes = "slower",
                                                            no = "no change"))

truncation.scaled.df$pyrethroid.rate.change = ifelse(truncation.scaled.df$rate.change.pyrethroid.percent > 0,
                                                     yes = "faster",
                                                     no = ifelse(truncation.scaled.df$rate.change.pyrethroid.percent < 0,
                                                                 yes = "slower",
                                                                 no = "no change"))


truncation.scaled.df$end.difference.pyrethroid = truncation.scaled.df$pyrethroid.mix.peak.bioassay - (truncation.scaled.df$start.bioassay.pyrethroid / 100)

#re subset:::
smooth.df.1 = subset(smooth.scaled.df, dose.1 == 1 &
                       dose.2 == 1)

smooth.df.0.75 = subset(smooth.scaled.df, dose.1 == 0.75 &
                          dose.2 == 0.75)

smooth.df.0.5 = subset(smooth.scaled.df, dose.1 == 0.5 &
                         dose.2 == 0.5)



truncation.df.1 = subset(truncation.scaled.df, dose.1 == 1 &
                           dose.2 == 1)

truncation.df.0.75 = subset(truncation.scaled.df, dose.1 == 0.75 &
                              dose.2 == 0.75)

truncation.df.0.5 = subset(truncation.scaled.df, dose.1 == 0.5 &
                             dose.2 == 0.5)


#####
#Rank the dosing strategies.
smooth.df.1$smooth.outcome.novel = ifelse(smooth.df.1$novel.mix.peak.bioassay <
                                smooth.df.0.75$novel.mix.peak.bioassay &
                                smooth.df.1$novel.mix.peak.bioassay <
                                smooth.df.0.5$novel.mix.peak.bioassay,
                              yes = "Full Dose",

                              no = ifelse(smooth.df.0.75$novel.mix.peak.bioassay <
                                            smooth.df.1$novel.mix.peak.bioassay &
                                            smooth.df.0.75$novel.mix.peak.bioassay <
                                            smooth.df.0.5$novel.mix.peak.bioassay,
                                          yes = "75 Efficacy Dose",
                                          no = ifelse(smooth.df.0.5$novel.mix.peak.bioassay <
                                                        smooth.df.1$novel.mix.peak.bioassay &
                                                        smooth.df.0.5$novel.mix.peak.bioassay <
                                                        smooth.df.0.75$novel.mix.peak.bioassay,
                                                      yes = "50 Efficacy Dose",
                                                      no = NA)))



smooth.df.1$smooth.outcome.pyrethroid = ifelse(smooth.df.1$pyrethroid.mix.peak.bioassay <
                                            smooth.df.0.75$pyrethroid.mix.peak.bioassay &
                                            smooth.df.1$pyrethroid.mix.peak.bioassay <
                                            smooth.df.0.5$pyrethroid.mix.peak.bioassay,
                                          yes = "Full Dose",

                                          no = ifelse(smooth.df.0.75$pyrethroid.mix.peak.bioassay <
                                                        smooth.df.1$pyrethroid.mix.peak.bioassay &
                                                        smooth.df.0.75$pyrethroid.mix.peak.bioassay <
                                                        smooth.df.0.5$pyrethroid.mix.peak.bioassay,
                                                      yes = "75 Efficacy Dose",
                                                      no = ifelse(smooth.df.0.5$pyrethroid.mix.peak.bioassay <
                                                                    smooth.df.1$pyrethroid.mix.peak.bioassay &
                                                                    smooth.df.0.5$pyrethroid.mix.peak.bioassay <
                                                                    smooth.df.0.75$pyrethroid.mix.peak.bioassay,
                                                                  yes = "50 Efficacy Dose",
                                                                  no = NA)))


smooth.df.1$smooth.outcome.total = ifelse(smooth.df.1$novel.mix.peak.bioassay + smooth.df.1$end.difference.pyrethroid<
                                            smooth.df.0.75$novel.mix.peak.bioassay + smooth.df.0.75$end.difference.pyrethroid&
                                            smooth.df.1$novel.mix.peak.bioassay + smooth.df.1$end.difference.pyrethroid<
                                            smooth.df.0.5$novel.mix.peak.bioassay + smooth.df.0.5$end.difference.pyrethroid,
                                          yes = "Full Dose",

                                          no = ifelse(smooth.df.0.75$novel.mix.peak.bioassay+ smooth.df.0.75$end.difference.pyrethroid <
                                                        smooth.df.1$novel.mix.peak.bioassay+ smooth.df.1$end.difference.pyrethroid&
                                                        smooth.df.0.75$novel.mix.peak.bioassay + smooth.df.0.75$end.difference.pyrethroid<
                                                        smooth.df.0.5$novel.mix.peak.bioassay+ smooth.df.0.5$end.difference.pyrethroid,
                                                      yes = "75 Efficacy Dose",
                                                      no = ifelse(smooth.df.0.5$novel.mix.peak.bioassay + smooth.df.0.5$end.difference.pyrethroid<
                                                                    smooth.df.1$novel.mix.peak.bioassay + smooth.df.1$end.difference.pyrethroid&
                                                                    smooth.df.0.5$novel.mix.peak.bioassay + smooth.df.0.5$end.difference.pyrethroid<
                                                                    smooth.df.0.75$novel.mix.peak.bioassay+ smooth.df.0.75$end.difference.pyrethroid,
                                                                  yes = "50 Efficacy Dose",
                                                                  no = NA)))
###And Truncation

smooth.df.1$truncation.outcome.novel = ifelse(truncation.df.1$novel.mix.peak.bioassay <
                                                truncation.df.0.75$novel.mix.peak.bioassay &
                                              truncation.df.1$novel.mix.peak.bioassay <
                                           truncation.df.0.5$novel.mix.peak.bioassay,
                                          yes = "Full Dose",

                                          no = ifelse(truncation.df.0.75$novel.mix.peak.bioassay <
                                                        truncation.df.1$novel.mix.peak.bioassay &
                                                        truncation.df.0.75$novel.mix.peak.bioassay <
                                                        truncation.df.0.5$novel.mix.peak.bioassay,
                                                      yes = "75 Efficacy Dose",
                                                      no = ifelse(truncation.df.0.5$novel.mix.peak.bioassay <
                                                                    truncation.df.1$novel.mix.peak.bioassay &
                                                                    truncation.df.0.5$novel.mix.peak.bioassay <
                                                                    truncation.df.0.75$novel.mix.peak.bioassay,
                                                                  yes = "50 Efficacy Dose",
                                                                  no = NA)))



smooth.df.1$truncation.outcome.pyrethroid = ifelse(truncation.df.1$pyrethroid.mix.peak.bioassay <
                                                 truncation.df.0.75$pyrethroid.mix.peak.bioassay &
                                                   truncation.df.1$pyrethroid.mix.peak.bioassay <
                                                   truncation.df.0.5$pyrethroid.mix.peak.bioassay,
                                               yes = "Full Dose",

                                               no = ifelse(truncation.df.0.75$pyrethroid.mix.peak.bioassay <
                                                             truncation.df.1$pyrethroid.mix.peak.bioassay &
                                                             truncation.df.0.75$pyrethroid.mix.peak.bioassay <
                                                             truncation.df.0.5$pyrethroid.mix.peak.bioassay,
                                                           yes = "75 Efficacy Dose",
                                                           no = ifelse(truncation.df.0.5$pyrethroid.mix.peak.bioassay <
                                                                         truncation.df.1$pyrethroid.mix.peak.bioassay &
                                                                         truncation.df.0.5$pyrethroid.mix.peak.bioassay <
                                                                         truncation.df.0.75$pyrethroid.mix.peak.bioassay,
                                                                       yes = "50 Efficacy Dose",
                                                                       no = NA)))


smooth.df.1$truncation.outcome.total = ifelse(truncation.df.1$novel.mix.peak.bioassay + truncation.df.1$end.difference.pyrethroid<
                                            truncation.df.0.75$novel.mix.peak.bioassay + truncation.df.0.75$end.difference.pyrethroid&
                                            truncation.df.1$novel.mix.peak.bioassay + truncation.df.1$end.difference.pyrethroid<
                                            truncation.df.0.5$novel.mix.peak.bioassay + truncation.df.0.5$end.difference.pyrethroid,
                                          yes = "Full Dose",

                                          no = ifelse(truncation.df.0.75$novel.mix.peak.bioassay+ truncation.df.0.75$end.difference.pyrethroid <
                                                        truncation.df.1$novel.mix.peak.bioassay+ truncation.df.1$end.difference.pyrethroid&
                                                        truncation.df.0.75$novel.mix.peak.bioassay + truncation.df.0.75$end.difference.pyrethroid<
                                                        truncation.df.0.5$novel.mix.peak.bioassay+ truncation.df.0.5$end.difference.pyrethroid,
                                                      yes = "75 Efficacy Dose",
                                                      no = ifelse(truncation.df.0.5$novel.mix.peak.bioassay + truncation.df.0.5$end.difference.pyrethroid<
                                                                    truncation.df.1$novel.mix.peak.bioassay + truncation.df.1$end.difference.pyrethroid&
                                                                    truncation.df.0.5$novel.mix.peak.bioassay + truncation.df.0.5$end.difference.pyrethroid<
                                                                    truncation.df.0.75$novel.mix.peak.bioassay+ truncation.df.0.75$end.difference.pyrethroid,
                                                                  yes = "50 Efficacy Dose",
                                                                  no = NA)))

##Same outcomes::

smooth.df.1$truncation.smooth.novel = ifelse(smooth.df.1$truncation.outcome.novel == smooth.df.1$smooth.outcome.novel,
                                 yes = TRUE,
                                 no = FALSE)

smooth.df.1$truncation.smooth.pyrethroid = ifelse(smooth.df.1$truncation.outcome.pyrethroid == smooth.df.1$smooth.outcome.novel,
                                 yes = TRUE,
                                 no = FALSE)

smooth.df.1$truncation.smooth.total = ifelse(smooth.df.1$truncation.outcome.total == smooth.df.1$smooth.outcome.novel,
                                 yes = TRUE,
                                 no = FALSE)


