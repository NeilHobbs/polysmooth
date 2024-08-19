###Comparing Polysmooth and Polytruncate:::
#######
#Read in the datasets:::
    ##Polysmooth
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

#and the 75% efficacy sets:::
set.1.75 = read.csv(".//75_efficacy_mixtures.smooth.set.1.csv")
set.2.75 = read.csv(".//75_efficacy_mixtures.smooth.set.2.csv")
set.3.75 = read.csv(".//75_efficacy_mixtures.smooth.set.3.csv")
set.4.75 = read.csv(".//75_efficacy_mixtures.smooth.set.4.csv")
set.5.75 = read.csv(".//75_efficacy_mixtures.smooth.set.5.csv")
set.6.75 = read.csv(".//75_efficacy_mixtures.smooth.set.6.csv")
set.7.75 = read.csv(".//75_efficacy_mixtures.smooth.set.7.csv")
set.8.75 = read.csv(".//75_efficacy_mixtures.smooth.set.8.csv")
set.9.75 = read.csv(".//75_efficacy_mixtures.smooth.set.9.csv")
set.10.75 = read.csv(".//75_efficacy_mixtures.smooth.set.10.csv")


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

##Issue with the naming of the variables --> make sure it is just where threshold gens are the same for all sims
smooth.scaled.df = subset(smooth.scaled.df, threshold.gens == threshold.gens.1)

A=(subset(smooth.scaled.df, dose.1 == 1 & dose.2 == 1))

smooth.scaled.df.75 = rbind(set.1.75,
                            set.2.75,
                            set.3.75,
                            set.4.75,
                            set.5.75,
                            set.6.75,
                            set.7.75,
                            set.8.75,
                            set.9.75,
                            set.10.75)

#add in solo deployments
smooth.scaled.df.75$novel.solo = A$novel.solo
smooth.scaled.df.75$pyrethroid.solo = A$pyrethroid.solo

rm(A)

smooth.scaled.df = smooth.scaled.df%>%
  dplyr::select(-"threshold.gens.1")

smooth.scaled.df = rbind(smooth.scaled.df,
                         smooth.scaled.df.75)


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
   set.10,
   set.1.75,
   set.2.75,
   set.3.75,
   set.4.75,
   set.5.75,
   set.6.75,
   set.7.75,
   set.8.75,
   set.9.75,
   set.10.75,
   smooth.scaled.df.75)

  #Polytruncate:
set.1 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.1.csv")
set.2 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.2.csv")
set.3 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.3.csv")
set.4 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.4.csv")
set.5 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.5.csv")
set.6 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.6.csv")
set.7 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.7.csv")
set.8 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.8.csv")
set.9 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.9.csv")
set.10 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.10.csv")

truncate.scaled.df = rbind(set.1,
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
truncate.scaled.df = subset(truncate.scaled.df, threshold.gens == threshold.gens.1)

A=(subset(truncate.scaled.df, dose.1 == 1 & dose.2 == 1))

#and the 75% efficacy sets:
set.1.75 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.1.csv")
set.2.75 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.2.csv")
set.3.75 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.3.csv")
set.4.75 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.4.csv")
set.5.75 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.5.csv")
set.6.75 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.6.csv")
set.7.75 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.7.csv")
set.8.75 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.8.csv")
set.9.75 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.9.csv")
set.10.75 = read.csv("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.10.csv")

truncate.scaled.df.75 = rbind(set.1.75,
                            set.2.75,
                            set.3.75,
                            set.4.75,
                            set.5.75,
                            set.6.75,
                            set.7.75,
                            set.8.75,
                            set.9.75,
                            set.10.75)

#add in solo deployments
truncate.scaled.df.75$novel.solo = A$novel.solo
truncate.scaled.df.75$pyrethroid.solo = A$pyrethroid.solo

rm(A)

truncate.scaled.df = truncate.scaled.df%>%
  dplyr::select(-"threshold.gens.1")

truncation.scaled.df = rbind(truncate.scaled.df,
                           truncate.scaled.df.75)

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
   set.10,
   set.1.75,
   set.2.75,
   set.3.75,
   set.4.75,
   set.5.75,
   set.6.75,
   set.7.75,
   set.8.75,
   set.9.75,
   set.10.75)
rm(truncate.scaled.df,
   truncate.scaled.df.75)

library(devtools)
load_all()

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





##########
#comparing polysmooth and polytruncate.

smooth.scaled.df.1 = subset(smooth.scaled.df, dose.1 == 1 &
                              dose.2 == 1)
truncation.scaled.df.1 = subset(truncation.scaled.df, dose.1 == 1 &
                              dose.2 == 1)

smooth.scaled.df.0.75 = subset(smooth.scaled.df, dose.1 == 0.75 &
                                dose.2 == 0.75)

truncation.scaled.df.0.75  = subset(truncation.scaled.df, dose.1 == 0.75 &
                                     dose.2 == 0.75)

smooth.scaled.df.0.5 = subset(smooth.scaled.df, dose.1 == 0.5 &
                              dose.2 == 0.5)
truncation.scaled.df.0.5  = subset(truncation.scaled.df, dose.1 == 0.5 &
                                  dose.2 == 0.5)


smooth.scaled.df.1$total.mix.peak.bioassay = smooth.scaled.df.1$novel.mix.peak.bioassay + smooth.scaled.df.1$pyrethroid.mix.peak.bioassay
truncation.scaled.df.1$total.mix.peak.bioassay = truncation.scaled.df.1$novel.mix.peak.bioassay + truncation.scaled.df.1$pyrethroid.mix.peak.bioassay

smooth.scaled.df.0.75$total.mix.peak.bioassay = smooth.scaled.df.0.75$novel.mix.peak.bioassay + smooth.scaled.df.0.75$pyrethroid.mix.peak.bioassay
truncation.scaled.df.0.75$total.mix.peak.bioassay = truncation.scaled.df.0.75$novel.mix.peak.bioassay + truncation.scaled.df.0.75$pyrethroid.mix.peak.bioassay


smooth.scaled.df.0.5$total.mix.peak.bioassay = smooth.scaled.df.0.5$novel.mix.peak.bioassay + smooth.scaled.df.0.5$pyrethroid.mix.peak.bioassay
truncation.scaled.df.0.5$total.mix.peak.bioassay = truncation.scaled.df.0.5$novel.mix.peak.bioassay + truncation.scaled.df.0.5$pyrethroid.mix.peak.bioassay

###Percentage differences.
#1 vs 0.5
smooth.percent.diff.novel.1v0.5 = (smooth.scaled.df.1$novel.mix.peak.bioassay - smooth.scaled.df.0.5$novel.mix.peak.bioassay)/smooth.scaled.df.0.5$novel.mix.peak.bioassay * 100
smooth.percent.diff.pyrethroid.1v0.5 = ((smooth.scaled.df.1$pyrethroid.mix.peak.bioassay)- (smooth.scaled.df.0.5$pyrethroid.mix.peak.bioassay))/(smooth.scaled.df.0.5$pyrethroid.mix.peak.bioassay) * 100
smooth.percent.diff.total.1v0.5 = (smooth.scaled.df.1$total.mix.peak.bioassay - smooth.scaled.df.0.5$total.mix.peak.bioassay)/smooth.scaled.df.0.5$total.mix.peak.bioassay * 100
truncation.percent.diff.novel.1v0.5 = (truncation.scaled.df.1$novel.mix.peak.bioassay - truncation.scaled.df.0.5$novel.mix.peak.bioassay)/truncation.scaled.df.0.5$novel.mix.peak.bioassay * 100
truncation.percent.diff.pyrethroid.1v0.5 = ((truncation.scaled.df.1$pyrethroid.mix.peak.bioassay)- (truncation.scaled.df.0.5$pyrethroid.mix.peak.bioassay))/(truncation.scaled.df.0.5$pyrethroid.mix.peak.bioassay) * 100
truncation.percent.diff.total.1v0.5 = (truncation.scaled.df.1$total.mix.peak.bioassay - truncation.scaled.df.0.5$total.mix.peak.bioassay)/truncation.scaled.df.0.5$total.mix.peak.bioassay * 100

smooth.outcome.novel.1v0.5 = ifelse(smooth.percent.diff.novel.1v0.5 < 0,
                             yes = "Full Dose",
                              no = "Half Dose retains 50%")
truncation.outcome.novel.1v0.5 = ifelse(truncation.percent.diff.novel.1v0.5 < 0,
                                  yes = "Full Dose",
                                  no = "Half Dose retains 50%")
smooth.outcome.pyrethroid.1v0.5 = ifelse(smooth.percent.diff.pyrethroid.1v0.5 < 0,
                                   yes = "Full Dose",
                                   no = "Half Dose retains 50%")
truncation.outcome.pyrethroid.1v0.5 = ifelse(truncation.percent.diff.pyrethroid.1v0.5 < 0,
                                       yes = "Full Dose",
                                       no = "Half Dose retains 50%")
smooth.outcome.total.1v0.5 = ifelse(smooth.percent.diff.total.1v0.5 < 0,
                              yes = "Full Dose",
                              no = "Half Dose retains 50%")
truncation.outcome.total.1v0.5 = ifelse(truncation.percent.diff.total.1v0.5 < 0,
                                  yes = "Full Dose",
                                  no = "Half Dose retains 50%")


#1 vs 0.75
smooth.percent.diff.novel.1v0.75 = (smooth.scaled.df.1$novel.mix.peak.bioassay - smooth.scaled.df.0.75$novel.mix.peak.bioassay)/smooth.scaled.df.0.75$novel.mix.peak.bioassay * 100
smooth.percent.diff.pyrethroid.1v0.75 = ((smooth.scaled.df.1$pyrethroid.mix.peak.bioassay)- (smooth.scaled.df.0.75$pyrethroid.mix.peak.bioassay))/(smooth.scaled.df.0.75$pyrethroid.mix.peak.bioassay) * 100
smooth.percent.diff.total.1v0.75 = (smooth.scaled.df.1$total.mix.peak.bioassay - smooth.scaled.df.0.75$total.mix.peak.bioassay)/smooth.scaled.df.0.75$total.mix.peak.bioassay * 100
truncation.percent.diff.novel.1v0.75 = (truncation.scaled.df.1$novel.mix.peak.bioassay - truncation.scaled.df.0.75$novel.mix.peak.bioassay)/truncation.scaled.df.0.75$novel.mix.peak.bioassay * 100
truncation.percent.diff.pyrethroid.1v0.75 = ((truncation.scaled.df.1$pyrethroid.mix.peak.bioassay)- (truncation.scaled.df.0.75$pyrethroid.mix.peak.bioassay))/(truncation.scaled.df.0.75$pyrethroid.mix.peak.bioassay) * 100
truncation.percent.diff.total.1v0.75 = (truncation.scaled.df.1$total.mix.peak.bioassay - truncation.scaled.df.0.75$total.mix.peak.bioassay)/truncation.scaled.df.0.75$total.mix.peak.bioassay * 100

smooth.outcome.novel.1v0.75 = ifelse(smooth.percent.diff.novel.1v0.75 < 0,
                                    yes = "Full Dose",
                                    no = "Half Dose retains 75%")
truncation.outcome.novel.1v0.75 = ifelse(truncation.percent.diff.novel.1v0.75 < 0,
                                        yes = "Full Dose",
                                        no = "Half Dose retains 75%")
smooth.outcome.pyrethroid.1v0.75 = ifelse(smooth.percent.diff.pyrethroid.1v0.75 < 0,
                                         yes = "Full Dose",
                                         no = "Half Dose retains 75%")
truncation.outcome.pyrethroid.1v0.75 = ifelse(truncation.percent.diff.pyrethroid.1v0.75 < 0,
                                             yes = "Full Dose",
                                             no = "Half Dose retains 75%")
smooth.outcome.total.1v0.75 = ifelse(smooth.percent.diff.total.1v0.75 < 0,
                                    yes = "Full Dose",
                                    no = "Half Dose retains 75%")
truncation.outcome.total.1v0.75 = ifelse(truncation.percent.diff.total.1v0.75 < 0,
                                        yes = "Full Dose",
                                        no = "Half Dose retains 75%")

#0.75 v 0.5
smooth.percent.diff.novel.0.75v0.5 = (smooth.scaled.df.0.75$novel.mix.peak.bioassay - smooth.scaled.df.0.5$novel.mix.peak.bioassay)/smooth.scaled.df.0.5$novel.mix.peak.bioassay * 100
smooth.percent.diff.pyrethroid.0.75v0.5 = ((smooth.scaled.df.0.75$pyrethroid.mix.peak.bioassay)- (smooth.scaled.df.0.5$pyrethroid.mix.peak.bioassay))/(smooth.scaled.df.0.5$pyrethroid.mix.peak.bioassay) * 100
smooth.percent.diff.total.0.75v0.5 = (smooth.scaled.df.0.75$total.mix.peak.bioassay - smooth.scaled.df.0.5$total.mix.peak.bioassay)/smooth.scaled.df.0.5$total.mix.peak.bioassay * 100
truncation.percent.diff.novel.0.75v0.5 = (truncation.scaled.df.0.75$novel.mix.peak.bioassay - truncation.scaled.df.0.5$novel.mix.peak.bioassay)/truncation.scaled.df.0.5$novel.mix.peak.bioassay * 100
truncation.percent.diff.pyrethroid.0.75v0.5 = ((truncation.scaled.df.0.75$pyrethroid.mix.peak.bioassay)- (truncation.scaled.df.0.5$pyrethroid.mix.peak.bioassay))/(truncation.scaled.df.0.5$pyrethroid.mix.peak.bioassay) * 100
truncation.percent.diff.total.0.75v0.5 = (truncation.scaled.df.0.75$total.mix.peak.bioassay - truncation.scaled.df.0.5$total.mix.peak.bioassay)/truncation.scaled.df.0.5$total.mix.peak.bioassay * 100

smooth.outcome.novel.0.75v0.5 = ifelse(smooth.percent.diff.novel.0.75v0.5 < 0,
                                    yes = "Half Dose retains 75%",
                                    no = "Half Dose retains 50%")
truncation.outcome.novel.0.75v0.5 = ifelse(truncation.percent.diff.novel.0.75v0.5 < 0,
                                        yes = "Half Dose retains 75%",
                                        no = "Half Dose retains 50%")
smooth.outcome.pyrethroid.0.75v0.5 = ifelse(smooth.percent.diff.pyrethroid.0.75v0.5 < 0,
                                         yes = "Half Dose retains 75%",
                                         no = "Half Dose retains 50%")
truncation.outcome.pyrethroid.0.75v0.5 = ifelse(truncation.percent.diff.pyrethroid.0.75v0.5 < 0,
                                             yes = "Half Dose retains 75%",
                                             no = "Half Dose retains 50%")
smooth.outcome.total.0.75v0.5 = ifelse(smooth.percent.diff.total.0.75v0.5 < 0,
                                    yes = "Half Dose retains 75%",
                                    no = "Half Dose retains 50%")
truncation.outcome.total.0.75v0.5 = ifelse(truncation.percent.diff.total.0.75v0.5 < 0,
                                        yes = "Half Dose retains 75%",
                                        no = "Half Dose retains 50%")






df = data.frame(smooth.percent.diff.novel.1v0.5,
                smooth.percent.diff.pyrethroid.1v0.5,
                truncation.percent.diff.novel.1v0.5,
                truncation.percent.diff.pyrethroid.1v0.5,
                smooth.outcome.novel.1v0.5,
                truncation.outcome.novel.1v0.5,
                smooth.outcome.pyrethroid.1v0.5,
                truncation.outcome.pyrethroid.1v0.5,
                smooth.percent.diff.total.1v0.5,
                truncation.percent.diff.total.1v0.5,
                smooth.outcome.total.1v0.5,
                truncation.outcome.total.1v0.5,
                smooth.percent.diff.novel.1v0.75,
                smooth.percent.diff.pyrethroid.1v0.75,
                truncation.percent.diff.novel.1v0.75,
                truncation.percent.diff.pyrethroid.1v0.75,
                smooth.outcome.novel.1v0.75,
                truncation.outcome.novel.1v0.75,
                smooth.outcome.pyrethroid.1v0.75,
                truncation.outcome.pyrethroid.1v0.75,
                smooth.percent.diff.total.1v0.75,
                truncation.percent.diff.total.1v0.75,
                smooth.outcome.total.1v0.75,
                truncation.outcome.total.1v0.75,
                smooth.percent.diff.pyrethroid.0.75v0.5,
                truncation.percent.diff.novel.0.75v0.5,
                truncation.percent.diff.pyrethroid.0.75v0.5,
                smooth.outcome.novel.0.75v0.5,
                truncation.outcome.novel.0.75v0.5,
                smooth.outcome.pyrethroid.0.75v0.5,
                truncation.outcome.pyrethroid.0.75v0.5,
                smooth.percent.diff.total.0.75v0.5,
                truncation.percent.diff.total.0.75v0.5,
                smooth.outcome.total.0.75v0.5,
                truncation.outcome.total.0.75v0.5)

parameters.df = smooth.scaled.df.1[ , c(4,5,6 , 9, 11, 13:17, 20, 28)]
comparison.df = data.frame(df, parameters.df)

comparison.df$initial.pyrethroid.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                            trait.mean = comparison.df$start.resistance.2,
                                                                                            michaelis.menten.slope = 1,
                                                                                            half.population.bioassay.survival.resistance = 900)

library(dplyr)
library(patchwork)

#compare 1 vs 0.5
comparison.df$compare.1vs0.5.novel = ifelse(comparison.df$smooth.outcome.novel.1v0.5 == comparison.df$truncation.outcome.novel.1v0.5,
                              yes = TRUE, no = FALSE)
comparison.df$compare.1vs0.5.pyrethroid = ifelse(comparison.df$smooth.outcome.pyrethroid.1v0.5 == comparison.df$truncation.outcome.pyrethroid.1v0.5,
                                            yes = TRUE, no = FALSE)
comparison.df$compare.1vs0.5.total = ifelse(comparison.df$smooth.outcome.total.1v0.5 == comparison.df$truncation.outcome.total.1v0.5,
                                                 yes = TRUE, no = FALSE)

df.1v0.5.novel = comparison.df %>%
  dplyr::group_by(start.resistance.2, compare.1vs0.5.novel) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(perc = count/sum(count))

df.1v0.5.pyrethroid = comparison.df %>%
  dplyr::group_by(start.resistance.2, compare.1vs0.5.pyrethroid) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(perc = count/sum(count))

df.1v0.5.total = comparison.df %>%
  dplyr::group_by(start.resistance.2, compare.1vs0.5.total) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(perc = count/sum(count))

plot.1v0.5.novel = ggplot(df.1v0.5.novel, aes(x=compare.1vs0.5.novel,
                           y= perc,
                           fill = as.factor(start.resistance.2)))+
  geom_bar(position = "dodge", stat = "identity",
           colour = "black")+
  scale_fill_manual(values = c("#66c2a5",
                               "#fc8d62",
                               "#8da0cb",
                               "#e78ac3"))+
  xlab("Agreement between polysmooth and polytruncate")+
  ylab("Percentage")+
  ggtitle("Novel")+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_bw()+
  theme(legend.position = "none")

plot.1v0.5.pyrethroid = ggplot(df.1v0.5.pyrethroid, aes(x=compare.1vs0.5.pyrethroid,
                           y= perc,
                           fill = as.factor(start.resistance.2)))+
  geom_bar(position = "dodge", stat = "identity",
           colour = "black")+
  scale_fill_manual(values = c("#66c2a5",
                               "#fc8d62",
                               "#8da0cb",
                               "#e78ac3"))+
  xlab("Agreement between polysmooth and polytruncate")+
  ylab("Percentage")+
  ggtitle("Pyrethroid")+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_bw()+
  theme(legend.position = "none")

plot.1v0.5.total = ggplot(df.1v0.5.total, aes(x=compare.1vs0.5.total,
                          y= perc,
               fill = as.factor(start.resistance.2)))+
  geom_bar(position = "dodge", stat = "identity",
           colour = "black")+
  scale_fill_manual(values = c("#66c2a5",
    "#fc8d62",
    "#8da0cb",
    "#e78ac3"))+
  xlab("Agreement between polysmooth and polytruncate")+
  ylab("Percentage")+
  ggtitle("Total")+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_bw()+
  theme(legend.position = "none")

plot.1v0.5.novel + plot.1v0.5.pyrethroid + plot.1v0.5.total + plot_annotation(title = "Full Dose vs Half Dose Retains 50% Efficacy")

ggsave(
  filename = "chapter5_figureS2.1.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 600,
  height = 200,
  units = "px",
  dpi = 300)


#compare 1 vs 0.75
comparison.df$compare.1vs0.75.novel = ifelse(comparison.df$smooth.outcome.novel.1v0.75 == comparison.df$truncation.outcome.novel.1v0.75,
                                            yes = TRUE, no = FALSE)
comparison.df$compare.1vs0.75.pyrethroid = ifelse(comparison.df$smooth.outcome.pyrethroid.1v0.75 == comparison.df$truncation.outcome.pyrethroid.1v0.75,
                                                 yes = TRUE, no = FALSE)
comparison.df$compare.1vs0.75.total = ifelse(comparison.df$smooth.outcome.total.1v0.75 == comparison.df$truncation.outcome.total.1v0.75,
                                            yes = TRUE, no = FALSE)

df.1v0.75.novel = comparison.df %>%
  dplyr::group_by(start.resistance.2, compare.1vs0.75.novel) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(perc = count/sum(count))

df.1v0.75.pyrethroid = comparison.df %>%
  dplyr::group_by(start.resistance.2, compare.1vs0.75.pyrethroid) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(perc = count/sum(count))

df.1v0.75.total = comparison.df %>%
  dplyr::group_by(start.resistance.2, compare.1vs0.75.total) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(perc = count/sum(count))

plot.1v0.75.novel = ggplot(df.1v0.75.novel, aes(x=compare.1vs0.75.novel,
                                              y= perc,
                                              fill = as.factor(start.resistance.2)))+
  geom_bar(position = "dodge", stat = "identity",
           colour = "black")+
  scale_fill_manual(values = c("#66c2a5",
                               "#fc8d62",
                               "#8da0cb",
                               "#e78ac3"))+
  xlab("Agreement between polysmooth and polytruncate")+
  ylab("Percentage")+
  ggtitle("Novel")+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_bw()+
  theme(legend.position = "none")

plot.1v0.75.pyrethroid = ggplot(df.1v0.75.pyrethroid, aes(x=compare.1vs0.75.pyrethroid,
                                                        y= perc,
                                                        fill = as.factor(start.resistance.2)))+
  geom_bar(position = "dodge", stat = "identity",
           colour = "black")+
  scale_fill_manual(values = c("#66c2a5",
                               "#fc8d62",
                               "#8da0cb",
                               "#e78ac3"))+
  xlab("Agreement between polysmooth and polytruncate")+
  ylab("Percentage")+
  ggtitle("Pyrethroid")+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_bw()+
  theme(legend.position = "none")

plot.1v0.75.total = ggplot(df.1v0.75.total, aes(x=compare.1vs0.75.total,
                                              y= perc,
                                              fill = as.factor(start.resistance.2)))+
  geom_bar(position = "dodge", stat = "identity",
           colour = "black")+
  scale_fill_manual(values = c("#66c2a5",
                               "#fc8d62",
                               "#8da0cb",
                               "#e78ac3"))+
  xlab("Agreement between polysmooth and polytruncate")+
  ylab("Percentage")+
  ggtitle("Total")+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_bw()+
  theme(legend.position = "none")

plot.1v0.75.novel + plot.1v0.75.pyrethroid + plot.1v0.75.total + plot_annotation(title = "Full Dose vs Half Dose Retains 75% Efficacy")

ggsave(
  filename = "chapter5_figureS2.2.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 600,
  height = 200,
  units = "px",
  dpi = 300)

#compare 0.75 vs 0.5
comparison.df$compare.0.75vs0.5.novel = ifelse(comparison.df$smooth.outcome.novel.0.75v0.5 == comparison.df$truncation.outcome.novel.0.75v0.5,
                                             yes = TRUE, no = FALSE)
comparison.df$compare.0.75vs0.5.pyrethroid = ifelse(comparison.df$smooth.outcome.pyrethroid.0.75v0.5 == comparison.df$truncation.outcome.pyrethroid.0.75v0.5,
                                                  yes = TRUE, no = FALSE)
comparison.df$compare.0.75vs0.5.total = ifelse(comparison.df$smooth.outcome.total.0.75v0.5 == comparison.df$truncation.outcome.total.0.75v0.5,
                                             yes = TRUE, no = FALSE)

df.0.75v0.5.novel = comparison.df %>%
  dplyr::group_by(start.resistance.2, compare.0.75vs0.5.novel) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(perc = count/sum(count))

df.0.75v0.5.pyrethroid = comparison.df %>%
  dplyr::group_by(start.resistance.2, compare.0.75vs0.5.pyrethroid) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(perc = count/sum(count))

df.0.75v0.5.total = comparison.df %>%
  dplyr::group_by(start.resistance.2, compare.0.75vs0.5.total) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(perc = count/sum(count))

plot.0.75v0.5.novel = ggplot(df.0.75v0.5.novel, aes(x=compare.0.75vs0.5.novel,
                                                y= perc,
                                                fill = as.factor(start.resistance.2)))+
  geom_bar(position = "dodge", stat = "identity",
           colour = "black")+
  scale_fill_manual(values = c("#66c2a5",
                               "#fc8d62",
                               "#8da0cb",
                               "#e78ac3"))+
  xlab("Agreement between polysmooth and polytruncate")+
  ylab("Percentage")+
  ggtitle("Novel")+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_bw()+
  theme(legend.position = "none")

plot.0.75v0.5.pyrethroid = ggplot(df.0.75v0.5.pyrethroid, aes(x=compare.0.75vs0.5.pyrethroid,
                                                          y= perc,
                                                          fill = as.factor(start.resistance.2)))+
  geom_bar(position = "dodge", stat = "identity",
           colour = "black")+
  scale_fill_manual(values = c("#66c2a5",
                               "#fc8d62",
                               "#8da0cb",
                               "#e78ac3"))+
  xlab("Agreement between polysmooth and polytruncate")+
  ylab("Percentage")+
  ggtitle("Pyrethroid")+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_bw()+
  theme(legend.position = "none")

plot.0.75v0.5.total = ggplot(df.0.75v0.5.total, aes(x=compare.0.75vs0.5.total,
                                                y= perc,
                                                fill = as.factor(start.resistance.2)))+
  geom_bar(position = "dodge", stat = "identity",
           colour = "black")+
  scale_fill_manual(values = c("#66c2a5",
                               "#fc8d62",
                               "#8da0cb",
                               "#e78ac3"))+
  xlab("Agreement between polysmooth and polytruncate")+
  ylab("Percentage")+
  ggtitle("Total")+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent(c(0, 0.25, 0.5, 0.75, 1))) +
  theme_bw()+
  theme(legend.position = "none")

plot.0.75v0.5.novel + plot.0.75v0.5.pyrethroid + plot.0.75v0.5.total + plot_annotation(title = "Half Dose Retains 75% Efficacy vs Half Dose Retains 50% Efficacy")

ggsave(
  filename = "chapter5_figureS2.3.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 600,
  height = 200,
  units = "px",
  dpi = 300)

##Try and quantify if these "divergences" are meaningful:::
#1 vs 0.5


hist.1.v.0.5.total = ggplot(subset(comparison.df, compare.1vs0.5.total == FALSE), aes(x=abs(smooth.percent.diff.total.1v0.5 - truncation.percent.diff.total.1v0.5),
                          fill = compare.1vs0.5.total))+
  geom_vline(xintercept = 1, colour = "#c994c7", size =2)+
  geom_vline(xintercept = 5, colour = "#e7298a", size = 2)+
  geom_vline(xintercept = 10, colour = "#91003f", size = 2)+
  geom_histogram(binwidth = 1)+
  xlab("Magnitude of Difference")+
  ggtitle("Full Dose \n vs \n Half Dose Retains 50% Efficacy")+
  scale_fill_manual(values = c("red", "skyblue"))+
  facet_grid((initial.pyrethroid.bioassay*100)~.)+
  theme_bw()+
  theme(legend.position = "none")

hist.1.v.0.75.total = ggplot(subset(comparison.df, compare.1vs0.75.total == FALSE), aes(x=abs(smooth.percent.diff.total.1v0.75 - truncation.percent.diff.total.1v0.75),
                                               fill = compare.1vs0.75.total))+
  geom_vline(xintercept = 1, colour = "#c994c7", size =2)+
  geom_vline(xintercept = 5, colour = "#e7298a", size = 2)+
  geom_vline(xintercept = 10, colour = "#91003f", size = 2)+
  geom_histogram(binwidth = 1)+
  xlab("Magnitude of Difference")+
  ggtitle("Total")+
  scale_fill_manual(values = c("red", "skyblue"))+
  ggtitle("Full Dose \n vs \n Half Dose Retains 75% Efficacy")+
  facet_grid((initial.pyrethroid.bioassay*100)~.)+
  theme_bw()+
  theme(legend.position = "none")

hist.0.75.v.0.5.total = ggplot(subset(comparison.df, compare.0.75vs0.5.total == FALSE), aes(x=abs(smooth.percent.diff.total.0.75v0.5 - truncation.percent.diff.total.0.75v0.5),
                                                fill = compare.0.75vs0.5.total))+
  geom_vline(xintercept = 1, colour = "#c994c7", size =2)+
  geom_vline(xintercept = 5, colour = "#e7298a", size = 2)+
  geom_vline(xintercept = 10, colour = "#91003f", size = 2)+
  geom_histogram(binwidth = 1)+
  xlab("Magnitude of Difference")+
  ggtitle("Total")+
  scale_fill_manual(values = c("red", "skyblue"))+
  ggtitle(paste0("Half Dose Retains 75% Efficacy \n vs \n Half Dose Retains 50% Efficacy"))+
  facet_grid((initial.pyrethroid.bioassay*100)~.)+
  theme_bw()+
  theme(legend.position = "none")

hist.1.v.0.5.total + hist.1.v.0.75.total + hist.0.75.v.0.5.total
