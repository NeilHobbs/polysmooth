library(devtools)
load_all()
library(ggplot2)
library(dplyr)
library(patchwork)
library(data.table)
##Read in the datasets:::
set.1 = fread(".//mixtures.smooth.set.1.csv")
set.2 = fread(".//mixtures.smooth.set.2.csv")
set.3 = fread(".//mixtures.smooth.set.3.csv")
set.4 = fread(".//mixtures.smooth.set.4.csv")
set.5 = fread(".//mixtures.smooth.set.5.csv")
set.6 = fread(".//mixtures.smooth.set.6.csv")
set.7 = fread(".//mixtures.smooth.set.7.csv")
set.8 = fread(".//mixtures.smooth.set.8.csv")
set.9 = fread(".//mixtures.smooth.set.9.csv")
set.10 = fread(".//mixtures.smooth.set.10.csv")

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



set1.75 = fread("75_efficacy_mixtures.smooth.set.1.csv")
set2.75 = fread("75_efficacy_mixtures.smooth.set.2.csv")
set3.75 = fread("75_efficacy_mixtures.smooth.set.3.csv")
set4.75 = fread("75_efficacy_mixtures.smooth.set.4.csv")
set5.75 = fread("75_efficacy_mixtures.smooth.set.5.csv")
set6.75 = fread("75_efficacy_mixtures.smooth.set.6.csv")
set7.75 = fread("75_efficacy_mixtures.smooth.set.7.csv")
set8.75 = fread("75_efficacy_mixtures.smooth.set.8.csv")
set9.75 = fread("75_efficacy_mixtures.smooth.set.9.csv")
set10.75 = fread("75_efficacy_mixtures.smooth.set.10.csv")

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
                                                                                     yes = "HD_HD 50%",
                                                                                     no = "HD_HD 75%")))))






#put dosings into a more logical order for plotting etc
smooth.scaled.df$dosing.strategy = factor(smooth.scaled.df$dosing.strategy,
                                          levels = c("FD_FD",
                                                     "HD_HD 75%",
                                                     "HD_HD 50%",
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





smooth.scaled.df$model = "polysmooth"

#Next read in all the truncation simulaltions:
##Read in the datasets:::

set.1 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.1.csv")
set.2 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.2.csv")
set.3 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.3.csv")
set.4 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.4.csv")
set.5 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.5.csv")
set.6 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.6.csv")
set.7 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.7.csv")
set.8 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.8.csv")
set.9 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.9.csv")
set.10 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.set.10.csv")


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
truncation.scaled.df = subset(truncation.scaled.df, threshold.gens == threshold.gens.1)


set1.75 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.1.csv")
set2.75 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.2.csv")
set3.75 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.3.csv")
set4.75 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.4.csv")
set5.75 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.5.csv")
set6.75 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.6.csv")
set7.75 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.7.csv")
set8.75 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.8.csv")
set9.75 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.9.csv")
set10.75 = fread("~/LSTM_IR_Modelling/polytruncate/mixtures.truncation.75efficacy.set.10.csv")

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


truncation.df.1 = subset(truncation.scaled.df, dose.1 == 1 &
                           dose.2 == 1)


set.75s$novel.solo = truncation.df.1$novel.solo
set.75s$pyrethroid.solo = truncation.df.1$pyrethroid.solo

#add solo columns

#make one dataset
truncation.scaled.df = dplyr::bind_rows(truncation.scaled.df,
                                        set.75s)

rm(set.75s, truncation.df, truncation.df.1)


#Raw change between the mixture and the solo.
truncation.scaled.df$change.novel.peak = truncation.scaled.df$novel.solo - truncation.scaled.df$mixture.novel
truncation.scaled.df$change.pyrethroid.peak = truncation.scaled.df$pyrethroid.solo - truncation.scaled.df$mixture.pyrethroid


#Convert PRS scores to Bioassay Survival
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



#Changes in the bioassay survival mixture vs solo
truncation.scaled.df$change.novel.peak.bioassay = truncation.scaled.df$novel.solo.peak.bioassay - truncation.scaled.df$novel.mix.peak.bioassay
truncation.scaled.df$change.pyrethroid.peak.bioassay = truncation.scaled.df$pyrethroid.solo.peak.bioassay - truncation.scaled.df$pyrethroid.mix.peak.bioassay


#Categorise the dosing strategies;  Novel_Pyrethroid
#FD --> Full Dose
# HD --> Half dose (retains 50% efficacy of full dose)
truncation.scaled.df$dosing.strategy = factor(ifelse(truncation.scaled.df$dose.1 == 1 &
                                                       truncation.scaled.df$dose.2 == 1,
                                                     yes = "FD_FD",
                                                     no = ifelse(truncation.scaled.df$dose.1 == 1 &
                                                                   truncation.scaled.df$dose.2 == 0.5,
                                                                 yes = "FD_HD",
                                                                 no = ifelse(truncation.scaled.df$dose.1 == 0.5 &
                                                                               truncation.scaled.df$dose.2 == 1,
                                                                             yes = "HD_FD",
                                                                             no = ifelse(truncation.scaled.df$dose.1 == 0.5 &
                                                                                           truncation.scaled.df$dose.2 == 0.5,
                                                                                         yes = "HD_HD 50%",
                                                                                         no = "HD_HD 75%")))))






#put dosings into a more logical order for plotting etc
truncation.scaled.df$dosing.strategy = factor(truncation.scaled.df$dosing.strategy,
                                              levels = c("FD_FD",
                                                         "HD_HD 75%",
                                                         "HD_HD 50%",
                                                         "FD_HD",
                                                         "HD_FD"))

table(truncation.scaled.df$dosing.strategy)


#Turn decay rates into categories for easier interpretation
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

truncation.scaled.df$model = "polytruncate"


##Join polysmooth and polytruncate simulations

smooth.truncation.df = rbind(smooth.scaled.df, truncation.scaled.df)
rm(smooth.scaled.df, truncation.scaled.df)

# smooth.truncation.df = subset(smooth.truncation.df, dosing.strategy %in% c("FD_FD", "HD_HD 75%", "HD_HD 50%"))
#####
smooth.truncation.df$change.novel.peak = smooth.truncation.df$novel.solo - smooth.truncation.df$mixture.novel
smooth.truncation.df$change.pyrethroid.peak = smooth.truncation.df$pyrethroid.solo - smooth.truncation.df$mixture.pyrethroid

##Convert PRS to Bioassays
smooth.truncation.df$start.bioassay.pyrethroid = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                           trait.mean = smooth.truncation.df$start.resistance.2,
                                                                                           half.population.bioassay.survival.resistance = 900,
                                                                                           michaelis.menten.slope = 1)*100

smooth.truncation.df$novel.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                          trait.mean = smooth.truncation.df$novel.solo,
                                                                                          half.population.bioassay.survival.resistance = 900,
                                                                                          michaelis.menten.slope = 1)


smooth.truncation.df$pyrethroid.solo.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                               trait.mean = smooth.truncation.df$pyrethroid.solo,
                                                                                               half.population.bioassay.survival.resistance = 900,
                                                                                               michaelis.menten.slope = 1)



smooth.truncation.df$novel.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                         trait.mean = smooth.truncation.df$mixture.novel,
                                                                                         half.population.bioassay.survival.resistance = 900,
                                                                                         michaelis.menten.slope = 1)



smooth.truncation.df$pyrethroid.mix.peak.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                              trait.mean = smooth.truncation.df$mixture.pyrethroid,
                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                              michaelis.menten.slope = 1)



#Calculate Differences between solo and mixture deployments
smooth.truncation.df$change.novel.peak.bioassay = smooth.truncation.df$novel.solo.peak.bioassay - smooth.truncation.df$novel.mix.peak.bioassay
smooth.truncation.df$change.pyrethroid.peak.bioassay = smooth.truncation.df$pyrethroid.solo.peak.bioassay - smooth.truncation.df$pyrethroid.mix.peak.bioassay

#calculate changes in rates of evolution:::
smooth.truncation.df$novel.solo.rate = smooth.truncation.df$novel.solo.peak.bioassay/200
smooth.truncation.df$pyrethroid.solo.rate = (smooth.truncation.df$pyrethroid.solo.peak.bioassay - (smooth.truncation.df$start.bioassay.pyrethroid/100))/200
smooth.truncation.df$novel.mix.rate = smooth.truncation.df$novel.mix.peak.bioassay/200
smooth.truncation.df$pyrethroid.mix.rate = (smooth.truncation.df$pyrethroid.mix.peak.bioassay - (smooth.truncation.df$start.bioassay.pyrethroid/100))/200

smooth.truncation.df$rate.change.novel.percent = ((smooth.truncation.df$novel.mix.rate - smooth.truncation.df$novel.solo.rate)/smooth.truncation.df$novel.solo.rate) * 100
smooth.truncation.df$rate.change.pyrethroid.percent = ((smooth.truncation.df$pyrethroid.mix.rate - smooth.truncation.df$pyrethroid.solo.rate)/smooth.truncation.df$pyrethroid.solo.rate) * 100

smooth.truncation.df$novel.rate.change = ifelse(smooth.truncation.df$rate.change.novel.percent > 0,
                                            yes = "faster",
                                            no = ifelse(smooth.truncation.df$rate.change.novel.percent < 0,
                                                        yes = "slower",
                                                        no = "no change"))

smooth.truncation.df$pyrethroid.rate.change = ifelse(smooth.truncation.df$rate.change.pyrethroid.percent > 0,
                                                 yes = "faster",
                                                 no = ifelse(smooth.truncation.df$rate.change.pyrethroid.percent < 0,
                                                             yes = "slower",
                                                             no = "no change"))


smooth.truncation.df$diff.end.bioassay.pyr.mix = (smooth.truncation.df$pyrethroid.mix.peak.bioassay*100) - smooth.truncation.df$start.bioassay.pyrethroid

smooth.truncation.df.1 = subset(smooth.truncation.df, dose.1 == 1 &
                       dose.2 == 1)

smooth.truncation.df.0.75 = subset(smooth.truncation.df, dose.1 == 0.75 &
                          dose.2 == 0.75)

smooth.truncation.df.0.5 = subset(smooth.truncation.df, dose.1 == 0.5 &
                         dose.2 == 0.5)



smooth.truncation.df.1$novel_75_vs_100 = ((smooth.truncation.df.0.75$novel.mix.peak.bioassay - smooth.truncation.df.1$novel.mix.peak.bioassay) / smooth.truncation.df.1$novel.mix.peak.bioassay)*100
smooth.truncation.df.1$novel_50_vs_100 = ((smooth.truncation.df.0.5$novel.mix.peak.bioassay - smooth.truncation.df.1$novel.mix.peak.bioassay) / smooth.truncation.df.1$novel.mix.peak.bioassay)*100
smooth.truncation.df.1$novel_50_vs_75 = ((smooth.truncation.df.0.5$novel.mix.peak.bioassay - smooth.truncation.df.0.75$novel.mix.peak.bioassay) / smooth.truncation.df.0.75$novel.mix.peak.bioassay)*100

smooth.truncation.df.1$novel_75_vs_100_outcome = ifelse(smooth.truncation.df.1$novel_75_vs_100 > 0,
                                             yes = "75% Win",
                                             no = "100% Win")

smooth.truncation.df.1$novel_50_vs_100_outcome = ifelse(smooth.truncation.df.1$novel_50_vs_100 > 0,
                                             yes = "50% Win",
                                             no = "100% Win")


smooth.truncation.df.1$novel_50_vs_75_outcome = ifelse(smooth.truncation.df.1$novel_50_vs_75 > 0,
                                            yes = "50% Win",
                                            no = "75% Win")




smooth.truncation.df.1$pyr_75_vs_100 = ((smooth.truncation.df.0.75$diff.end.bioassay.pyr.mix - smooth.truncation.df.1$diff.end.bioassay.pyr.mix) / smooth.truncation.df.1$diff.end.bioassay.pyr.mix)*100
smooth.truncation.df.1$pyr_50_vs_100 = ((smooth.truncation.df.0.5$diff.end.bioassay.pyr.mix - smooth.truncation.df.1$diff.end.bioassay.pyr.mix) / smooth.truncation.df.1$diff.end.bioassay.pyr.mix)*100
smooth.truncation.df.1$pyr_50_vs_75 = ((smooth.truncation.df.0.5$diff.end.bioassay.pyr.mix - smooth.truncation.df.0.75$diff.end.bioassay.pyr.mix) / smooth.truncation.df.0.75$diff.end.bioassay.pyr.mix)*100

smooth.truncation.df.1$pyr_75_vs_100_outcome = ifelse(smooth.truncation.df.1$pyr_75_vs_100 > 0,
                                           yes = "75% Win",
                                           no = "100% Win")

smooth.truncation.df.1$pyr_50_vs_100_outcome = ifelse(smooth.truncation.df.1$pyr_50_vs_100 > 0,
                                           yes = "50% Win",
                                           no = "100% Win")


smooth.truncation.df.1$pyr_50_vs_75_outcome = ifelse(smooth.truncation.df.1$pyr_50_vs_75 > 0,
                                          yes = "50% Win",
                                          no = "75% Win")



smooth.truncation.df.1$total_75_vs_100 = (((smooth.truncation.df.0.75$diff.end.bioassay.pyr.mix + smooth.truncation.df.0.75$novel.mix.peak.bioassay)- (smooth.truncation.df.1$novel.mix.peak.bioassay +smooth.truncation.df.1$diff.end.bioassay.pyr.mix)) / (smooth.truncation.df.1$novel.mix.peak.bioassay +smooth.truncation.df.1$diff.end.bioassay.pyr.mix))*100
smooth.truncation.df.1$total_50_vs_100 = (((smooth.truncation.df.0.5$diff.end.bioassay.pyr.mix + smooth.truncation.df.0.5$novel.mix.peak.bioassay)- (smooth.truncation.df.1$novel.mix.peak.bioassay +smooth.truncation.df.1$diff.end.bioassay.pyr.mix)) / (smooth.truncation.df.1$novel.mix.peak.bioassay +smooth.truncation.df.1$diff.end.bioassay.pyr.mix))*100
smooth.truncation.df.1$total_50_vs_75 = (((smooth.truncation.df.0.5$diff.end.bioassay.pyr.mix + smooth.truncation.df.0.5$novel.mix.peak.bioassay)- (smooth.truncation.df.0.75$novel.mix.peak.bioassay +smooth.truncation.df.0.75$diff.end.bioassay.pyr.mix)) / (smooth.truncation.df.0.75$novel.mix.peak.bioassay +smooth.truncation.df.0.75$diff.end.bioassay.pyr.mix))*100


smooth.truncation.df.1$total_75_vs_100_outcome = ifelse(smooth.truncation.df.1$total_75_vs_100 > 0,
                                             yes = "75% Win",
                                             no = "100% Win")

smooth.truncation.df.1$total_50_vs_100_outcome = ifelse(smooth.truncation.df.1$total_50_vs_100 > 0,
                                             yes = "50% Win",
                                             no = "100% Win")


smooth.truncation.df.1$total_50_vs_75_outcome = ifelse(smooth.truncation.df.1$total_50_vs_75 > 0,
                                            yes = "50% Win",
                                            no = "75% Win")

##Importance of Dosing
novel.median.value = smooth.truncation.df %>%
  dplyr::group_by(dosing.strategy) %>%
  dplyr::summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.truncation.df %>%
  dplyr::group_by(dosing.strategy) %>%
  dplyr::summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = smooth.truncation.df %>%
  dplyr::group_by(dosing.strategy) %>%
  dplyr::summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.truncation.df %>%
  dplyr::group_by(dosing.strategy) %>%
  dplyr::summarize(mean=mean(rate.change.pyrethroid.percent))


dosing.plot.novel = ggplot(smooth.truncation.df,
                           aes(x=rate.change.novel.percent,
                               fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "skyblue", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value,
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Dosing Strategy",
                                         breaks = NULL,
                                         labels = NULL))+
  facet_grid(dosing.strategy~.)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("Novel Insecticide in Mixture\nvs\nNovel Insecticide Monotherapy"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

dosing.plot.pyr = ggplot(smooth.truncation.df,
                         aes(x=rate.change.pyrethroid.percent,
                             fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "skyblue"))+
  facet_grid(dosing.strategy~.)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Dosing Strategy",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_vline(data= pyrethroid.median.value,
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("Pyrethroid Insecticide in Mixture\nvs\nPyrethroid Insecticide Monotherapy"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))


dosing.plot.novel + dosing.plot.pyr

rm(dosing.plot.novel, dosing.plot.pyr)



#Importance of Resistance
novel.median.value = smooth.truncation.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.truncation.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = smooth.truncation.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.truncation.df %>%
  group_by(start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))

resistance.plot.novel = ggplot(smooth.truncation.df, aes(x=rate.change.novel.percent,
                                                     fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "skyblue", "grey"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("Novel Insecticide in Mixture\nvs\nNovel Insecticide Monotherapy"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

resistance.plot.pyr = ggplot(smooth.truncation.df, aes(x=rate.change.pyrethroid.percent,
                                                   fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("grey", "skyblue", "red"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("Pyrethroid Insecticide in Mixture\nvs\nPyrethroid Insecticide Monotherapy"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

resistance.plot.novel + resistance.plot.pyr
rm(resistance.plot.novel, resistance.plot.pyr)

#Importance of Decay
novel.median.value = smooth.truncation.df %>%
  group_by(decay.rate) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.truncation.df %>%
  group_by(decay.rate) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = smooth.truncation.df %>%
  group_by(decay.rate) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.truncation.df %>%
  group_by(decay.rate) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))

decay.plot.novel = ggplot(smooth.truncation.df, aes(x=rate.change.novel.percent,
                                                fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "skyblue", "grey"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "The novel insecticide decays ____ than the pyrethroid insecticide",
                                         breaks = NULL,
                                         labels = NULL))+
  facet_grid(decay.rate ~ .)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("Novel Insecticide in Mixture\nvs\nNovel Insecticide Monotherapy"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

decay.plot.pyr = ggplot(smooth.truncation.df, aes(x=rate.change.pyrethroid.percent,
                                              fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("grey", "skyblue", "red"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.3,
             size = 2)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "The novel insecticide decays ____ than the pyrethroid insecticide",
                                         breaks = NULL,
                                         labels = NULL))+
  facet_grid(decay.rate ~ .)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("Pyrethroid Insecticide in Mixture\nvs\nPyrethroid Insecticide Monotherapy"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))


decay.plot.novel + decay.plot.pyr
rm(decay.plot.novel, decay.plot.pyr)



#Of course there will be interactions between all these aspects.
#Dosing and Resistance

novel.median.value = smooth.truncation.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.truncation.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.novel.percent))


pyrethroid.median.value = smooth.truncation.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.truncation.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid) %>%
  summarize(mean=mean(rate.change.pyrethroid.percent))

resistance.dose.plot.novel = ggplot(smooth.truncation.df, aes(x=rate.change.novel.percent,
                                                          fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "skyblue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.5,
             size = 1)+
  facet_grid(start.bioassay.pyrethroid ~ dosing.strategy)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = paste("Initial Pyrethroid Bioassay Survival (%)"),
                                         breaks = NULL,
                                         labels = NULL),
                     expand= c(0, 0))+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Dosing of the Mixture Formulation",
                                         breaks = NULL,
                                         labels = NULL),
                     expand = c(0,0))+
  xlab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("Novel Insecticide in Mixture\nvs\nNovel Insecticide Monotherapy"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x.bottom = element_text(size = 12),
        axis.title.x.top = element_text(size = 12),
        axis.title.y.right = element_text(size = 12),
        axis.title.y.left = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45,
                                   colour = "black"),
        axis.text.y = element_text(colour = "black",
                                   size = 6),
        strip.background =element_rect(fill="#dadaeb"),
        title = element_text(size = 12))



resistance.dose.plot.pr = ggplot(smooth.truncation.df, aes(x=rate.change.pyrethroid.percent,
                                                       fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("skyblue", "red"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median),
             colour = "orange",
             alpha = 0.5,
             size = 1)+
  facet_grid(start.bioassay.pyrethroid ~ dosing.strategy)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = paste("Initial Pyrethroid Bioassay Survival (%)"),
                                         breaks = NULL,
                                         labels = NULL),
                     expand= c(0, 0))+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Dosing of the Mixture Formulation",
                                         breaks = NULL,
                                         labels = NULL),
                     expand = c(0,0))+
  xlab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x.bottom = element_text(size = 12),
        axis.title.x.top = element_text(size = 12),
        axis.title.y.right = element_text(size = 12),
        axis.title.y.left = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45,
                                   colour = "black"),
        axis.text.y = element_text(colour = "black",
                                   size = 6),
        strip.background =element_rect(fill="#dadaeb"),
        title = element_text(size = 12))


resistance.dose.plot.novel + resistance.dose.plot.pr
rm(resistance.dose.plot.novel, resistance.dose.plot.pr)

##Trying to put "everything" together...

novel.median.value = smooth.truncation.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid, decay.rate) %>%
  summarize(median=median(rate.change.novel.percent))

pyrethroid.median.value = smooth.truncation.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid, decay.rate) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

resistance.dose.decay.plot.novel = ggplot(smooth.truncation.df, aes(x=rate.change.novel.percent,
                                                                fill = dosing.strategy))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= novel.median.value, aes(xintercept = median,
                                           colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dashed")+
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
  xlab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("Novel Insecticide in Mixture\nvs\nNovel Insecticide Monotherapy"))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))


resistance.dose.decay.plot.pyrethroid = ggplot(smooth.truncation.df, aes(x=rate.change.pyrethroid.percent,
                                                                     fill = dosing.strategy))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= pyrethroid.median.value, aes(xintercept = median,
                                                colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dashed")+
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
  xlab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("Pyrethroid Insecticide in Mixture\nvs\nPyrethroid Insecticide Monotherapy"))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))


resistance.dose.decay.plot.novel + resistance.dose.decay.plot.pyrethroid

ggsave(
  filename = "chapter5_figure5.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 1200,
  height = 800,
  units = "px",
  dpi = 300)




rm(resistance.dose.decay.plot.novel, resistance.dose.decay.plot.pyrethroid)

###Fit and Plot Generalised Additive Models:
# 1.Heritability
# 2.Female Exposure
# 3. Total Male Exposure
# 4. Coverage
# 5. Dispersal

#Heritability
heritability.novel.gam = ggplot(smooth.truncation.df, aes(x=heritability,
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
  ggtitle("Mixture vs Novel Insecticide Monotherapy")+
  theme_bw()+
  theme(legend.position = "bottom")


heritability.pyrethroid.gam = ggplot(smooth.truncation.df, aes(x=heritability,
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
  ggtitle("Mixture vs Pyrethroid Insecticide Monotherapy")+
  theme_bw()+
  theme(legend.position = "bottom")



heritability.novel.gam + heritability.pyrethroid.gam
rm(heritability.novel.gam, heritability.pyrethroid.gam)

female.exposure.novel.gam = ggplot(smooth.truncation.df, aes(x=female.exposure,
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
  facet_grid(.~start.bioassay.pyrethroid)+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  xlab("Female Insecticide Exposure")+
  ggtitle(paste0("Novel Insecticide in Mixture\nvs\nNovel Insecticide Monotherapy"))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 15))

female.exposure.pyrethroid.gam = ggplot(smooth.truncation.df, aes(x=female.exposure,
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
  facet_grid(.~start.bioassay.pyrethroid)+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  xlab("Female Insecticide Exposure")+
  ggtitle(paste0("Pyrethroid Insecticide in Mixture\nvs\nPyrethroid Insecticide Monotherapy"))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 15))



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




#Colour Scheme:
#   "#e41a1c", #red = FD_FD
#   "#ff7f00",#orange = HD_HD 75%
#   "#984ea3", #purple = HD_HD 50%


novel_75_vs_100_plot = ggplot(smooth.truncation.df.1, aes(x=novel_75_vs_100,
                                               fill = novel_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Novel Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

novel_50_vs_100_plot = ggplot(smooth.truncation.df.1, aes(x=novel_50_vs_100,
                                               fill = novel_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Novel Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

novel_50_vs_75_plot= ggplot(smooth.truncation.df.1, aes(x=novel_50_vs_75,
                                             fill = novel_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Novel Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

pyr_75_vs_100_plot = ggplot(smooth.truncation.df.1, aes(x=pyr_75_vs_100,
                                             fill = pyr_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Pyrethroid Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        title = element_text(size = 18),
        strip.text = element_text(size = 16))

pyr_50_vs_100_plot = ggplot(smooth.truncation.df.1, aes(x=pyr_50_vs_100,
                                             fill = pyr_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Pyrethroid Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  ylab("Frequency")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        title = element_text(size = 18),
        strip.text = element_text(size = 16))

pyr_50_vs_75_plot= ggplot(smooth.truncation.df.1, aes(x=pyr_50_vs_75,
                                           fill = pyr_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Pyrethroid Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        title = element_text(size = 18),
        strip.text = element_text(size = 16))

p.1 = novel_75_vs_100_plot + pyr_75_vs_100_plot +
  plot_annotation(title = "Comparing FD_FD vs HD_HD retains 75%",
                  theme = theme(plot.title = element_text(size = 22)))

p.2 = novel_50_vs_100_plot + pyr_50_vs_100_plot +
  plot_annotation(title = "Comparing FD_FD vs HD_HD retains 50%",
                  theme = theme(plot.title = element_text(size = 22)))

p.3 = novel_50_vs_75_plot + pyr_50_vs_75_plot +
  plot_annotation(title = "Comparing HD_HD retains 75% vs HD_HD retains 50%",
                  theme = theme(plot.title = element_text(size = 22)))

p.1/p.2/p.3

ggsave(
  filename = "chapter5_figure6.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 800,
  height = 1200,
  units = "px",
  dpi = 300)



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






########
#Comparing Polytruncate and Polysmooth


##Importance of Dosing
novel.median.value = smooth.truncation.df %>%
  dplyr::group_by(dosing.strategy, model) %>%
  dplyr::summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.truncation.df %>%
  dplyr::group_by(dosing.strategy, model) %>%
  dplyr::summarize(mean=mean(rate.change.novel.percent))

pyrethroid.median.value = smooth.truncation.df %>%
  dplyr::group_by(dosing.strategy, model) %>%
  dplyr::summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.truncation.df %>%
  dplyr::group_by(dosing.strategy, model) %>%
  dplyr::summarize(mean=mean(rate.change.pyrethroid.percent))


dosing.plot.novel.smooth = ggplot(subset(smooth.truncation.df, model == "polysmooth"),
                                  aes(x=rate.change.novel.percent,
                                      fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= subset(novel.median.value, model == "polysmooth"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Dosing Strategy",
                                         breaks = NULL,
                                         labels = NULL))+
  facet_grid(dosing.strategy~ .)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

dosing.plot.pyr.smooth = ggplot(subset(smooth.truncation.df, model == "polysmooth"),
                                aes(x=rate.change.pyrethroid.percent,
                                    fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("grey", "red"))+
  facet_grid(dosing.strategy~.)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Dosing Strategy",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_vline(data= subset(pyrethroid.median.value, model == "polysmooth"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

dosing.plot.novel.truncate = ggplot(subset(smooth.truncation.df, model == "polytruncate"),
                                    aes(x=rate.change.novel.percent,
                                        fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= subset(novel.median.value, model == "polytruncate"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Dosing Strategy",
                                         breaks = NULL,
                                         labels = NULL))+
  facet_grid(dosing.strategy~ .)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polytruncate: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

dosing.plot.pyr.truncate = ggplot(subset(smooth.truncation.df, model == "polytruncate"),
                                  aes(x=rate.change.pyrethroid.percent,
                                      fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "grey"))+
  facet_grid(dosing.strategy~.)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Dosing Strategy",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_vline(data= subset(pyrethroid.median.value, model == "polytruncate"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polytruncate: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

the.layout = "
AB
CD"


dosing.plot.novel.truncate+
  dosing.plot.pyr.truncate+
  dosing.plot.novel.smooth +
  dosing.plot.pyr.smooth +
  plot_layout(design = the.layout)


ggsave(
  filename = "chapter5_figureS1.1.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 800,
  height = 800,
  units = "px",
  dpi = 300)


##Importance of Start Resistance
novel.median.value = smooth.truncation.df %>%
  dplyr::group_by(start.bioassay.pyrethroid, model) %>%
  dplyr::summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.truncation.df %>%
  dplyr::group_by(start.bioassay.pyrethroid, model) %>%
  dplyr::summarize(mean=mean(rate.change.novel.percent))

pyrethroid.median.value = smooth.truncation.df %>%
  dplyr::group_by(start.bioassay.pyrethroid, model) %>%
  dplyr::summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.truncation.df %>%
  dplyr::group_by(start.bioassay.pyrethroid, model) %>%
  dplyr::summarize(mean=mean(rate.change.pyrethroid.percent))


dosing.plot.novel.smooth = ggplot(subset(smooth.truncation.df, model == "polysmooth"),
                                  aes(x=rate.change.novel.percent,
                                      fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= subset(novel.median.value, model == "polysmooth"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  facet_grid(start.bioassay.pyrethroid~ .)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

dosing.plot.pyr.smooth = ggplot(subset(smooth.truncation.df, model == "polysmooth"),
                                aes(x=rate.change.pyrethroid.percent,
                                    fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("grey", "red"))+
  facet_grid(start.bioassay.pyrethroid~.)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_vline(data= subset(pyrethroid.median.value, model == "polysmooth"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

dosing.plot.novel.truncate = ggplot(subset(smooth.truncation.df, model == "polytruncate"),
                                    aes(x=rate.change.novel.percent,
                                        fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= subset(novel.median.value, model == "polytruncate"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  facet_grid(start.bioassay.pyrethroid~ .)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polytruncate: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

dosing.plot.pyr.truncate = ggplot(subset(smooth.truncation.df, model == "polytruncate"),
                                  aes(x=rate.change.pyrethroid.percent,
                                      fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "grey"))+
  facet_grid(start.bioassay.pyrethroid~.)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_vline(data= subset(pyrethroid.median.value, model == "polytruncate"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polytruncate: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

the.layout = "
AB
CD"


dosing.plot.novel.truncate+
  dosing.plot.pyr.truncate+
  dosing.plot.novel.smooth +
  dosing.plot.pyr.smooth +
  plot_layout(design = the.layout)


ggsave(
  filename = "chapter5_figureS1.2.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 800,
  height = 800,
  units = "px",
  dpi = 300)




##Importance of Start Resistance
novel.median.value = smooth.truncation.df %>%
  dplyr::group_by(decay.rate, model) %>%
  dplyr::summarize(median=median(rate.change.novel.percent))

novel.mean.value = smooth.truncation.df %>%
  dplyr::group_by(decay.rate, model) %>%
  dplyr::summarize(mean=mean(rate.change.novel.percent))

pyrethroid.median.value = smooth.truncation.df %>%
  dplyr::group_by(decay.rate, model) %>%
  dplyr::summarize(median=median(rate.change.pyrethroid.percent))

pyrethroid.mean.value = smooth.truncation.df %>%
  dplyr::group_by(decay.rate, model) %>%
  dplyr::summarize(mean=mean(rate.change.pyrethroid.percent))


dosing.plot.novel.smooth = ggplot(subset(smooth.truncation.df, model == "polysmooth"),
                                  aes(x=rate.change.novel.percent,
                                      fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= subset(novel.median.value, model == "polysmooth"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "The novel insecticide decays ____ than the pyrethroid insecticide",
                                         breaks = NULL,
                                         labels = NULL))+
  facet_grid(decay.rate~ .)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polysmooth: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

dosing.plot.pyr.smooth = ggplot(subset(smooth.truncation.df, model == "polysmooth"),
                                aes(x=rate.change.pyrethroid.percent,
                                    fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("grey", "red"))+
  facet_grid(decay.rate~.)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "The novel insecticide decays ____ than the pyrethroid insecticide",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_vline(data= subset(pyrethroid.median.value, model == "polysmooth"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polysmooth: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

dosing.plot.novel.truncate = ggplot(subset(smooth.truncation.df, model == "polytruncate"),
                                    aes(x=rate.change.novel.percent,
                                        fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "grey", "blue"))+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= subset(novel.median.value, model == "polytruncate"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "The novel insecticide decays ____ than the pyrethroid insecticide",
                                         breaks = NULL,
                                         labels = NULL))+
  facet_grid(decay.rate~ .)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polytruncate: Novel Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

dosing.plot.pyr.truncate = ggplot(subset(smooth.truncation.df, model == "polytruncate"),
                                  aes(x=rate.change.pyrethroid.percent,
                                      fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("red", "grey"))+
  facet_grid(decay.rate~.)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "The novel insecticide decays ____ than the pyrethroid insecticide",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_vline(data= subset(pyrethroid.median.value, model == "polytruncate"),
             aes(xintercept = median),
             colour = "orange",
             size = 2,
             alpha = 0.3)+
  xlab(paste0("Change in the Rate of Evolution (%)\n When in Mixture Compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle("Polytruncate: Pyrethroid Insecticide")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

the.layout = "
AB
CD"


dosing.plot.novel.truncate+
  dosing.plot.pyr.truncate+
  dosing.plot.novel.smooth +
  dosing.plot.pyr.smooth +
  plot_layout(design = the.layout)


ggsave(
  filename = "chapter5_figureS1.3.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 800,
  height = 800,
  units = "px",
  dpi = 300)


novel.median.value = smooth.truncation.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid, decay.rate, model) %>%
  summarize(median=median(rate.change.novel.percent))

pyrethroid.median.value = smooth.truncation.df %>%
  group_by(dosing.strategy, start.bioassay.pyrethroid, decay.rate, model) %>%
  summarize(median=median(rate.change.pyrethroid.percent))

resistance.dose.decay.plot.novel.smooth = ggplot(subset(smooth.truncation.df, model == "polysmooth"), aes(x=rate.change.novel.percent,
                                                                                                          fill = dosing.strategy))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= subset(novel.median.value, model == "polysmooth"), aes(xintercept = median,
                                                                          colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dashed")+
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
  xlab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("polysmooth: Novel Insecticide"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))


resistance.dose.decay.plot.pyrethroid.smooth  = ggplot(subset(smooth.truncation.df, model == "polysmooth"), aes(x=rate.change.pyrethroid.percent,
                                                                                                                fill = dosing.strategy))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= subset(pyrethroid.median.value, model == "polysmooth"), aes(xintercept = median,
                                                                               colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dashed")+
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
  xlab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("polysmooth: Pyrethroid"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))

resistance.dose.decay.plot.novel.truncate = ggplot(subset(smooth.truncation.df, model == "polysmooth"), aes(x=rate.change.novel.percent,
                                                                                                            fill = dosing.strategy))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= subset(novel.median.value, model == "polytruncate"), aes(xintercept = median,
                                                                            colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dashed")+
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
  xlab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("polytruncate: Novel Insecticide"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))


resistance.dose.decay.plot.pyrethroid.truncate  = ggplot(subset(smooth.truncation.df, model == "polytruncate"), aes(x=rate.change.pyrethroid.percent,
                                                                                                                    fill = dosing.strategy))+
  geom_histogram(binwidth = 1)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  geom_vline(data= subset(pyrethroid.median.value, model == "polytruncate"), aes(xintercept = median,
                                                                                 colour = dosing.strategy),
             alpha = 1,
             size = 1,
             linetype = "dashed")+
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
  xlab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  ylab("Frequency")+
  ggtitle(paste0("polytruncate: Pyrethroid"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(size = 12))


the.layout = "
AB
CD
"


resistance.dose.decay.plot.novel.truncate +
  resistance.dose.decay.plot.pyrethroid.truncate+
  resistance.dose.decay.plot.novel.smooth+
  resistance.dose.decay.plot.pyrethroid.smooth +
  plot_layout(design = the.layout)


ggsave(
  filename = "chapter5_figureS1.4.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 800,
  height = 800,
  units = "px",
  dpi = 300)


female.exposure.novel.gam.truncate = ggplot(subset(smooth.truncation.df, model == "polytruncate"), aes(x=female.exposure,
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
  facet_grid(.~start.bioassay.pyrethroid)+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  xlab("Female Insecticide Exposure")+
  ggtitle(paste0("Polytruncate: Novel Insecticide"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12))

female.exposure.pyrethroid.gam.truncate= ggplot(subset(smooth.truncation.df, model == "polytruncate"), aes(x=female.exposure,
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
  facet_grid(.~start.bioassay.pyrethroid)+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  xlab("Female Insecticide Exposure")+
  ggtitle(paste0("Polytruncate: Pyrethroid"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12))


female.exposure.novel.gam.smooth = ggplot(subset(smooth.truncation.df, model == "polysmooth"), aes(x=female.exposure,
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
  facet_grid(.~start.bioassay.pyrethroid)+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  xlab("Female Insecticide Exposure")+
  ggtitle(paste0("Polysmooth: Novel Insecticide"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12))

female.exposure.pyrethroid.gam.smooth = ggplot(subset(smooth.truncation.df, model == "polysmooth"), aes(x=female.exposure,
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
  facet_grid(.~start.bioassay.pyrethroid)+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab(paste0("Change in the Rate of Evolution (%)\n when in Mixture compared to Monotherapy Deployment"))+
  xlab("Female Insecticide Exposure")+
  ggtitle(paste0("Polysmooth: Pyrethroid"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12))

the.layout = "
AB
CD
"

female.exposure.novel.gam.truncate +
  female.exposure.pyrethroid.gam.truncate+
  female.exposure.novel.gam.smooth +
  female.exposure.pyrethroid.gam.smooth +
  plot_layout(design = the.layout)

ggsave(
  filename = "chapter5_figureS1.5.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 800,
  height = 800,
  units = "px",
  dpi = 300)



novel_75_vs_100_plot = ggplot(subset(smooth.truncation.df.1, model == "polytruncate"), aes(x=novel_75_vs_100,
                                                                                           fill = novel_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Novel Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

novel_50_vs_100_plot = ggplot(subset(smooth.truncation.df.1, model == "polytruncate"), aes(x=novel_50_vs_100,
                                                                                           fill = novel_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Novel Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

novel_50_vs_75_plot= ggplot(subset(smooth.truncation.df.1, model == "polytruncate"), aes(x=novel_50_vs_75,
                                                                                         fill = novel_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Novel Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

pyr_75_vs_100_plot = ggplot(subset(smooth.truncation.df.1, model == "polytruncate"), aes(x=pyr_75_vs_100,
                                                                                         fill = pyr_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Pyrethroid Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

pyr_50_vs_100_plot = ggplot(subset(smooth.truncation.df.1, model == "polytruncate"), aes(x=pyr_50_vs_100,
                                                                                         fill = pyr_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Pyrethroid Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  ylab("Frequency")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

pyr_50_vs_75_plot= ggplot(subset(smooth.truncation.df.1, model == "polytruncate"), aes(x=pyr_50_vs_75,
                                                                                       fill = pyr_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Pyrethroid Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

p.1 = novel_75_vs_100_plot + pyr_75_vs_100_plot +
  plot_annotation(title = "Polytruncate: Comparing FD_FD vs HD_HD retains 75%",
                  theme = theme(plot.title = element_text(size = 22)))

p.2 = novel_50_vs_100_plot + pyr_50_vs_100_plot +
  plot_annotation(title = "Polytruncate: Comparing FD_FD vs HD_HD retains 50%",
                  theme = theme(plot.title = element_text(size = 22)))

p.3 = novel_50_vs_75_plot + pyr_50_vs_75_plot +
  plot_annotation(title = "Polytruncate: Comparing HD_HD retains 75% vs HD_HD retains 50%",
                  theme = theme(plot.title = element_text(size = 22)))

p.1/p.2/p.3

ggsave(
  filename = "chapter5_figureS6a.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 800,
  height = 600,
  units = "px",
  dpi = 300)


novel_75_vs_100_plot = ggplot(subset(smooth.truncation.df.1, model == "polysmooth"), aes(x=novel_75_vs_100,
                                                                                         fill = novel_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Novel Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

novel_50_vs_100_plot = ggplot(subset(smooth.truncation.df.1, model == "polysmooth"), aes(x=novel_50_vs_100,
                                                                                         fill = novel_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Novel Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

novel_50_vs_75_plot= ggplot(subset(smooth.truncation.df.1, model == "polysmooth"), aes(x=novel_50_vs_75,
                                                                                       fill = novel_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Novel Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

pyr_75_vs_100_plot = ggplot(subset(smooth.truncation.df.1, model == "polysmooth"), aes(x=pyr_75_vs_100,
                                                                                       fill = pyr_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Pyrethroid Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

pyr_50_vs_100_plot = ggplot(subset(smooth.truncation.df.1, model == "polysmooth"), aes(x=pyr_50_vs_100,
                                                                                       fill = pyr_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#984ea3",
                               "#e41a1c"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Pyrethroid Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  ylab("Frequency")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

pyr_50_vs_75_plot= ggplot(subset(smooth.truncation.df.1, model == "polysmooth"), aes(x=pyr_50_vs_75,
                                                                                     fill = pyr_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#ff7f00",
                               "#984ea3"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab(paste0("Percentage Difference in Pyrethroid Bioassay Survival\nAfter 200 Generations"))+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  ylab("Frequency")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        title = element_text(size = 12),
        strip.text = element_text(size = 12))

p.1 = novel_75_vs_100_plot + pyr_75_vs_100_plot +
  plot_annotation(title = "polysmooth: Comparing FD_FD vs HD_HD retains 75%",
                  theme = theme(plot.title = element_text(size = 22)))

p.2 = novel_50_vs_100_plot + pyr_50_vs_100_plot +
  plot_annotation(title = "polysmooth: Comparing FD_FD vs HD_HD retains 50%",
                  theme = theme(plot.title = element_text(size = 22)))

p.3 = novel_50_vs_75_plot + pyr_50_vs_75_plot +
  plot_annotation(title = "polysmooth: Comparing HD_HD retains 75% vs HD_HD retains 50%",
                  theme = theme(plot.title = element_text(size = 22)))

p.1/p.2/p.3

ggsave(
  filename = "chapter5_figureS6b.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 800,
  height = 600,
  units = "px",
  dpi = 300)

