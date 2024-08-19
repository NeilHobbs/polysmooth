library(devtools)
load_all()
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggh4x)
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


smooth.truncation.df$base.decay.1.descriptor = factor(ifelse(smooth.truncation.df$base.decay.1 == 0.025,
                                                        yes = "Pyrethroid: Faster",
                                                        no = ifelse(smooth.truncation.df$base.decay.1 ==  0.005,
                                                                    yes = "Pyrethroid: Slower",
                                                                    no = "Pyrethroid: Default")),
                                                      levels = c("Pyrethroid: Slower", "Pyrethroid: Default", "Pyrethroid: Faster"))

smooth.truncation.df$base.decay.2.descriptor = factor(ifelse(smooth.truncation.df$base.decay.2 ==  0.025,
                                                        yes = "Novel: Faster",
                                                        no = ifelse(smooth.truncation.df$base.decay.2 == 0.005,
                                                                    yes = "Novel: Slower",
                                                                    no = "Novel: Default")),
                                                      levels = c("Novel: Slower", "Novel: Default", "Novel: Faster"))


novel.plot = ggplot(smooth.truncation.df, aes(x=dosing.strategy,
                                 fill = dosing.strategy,
                                 y = rate.change.novel.percent,
       colour = as.factor(threshold.gens)))+
  geom_violin()+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_colour_manual(values = c("#fde0dd",
                                 "#fa9fb5",
                                 "#c51b8a"
      ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2(start.bioassay.pyrethroid ~ base.decay.1.descriptor + base.decay.2.descriptor,

              strip =   strip_themed(

              # Horizontal strips
              background_x = elem_list_rect(fill = c(rep(c("#fdb462","#8dd3c7","#bc80bd"), each = 3),
                                                     rep(c("#fdb462","#8dd3c7","#bc80bd"), times = 3)),

              by_layer_x = FALSE),
              background_y = elem_list_rect(fill = rep("white", 4),
                                            by_layer_y = FALSE)

              ))+
  ylab(paste0("Change in the rate of evolution (%)\n for the novel insecticide when in mixture\ncompared to monotherapy deployment"))+
  guides(fill=guide_legend(title="Mixture Dosing"),
         colour =guide_legend(title="Threshold Decay Generation"))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))



pyrethroid.plot = ggplot(smooth.truncation.df, aes(x=dosing.strategy,
                                 fill = dosing.strategy,
                                 y = rate.change.pyrethroid.percent,
                                 colour = as.factor(threshold.gens)))+
  geom_violin()+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_colour_manual(values = c("#fde0dd",
                                 "#fa9fb5",
                                 "#c51b8a"
  ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2(start.bioassay.pyrethroid ~ base.decay.1.descriptor + base.decay.2.descriptor,

              strip =   strip_themed(

                # Horizontal strips
                background_x = elem_list_rect(fill = c(rep(c("#fdb462","#8dd3c7","#bc80bd"), each = 3),
                                                       rep(c("#fdb462","#8dd3c7","#bc80bd"), times = 3)),

                                              by_layer_x = FALSE),
                background_y = elem_list_rect(fill = rep("white", 4),
                                              by_layer_y = FALSE)

              ))+
  ylab(paste0("Change in the rate of evolution (%)\n for the pyrethroid when in mixture\ncompared to monotherapy deployment"))+
  guides(fill=guide_legend(title="Mixture Dosing"),
         colour =guide_legend(title="Threshold Decay Generation"))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1.5, "cm"))

overall.plot = novel.plot / pyrethroid.plot


ggsave(
  filename = "Mixture_manuscript_Dosing_Decay_Resistance_novel.jpeg",
  plot = novel.plot,
  scale = 5,
  width = 2000,
  height = 1200,
  units = "px",
  dpi = 300)

ggsave(
  filename = "Mixture_manuscript_Dosing_Decay_Resistance_pyrethroid.jpeg",
  plot = pyrethroid.plot,
  scale = 5,
  width = 2000,
  height = 1200,
  units = "px",
  dpi = 300)



novel.plot.0 = ggplot(subset(smooth.truncation.df, start.bioassay.pyrethroid == 0), aes(x=dosing.strategy,
                                                                                        fill = dosing.strategy,
                                                                                        y = rate.change.novel.percent,
                                                                                        colour = as.factor(threshold.gens)))+
  geom_violin()+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(limits = c(-100, 7))+
  scale_colour_manual(values = c("#fde0dd",
                                 "#fa9fb5",
                                 "#c51b8a"
  ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2( base.decay.1.descriptor ~ base.decay.2.descriptor,

               strip =   strip_themed(

                 # Horizontal strips
                 background_x = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),

                                               by_layer_x = FALSE),
                 background_y = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),
                                               by_layer_y = FALSE)

               ))+
  ylab(paste0("Change in the rate of evolution (%) for the novel insecticide \nwhen in mixture compared to monotherapy deployment"))+  guides(fill=guide_legend(title="Mixture Dosing"),
                                                                                                                                              colour =guide_legend(title="Threshold Decay Generation"))+
  ggtitle("Initial Pyrethroid Bioassay Survival: 0%")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

novel.plot.10 = ggplot(subset(smooth.truncation.df, start.bioassay.pyrethroid == 10), aes(x=dosing.strategy,
                                                                                          fill = dosing.strategy,
                                                                                          y = rate.change.novel.percent,
                                                                                          colour = as.factor(threshold.gens)))+
  geom_violin()+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(limits = c(-100, 7))+
  scale_colour_manual(values = c("#fde0dd",
                                 "#fa9fb5",
                                 "#c51b8a"
  ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2( base.decay.1.descriptor ~ base.decay.2.descriptor,

               strip =   strip_themed(

                 # Horizontal strips
                 background_x = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),

                                               by_layer_x = FALSE),
                 background_y = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),
                                               by_layer_y = FALSE)

               ))+
  ylab(paste0("Change in the rate of evolution (%) for the novel insecticide \nwhen in mixture compared to monotherapy deployment"))+  guides(fill=guide_legend(title="Mixture Dosing"),
                                                                                                                                              colour =guide_legend(title="Threshold Decay Generation"))+
  ggtitle("Initial Pyrethroid Bioassay Survival: 10%")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

novel.plot.50 = ggplot(subset(smooth.truncation.df, start.bioassay.pyrethroid == 50), aes(x=dosing.strategy,
                                                                                          fill = dosing.strategy,
                                                                                          y = rate.change.novel.percent,
                                                                                          colour = as.factor(threshold.gens)))+
  geom_violin()+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(limits = c(-100, 7))+
  scale_colour_manual(values = c("#fde0dd",
                                 "#fa9fb5",
                                 "#c51b8a"
  ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2( base.decay.1.descriptor ~ base.decay.2.descriptor,

               strip =   strip_themed(

                 # Horizontal strips
                 background_x = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),

                                               by_layer_x = FALSE),
                 background_y = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),
                                               by_layer_y = FALSE)

               ))+
  ylab(paste0("Change in the rate of evolution (%) for the novel insecticide \nwhen in mixture compared to monotherapy deployment"))+  guides(fill=guide_legend(title="Mixture Dosing"),
                                                                                                                                              colour =guide_legend(title="Threshold Decay Generation"))+
  ggtitle("Initial Pyrethroid Bioassay Survival: 50%")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

novel.plot.80 = ggplot(subset(smooth.truncation.df, start.bioassay.pyrethroid == 80), aes(x=dosing.strategy,
                                                                                          fill = dosing.strategy,
                                                                                          y = rate.change.novel.percent,
                                                                                          colour = as.factor(threshold.gens)))+
  geom_violin()+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(limits = c(-100, 7))+
  scale_colour_manual(values = c("#fde0dd",
                                 "#fa9fb5",
                                 "#c51b8a"
  ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2( base.decay.1.descriptor ~ base.decay.2.descriptor,

               strip =   strip_themed(

                 # Horizontal strips
                 background_x = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),

                                               by_layer_x = FALSE),
                 background_y = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),
                                               by_layer_y = FALSE)

               ))+
  ylab(paste0("Change in the rate of evolution (%) for the novel insecticide \nwhen in mixture compared to monotherapy deployment"))+
  guides(fill=guide_legend(title="Mixture Dosing"),
         colour =guide_legend(title="Threshold Decay Generation"))+
  ggtitle("Initial Pyrethroid Bioassay Survival: 80%")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))





the.legend = cowplot::get_plot_component(ggplot(subset(smooth.truncation.df, start.bioassay.pyrethroid == 80), aes(x=dosing.strategy,
                                                                                                                   fill = dosing.strategy,
                                                                                                                   y = rate.change.novel.percent,
                                                                                                                   colour = as.factor(threshold.gens)))+
                                           geom_violin()+
                                           geom_vline(xintercept = 0, linetype = "dashed",
                                                      colour = "black")+
                                           scale_y_continuous(limits = c(-100, 7))+
                                           scale_colour_manual(values = c("#fde0dd",
                                                                          "#fa9fb5",
                                                                          "#c51b8a"
                                           ))+
                                           scale_fill_manual(values = c("#8dd3c7",#FD_FD
                                                                        "#ffffb3",#D_HD 75%
                                                                        "#bebada",#HD_HD 50%
                                                                        "#fb8072",#FD_HD
                                                                        "#80b1d3" #HD_FD
                                           ))+
                                           facet_grid2( base.decay.1.descriptor ~ base.decay.2.descriptor,

                                                        strip =   strip_themed(

                                                          # Horizontal strips
                                                          background_x = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),

                                                                                        by_layer_x = FALSE),
                                                          background_y = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),
                                                                                        by_layer_y = FALSE)

                                                        ))+
                                           ylab(paste0("Change in the rate of evolution (%)\n for the novel insecticide when in mixture\ncompared to monotherapy deployment"))+
                                           guides(fill=guide_legend(title="Mixture Dosing"),
                                                  colour =guide_legend(title="Threshold Decay Generation"))+
                                           ggtitle("Initial Pyrethroid Bioassay Survival: 80%")+
                                           theme_bw()+
                                           theme(legend.position = "bottom",
                                                 axis.text.x = element_blank(),
                                                 axis.ticks.x = element_blank(),
                                                 axis.title.x = element_blank(),
                                                 axis.title.y = element_text(size = 18),
                                                 title = element_text(size = 20),
                                                 strip.text = element_text(size = 18)),
                                         "guide-box-bottom")








the.layout = "
AB
AB
AB
AB
AB
AB
AB
CD
CD
CD
CD
CD
CD
CD
EE"

novel.plot.0 + novel.plot.10 +
  novel.plot.50 + novel.plot.80 +
  the.legend +
  plot_layout(design = the.layout) +
  plot_annotation(title = "Scenario 1: Novel Insecticide")


ggsave(
  filename = "Mixture_manuscript_Dosing_Decay_Resistance_novel_0.10.50.80.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 1200,
  height = 1200,
  units = "px",
  dpi = 300)










pyrethroid.plot.0 = ggplot(subset(smooth.truncation.df, start.bioassay.pyrethroid == 0), aes(x=dosing.strategy,
                                                                                             fill = dosing.strategy,
                                                                                             y = rate.change.pyrethroid.percent,
                                                                                             colour = as.factor(threshold.gens)))+
  geom_violin()+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(limits = c(-100, 7))+
  scale_colour_manual(values = c("#fde0dd",
                                 "#fa9fb5",
                                 "#c51b8a"
  ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2( base.decay.1.descriptor ~ base.decay.2.descriptor,

               strip =   strip_themed(

                 # Horizontal strips
                 background_x = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),

                                               by_layer_x = FALSE),
                 background_y = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),
                                               by_layer_y = FALSE)

               ))+
  ylab(paste0("Change in the rate of evolution (%) for the pyrethroid insecticide \nwhen in mixture compared to monotherapy deployment"))+  guides(fill=guide_legend(title="Mixture Dosing"),
                                                                                                                                                   colour =guide_legend(title="Threshold Decay Generation"))+
  ggtitle("Initial Pyrethroid Bioassay Survival: 0%")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

pyrethroid.plot.10 = ggplot(subset(smooth.truncation.df, start.bioassay.pyrethroid == 10), aes(x=dosing.strategy,
                                                                                               fill = dosing.strategy,
                                                                                               y = rate.change.pyrethroid.percent,
                                                                                               colour = as.factor(threshold.gens)))+
  geom_violin()+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(limits = c(-100, 7))+
  scale_colour_manual(values = c("#fde0dd",
                                 "#fa9fb5",
                                 "#c51b8a"
  ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2( base.decay.1.descriptor ~ base.decay.2.descriptor,

               strip =   strip_themed(

                 # Horizontal strips
                 background_x = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),

                                               by_layer_x = FALSE),
                 background_y = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),
                                               by_layer_y = FALSE)

               ))+
  ylab(paste0("Change in the rate of evolution (%) for the pyrethroid insecticide \nwhen in mixture compared to monotherapy deployment"))+  guides(fill=guide_legend(title="Mixture Dosing"),
                                                                                                                                                   colour =guide_legend(title="Threshold Decay Generation"))+
  ggtitle("Initial Pyrethroid Bioassay Survival: 10%")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

pyrethroid.plot.50 = ggplot(subset(smooth.truncation.df, start.bioassay.pyrethroid == 50), aes(x=dosing.strategy,
                                                                                               fill = dosing.strategy,
                                                                                               y = rate.change.pyrethroid.percent,
                                                                                               colour = as.factor(threshold.gens)))+
  geom_violin()+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(limits = c(-100, 7))+
  scale_colour_manual(values = c("#fde0dd",
                                 "#fa9fb5",
                                 "#c51b8a"
  ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2( base.decay.1.descriptor ~ base.decay.2.descriptor,

               strip =   strip_themed(

                 # Horizontal strips
                 background_x = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),

                                               by_layer_x = FALSE),
                 background_y = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),
                                               by_layer_y = FALSE)

               ))+
  ylab(paste0("Change in the rate of evolution (%) for the pyrethroid insecticide \nwhen in mixture compared to monotherapy deployment"))+  guides(fill=guide_legend(title="Mixture Dosing"),
                                                                                                                                                   colour =guide_legend(title="Threshold Decay Generation"))+
  ggtitle("Initial Pyrethroid Bioassay Survival: 50%")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

pyrethroid.plot.80 = ggplot(subset(smooth.truncation.df, start.bioassay.pyrethroid == 80), aes(x=dosing.strategy,
                                                                                               fill = dosing.strategy,
                                                                                               y = rate.change.pyrethroid.percent,
                                                                                               colour = as.factor(threshold.gens)))+
  geom_violin()+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_y_continuous(limits = c(-100, 7))+
  scale_colour_manual(values = c("#fde0dd",
                                 "#fa9fb5",
                                 "#c51b8a"
  ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2( base.decay.1.descriptor ~ base.decay.2.descriptor,

               strip =   strip_themed(

                 # Horizontal strips
                 background_x = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),

                                               by_layer_x = FALSE),
                 background_y = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),
                                               by_layer_y = FALSE)

               ))+
  ylab(paste0("Change in the rate of evolution (%) for the pyrethroid insecticide \nwhen in mixture compared to monotherapy deployment"))+
  guides(fill=guide_legend(title="Mixture Dosing"),
         colour =guide_legend(title="Threshold Decay Generation"))+
  ggtitle("Initial Pyrethroid Bioassay Survival: 80%")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))





the.legend = cowplot::get_plot_component(ggplot(subset(smooth.truncation.df, start.bioassay.pyrethroid == 80), aes(x=dosing.strategy,
                                                                                                                   fill = dosing.strategy,
                                                                                                                   y = rate.change.pyrethroid.percent,
                                                                                                                   colour = as.factor(threshold.gens)))+
                                           geom_violin()+
                                           geom_vline(xintercept = 0, linetype = "dashed",
                                                      colour = "black")+
                                           scale_y_continuous(limits = c(-100, 7))+
                                           scale_colour_manual(values = c("#fde0dd",
                                                                          "#fa9fb5",
                                                                          "#c51b8a"
                                           ))+
                                           scale_fill_manual(values = c("#8dd3c7",#FD_FD
                                                                        "#ffffb3",#D_HD 75%
                                                                        "#bebada",#HD_HD 50%
                                                                        "#fb8072",#FD_HD
                                                                        "#80b1d3" #HD_FD
                                           ))+
                                           facet_grid2( base.decay.1.descriptor ~ base.decay.2.descriptor,

                                                        strip =   strip_themed(

                                                          # Horizontal strips
                                                          background_x = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),

                                                                                        by_layer_x = FALSE),
                                                          background_y = elem_list_rect(fill = c("#fdb462","#8dd3c7","#bc80bd"),
                                                                                        by_layer_y = FALSE)

                                                        ))+
                                           ylab(paste0("Change in the rate of evolution (%)\n for the pyrethroid insecticide when in mixture\ncompared to monotherapy deployment"))+
                                           guides(fill=guide_legend(title="Mixture Dosing"),
                                                  colour =guide_legend(title="Threshold Decay Generation"))+
                                           ggtitle("Initial Pyrethroid Bioassay Survival: 80%")+
                                           theme_bw()+
                                           theme(legend.position = "bottom",
                                                 axis.text.x = element_blank(),
                                                 axis.ticks.x = element_blank(),
                                                 axis.title.x = element_blank(),
                                                 axis.title.y = element_text(size = 18),
                                                 title = element_text(size = 20),
                                                 strip.text = element_text(size = 18)),
                                         "guide-box-bottom")








the.layout = "
AB
AB
AB
AB
AB
AB
AB
CD
CD
CD
CD
CD
CD
CD
EE"

pyrethroid.plot.0 + pyrethroid.plot.10 +
  pyrethroid.plot.50 + pyrethroid.plot.80 +
  the.legend +
  plot_layout(design = the.layout) +
  plot_annotation(title = "Scenario 1: Pyrethroid Insecticide")


ggsave(
  filename = "Mixture_manuscript_Dosing_Decay_Resistance_pyrethroid_0.10.50.80.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 1200,
  height = 1200,
  units = "px",
  dpi = 300)






pyrethroid.summary.df = smooth.truncation.df|>
  dplyr::group_by(dosing.strategy,
                  start.bioassay.pyrethroid,
                  threshold.gens,
                  base.decay.1.descriptor,
                  base.decay.2.descriptor)|>
  dplyr::summarise(mean.vals = mean(rate.change.pyrethroid.percent))


novel.summary.df = smooth.truncation.df|>
  dplyr::group_by(dosing.strategy,
                  start.bioassay.pyrethroid,
                  threshold.gens,
                  base.decay.1.descriptor,
                  base.decay.2.descriptor)|>
  dplyr::summarise(mean.vals = mean(rate.change.novel.percent))

pyr.summary.plot = ggplot(pyrethroid.summary.df, aes(x= as.factor(start.bioassay.pyrethroid),
                                                     fill = dosing.strategy,
                                                     y = dosing.strategy,
                                                     alpha = -(mean.vals)/100))+
  geom_tile(colour = "black")+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  scale_x_discrete(labels = c("0%", "10%", "50%", "80%"))+

  geom_text(aes(x = as.factor(start.bioassay.pyrethroid), y = dosing.strategy, label = round(mean.vals, 1)),
            size = 9, colour = "black", alpha = 1)+

  facet_grid2(threshold.gens ~ base.decay.1.descriptor + base.decay.2.descriptor,

              strip =   strip_themed(

                # Horizontal strips
                background_x = elem_list_rect(fill = c(rep(c("#fdb462","#8dd3c7","#bc80bd"), each = 3),
                                                       rep(c("#fdb462","#8dd3c7","#bc80bd"), times = 3)),

                                              by_layer_x = FALSE),
                background_y = elem_list_rect(fill = c("#fde0dd",
                                                       "#fa9fb5",
                                                       "#c51b8a"
                ),
                by_layer_y = FALSE)

              ))+
  xlab("Initial Pyrethroid Bioassay Survival")+
  guides(fill=guide_legend(title="Mixture Dosing"),
         alpha = "none")+
  ggtitle("Pyrethroid")+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 15, colour = "black"),
        title = element_text(size = 20),
        strip.text.x = element_text(size = 12))


novel.summary.plot = ggplot(novel.summary.df, aes(x= as.factor(start.bioassay.pyrethroid),
                                                       fill = dosing.strategy,
                                                       y = dosing.strategy,
                                                  alpha = -(mean.vals)/100))+
  geom_tile(colour = "black")+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  scale_x_discrete(labels = c("0%", "10%", "50%", "80%"))+

  geom_text(aes(x = as.factor(start.bioassay.pyrethroid), y = dosing.strategy, label = round(mean.vals, 1)),
                size = 9, colour = "black", alpha = 1)+

  facet_grid2(threshold.gens ~ base.decay.1.descriptor + base.decay.2.descriptor,

              strip =   strip_themed(

                # Horizontal strips
                background_x = elem_list_rect(fill = c(rep(c("#fdb462","#8dd3c7","#bc80bd"), each = 3),
                                                       rep(c("#fdb462","#8dd3c7","#bc80bd"), times = 3)),

                                              by_layer_x = FALSE),
                background_y = elem_list_rect(fill = c("#fde0dd",
                                                       "#fa9fb5",
                                                       "#c51b8a"
                ),
                by_layer_y = FALSE)

              ))+
  xlab("Initial Pyrethroid Bioassay Survival")+
  guides(fill=guide_legend(title="Mixture Dosing"),
         alpha = "none")+
  ggtitle("Novel")+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 15, colour = "black"),
        title = element_text(size = 20),
        strip.text.x = element_text(size = 12))


ggsave(
  filename = "Mixture_manuscript_Dosing_Decay_Resistance_pyrethroid_summary.jpeg",
  plot = pyr.summary.plot,
  scale = 5,
  width = 2000,
  height = 600,
  units = "px",
  dpi = 300)

ggsave(
  filename = "Mixture_manuscript_Dosing_Decay_Resistance_novel_summary.jpeg",
  plot = novel.summary.plot,
  scale = 5,
  width = 2000,
  height = 600,
  units = "px",
  dpi = 300)



















for(i in 1:5){

better.names = c("Heritability",
                 "Male Insecticide Exposure",
                 "Female Insecticide Exposure",
                 "Intervention Coverage",
                 "Disperal Rate")

col.i.values = 13:17

gam.df = smooth.truncation.df[ , c(..col.i.values[..i], 33, 34, 27, 20, 37)]

gam.df$parameter = gam.df[ , 1]

ggplot(gam.df, aes(x=parameter,
                                 fill = dosing.strategy,
                                 y = rate.change.novel.percent,
                                 colour = dosing.strategy))+
  geom_smooth()+
  scale_y_continuous(limits = c(-100, 7))+
  scale_colour_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2(model ~ start.bioassay.pyrethroid,

               strip =   strip_themed(

                 # Horizontal strips
                 background_x = elem_list_rect(fill = "white",

                                               by_layer_x = FALSE),
                 background_y = elem_list_rect(fill = "white",
                                               by_layer_y = FALSE)

               ))+
  ylab(paste0("Change in the rate of evolution (%) for the novel insecticide \nwhen in mixture compared to monotherapy deployment"))+
  xlab(better.names[i])+
  guides(fill=guide_legend(title="Mixture Dosing"),
         colour=guide_legend(title="Mixture Dosing"))+
  ggtitle("Initial Pyrethroid Bioassay Survival (%)")+
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18)) +

  ggplot(gam.df, aes(x=parameter,
                     fill = dosing.strategy,
                     y = rate.change.pyrethroid.percent,
                     colour = dosing.strategy))+
  geom_smooth()+
  scale_y_continuous(limits = c(-100, 7))+
  scale_colour_manual(values = c("#8dd3c7",#FD_FD
                                 "#ffffb3",#D_HD 75%
                                 "#bebada",#HD_HD 50%
                                 "#fb8072",#FD_HD
                                 "#80b1d3" #HD_FD
  ))+
  scale_fill_manual(values = c("#8dd3c7",#FD_FD
                               "#ffffb3",#D_HD 75%
                               "#bebada",#HD_HD 50%
                               "#fb8072",#FD_HD
                               "#80b1d3" #HD_FD
  ))+
  facet_grid2(model ~ start.bioassay.pyrethroid,

              strip =   strip_themed(

                # Horizontal strips
                background_x = elem_list_rect(fill = "white",

                                              by_layer_x = FALSE),
                background_y = elem_list_rect(fill = "white",
                                              by_layer_y = FALSE)

              ))+
  ylab(paste0("Change in the rate of evolution (%) for the pyrethroid insecticide \nwhen in mixture compared to monotherapy deployment"))+
  xlab(better.names[i])+
  guides(fill=guide_legend(title="Mixture Dosing"),
         colour=guide_legend(title="Mixture Dosing"))+
  ggtitle("Initial Pyrethroid Bioassay Survival (%)")+
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 18),
        title = element_text(size = 20),
        strip.text = element_text(size = 18))

ggsave(
  filename = paste0("Mixture_manuscript_Dosing_Decay_Resistance_parameterGAM",i,".jpeg"),
  plot = last_plot(),
  scale = 5,
  width = 1200,
  height = 800,
  units = "px",
  dpi = 300)

}

