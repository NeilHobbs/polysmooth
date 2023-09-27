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




rm(set.75s)







#one dataframe
smooth.scaled.df = rbind(smooth.df.1,
                             smooth.df.0.75,
                             smooth.df.0.5)

rm(smooth.df)


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



#Define dosing strategies
smooth.scaled.df$dosing.strategy = ifelse(smooth.scaled.df$dose.1 == 1,
                                              yes = "Full Dose",
                                              no = ifelse(smooth.scaled.df$dose.1 == 0.5,
                                                          yes = "Half Dose retains 50% Efficacy",
                                                          no = "Half Dose retains 75% Efficacy"))

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


pyrethroid.plot = ggplot(smooth.scaled.df, aes(x=rate.change.pyrethroid.percent,
                                                   fill = pyrethroid.rate.change))+
  geom_histogram(binwidth = 1)+
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
  xlim(-100, 10)+
  xlab("Change in the Rate of Evolution")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  facet_grid(dosing.strategy ~ start.bioassay.pyrethroid)+
  theme_bw()+
  theme(legend.position = "none")

novel.plot = ggplot(smooth.scaled.df, aes(x=rate.change.novel.percent,
                                              fill = novel.rate.change))+
  geom_histogram(binwidth = 1)+
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
  xlim(-100, 10)+
  xlab("Change in the Rate of Evolution")+
  ggtitle("polysmooth: Novel Insecticide")+
  facet_grid(dosing.strategy ~ start.bioassay.pyrethroid)+
  theme_bw()+
  theme(legend.position = "none")


novel.plot + pyrethroid.plot

rm(novel.plot, pyrethroid.plot)


######

##gets the bioassay survival stuff

#colour scheme:
#100 =  #35978f    teal
#75 = #810f7c   purple
#50 = #b2182b   red



novel_75_vs_100_plot = ggplot(smooth.df.1, aes(x=novel_75_vs_100,
                        fill = novel_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#810f7c", "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlim(-50, 150)+
  theme_classic()+
  theme(legend.position = "none")


novel_50_vs_100_plot = ggplot(smooth.df.1, aes(x=novel_50_vs_100,
                                                  fill = novel_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#b2182b", "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlim(-50, 150)+
  theme_classic()+
  theme(legend.position = "none")

novel_50_vs_75_plot= ggplot(smooth.df.1, aes(x=novel_50_vs_75,
                                                fill = novel_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c( "#810f7c", "#b2182b"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Novel Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlim(-50, 150)+
  theme_classic()+
  theme(legend.position = "none")

pyr_75_vs_100_plot = ggplot(smooth.df.1, aes(x=pyr_75_vs_100,
                                                fill = pyr_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#810f7c", "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlim(-50, 150)+
  theme_classic()+
  theme(legend.position = "none")

pyr_50_vs_100_plot = ggplot(smooth.df.1, aes(x=pyr_50_vs_100,
                                                fill = pyr_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#b2182b", "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlim(-50, 150)+
  theme_classic()+
  theme(legend.position = "none")

pyr_50_vs_75_plot= ggplot(smooth.df.1, aes(x=pyr_50_vs_75,
                                              fill = pyr_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#810f7c", "#b2182b"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Pyrethroid Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlim(-50, 150)+
  theme_classic()+
  theme(legend.position = "none")

total_75_vs_100_plot = ggplot(smooth.df.1, aes(x=total_75_vs_100,
                                                  fill = total_75_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#810f7c", "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Total Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlim(-50, 150)+
  theme_classic()+
  theme(legend.position = "none")

total_50_vs_100_plot = ggplot(smooth.df.1, aes(x=total_50_vs_100,
                                                  fill = total_50_vs_100_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#b2182b", "#35978f"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Total Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlim(-50, 150)+
  theme_classic()+
  theme(legend.position = "none")

total_50_vs_75_plot= ggplot(smooth.df.1, aes(x=total_50_vs_75,
                                                fill = total_50_vs_75_outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("#810f7c", "#b2182b"))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Percentage Difference in Total Bioassay Change")+
  facet_grid(start.bioassay.pyrethroid ~ .)+
  xlim(-50, 150)+
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
  plot_layout(design = plot.layout) +
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
#colour scheme:
#100 =  #35978f    teal
#75 = #810f7c   purple
#50 = #b2182b   red

female.novel.gam = ggplot(smooth.scaled.df, aes(x=female.exposure,
                                                    y=rate.change.novel.percent,
                                                    colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Female Insecticide Encounter Probability")+
  ggtitle("polysmooth: Novel Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")


female.pyrethroid.gam = ggplot(smooth.scaled.df, aes(x=female.exposure,
                                                         y=rate.change.pyrethroid.percent,
                                                         colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Female Insecticide Encounter Probability")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")

female.novel.gam + female.pyrethroid.gam

rm(female.pyrethroid.gam, female.novel.gam)

male.novel.gam = ggplot(smooth.scaled.df, aes(x=male.exposure,
                                                y=rate.change.novel.percent,
                                                colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Male Insecticide Encounter Probability as a proportion of Female")+
  ggtitle("polysmooth: Novel Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")


male.pyrethroid.gam = ggplot(smooth.scaled.df, aes(x=male.exposure,
                                                     y=rate.change.pyrethroid.percent,
                                                     colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Male Insecticide Encounter Probability as a proportion of Female")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")

male.novel.gam + male.pyrethroid.gam

rm(male.pyrethroid.gam, male.novel.gam)


heritability.novel.gam = ggplot(smooth.scaled.df, aes(x=heritability,
                                              y=rate.change.novel.percent,
                                              colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Heritability")+
  ggtitle("polysmooth: Novel Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")


heritability.pyrethroid.gam = ggplot(smooth.scaled.df, aes(x=heritability,
                                                   y=rate.change.pyrethroid.percent,
                                                   colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Heritability")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")

heritability.novel.gam + heritability.pyrethroid.gam

rm(heritability.pyrethroid.gam, heritability.novel.gam)


coverage.novel.gam = ggplot(smooth.scaled.df, aes(x=intervention.coverage,
                                                      y=rate.change.novel.percent,
                                                      colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Intervention Coverage")+
  ggtitle("polysmooth: Novel Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")


coverage.pyrethroid.gam = ggplot(smooth.scaled.df, aes(x=intervention.coverage,
                                                           y=rate.change.pyrethroid.percent,
                                                           colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Intervention Coverage")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")

coverage.novel.gam + coverage.pyrethroid.gam

rm(coverage.pyrethroid.gam, coverage.novel.gam)

dispersal.novel.gam = ggplot(smooth.scaled.df, aes(x=dispersal,
                                                  y=rate.change.novel.percent,
                                                  colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Dispersal Rate")+
  ggtitle("polysmooth: Novel Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")


dispersal.pyrethroid.gam = ggplot(smooth.scaled.df, aes(x=dispersal,
                                                       y=rate.change.pyrethroid.percent,
                                                       colour = dosing.strategy))+
  geom_smooth(method = "gam")+
  scale_colour_manual(values = c("#35978f",
                                 "#810f7c",
                                 "#b2182b"))+
  xlab("Dispersal Rate")+
  ggtitle("polysmooth: Pyrethroid Insecticide")+
  ylab("Change in the Rate of Evolution")+
  facet_grid(. ~ start.bioassay.pyrethroid)+
  guides(fill=guide_legend(title="Dosing Strategy"))+
  theme_bw()+
  theme(legend.position = "bottom")

dispersal.novel.gam + dispersal.pyrethroid.gam

rm(coverage.pyrethroid.gam, coverage.novel.gam)





