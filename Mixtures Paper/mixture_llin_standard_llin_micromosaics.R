##Micromosaics of  Mixture ITN and Standard ITN

library(devtools)
load_all()
library(ggplot2)
library(data.table)
library(patchwork)
library(ggh4x)
#First set of analysis just does wins/losses/draws
#Second set of analysis delves into the magnitude of the wins/draws/losses

micromosaics.mixture.standard = fread("mixture.llin_standard.llin_micromosaics.csv")
micromosaics.hd.mixture.standard = fread("half.dose.50.mixture.llin_standard.llin_micromosaics.csv")
micromosaics.mixture.standard$dose = "full dose"

#get the standard ITN only simulations:: ["Lower Benchmark"]
standard.llin.only = subset(micromosaics.mixture.standard, coverage.llin == 1)

#get the mixture llin only simulations:: ["Upper Benchmark"]
mixture.llin.only = subset(micromosaics.mixture.standard, coverage.mixture == 1)

#colnames(mixture.llin.only)

#colnames(micromosaics.mixture.standard)

#rename columns so can be easily added to micromosaics dataframe
mixture.llin.only =  mixture.llin.only |>
  dplyr::rename(peak.survival.i.mixture = peak.survival.i)|>
  dplyr::rename(peak.survival.j.mixture = peak.survival.j)|>
  dplyr::rename(mean.survival.i.mixture = mean.survival.i)|>
  dplyr::rename(mean.survival.j.mixture = mean.survival.j)|>
  dplyr::rename(median.survival.i.mixture = median.survival.i)|>
  dplyr::rename(median.survival.j.mixture = median.survival.j)|>
  dplyr::select(-"V1", -"X", -"coverage.mixture", -"coverage.llin")

standard.llin.only =  standard.llin.only |>
  dplyr::rename(peak.survival.i.standard = peak.survival.i)|>
  dplyr::rename(peak.survival.j.standard  = peak.survival.j)|>
  dplyr::rename(mean.survival.i.standard  = mean.survival.i)|>
  dplyr::rename(mean.survival.j.standard  = mean.survival.j)|>
  dplyr::rename(median.survival.i.standard  = median.survival.i)|>
  dplyr::rename(median.survival.j.standard  = median.survival.j)|>
  dplyr::select(-"V1", -"X", -"coverage.mixture", -"coverage.llin")

#Join micromosaics and mixtures only results
A = dplyr::right_join(micromosaics.mixture.standard, mixture.llin.only,
                      by = c("Heritability",
                             "Male.Insecticide.Exposure", "Female.Insecticide.Exposure", "Female.Fitness.Cost",
                             "Male.Fitness.Cost", "Intervention.Coverage", "Dispersal", "pyr.start.resistance"))

#add in standard llin only results
micromosaics.mixture.standard.df = dplyr::right_join(A, standard.llin.only,
                                                     by = c("Heritability",
                                                            "Male.Insecticide.Exposure", "Female.Insecticide.Exposure", "Female.Fitness.Cost",
                                                            "Male.Fitness.Cost", "Intervention.Coverage", "Dispersal", "pyr.start.resistance"))

#remove no longer required dataframes

colnames(micromosaics.mixture.standard.df)

micromosaics.mixture.standard.df = micromosaics.mixture.standard.df|>
  dplyr::mutate(pyrethroid.start = round(convert_resistance_score_to_bioassay_survival(trait.mean = pyr.start.resistance)*100,
                                         1))


####Now look at issue of half dose mixtures:::
#get the mixture llin only simulations:: ["Upper Benchmark"]
hd.mixture.llin.only = subset(micromosaics.hd.mixture.standard, coverage.mixture == 1)

standard.llin.only = subset(micromosaics.mixture.standard, coverage.llin == 1)

colnames(hd.mixture.llin.only)

#colnames(micromosaics.mixture.standard)
hd.mixture.llin.only =  hd.mixture.llin.only |>
  dplyr::rename(peak.survival.i.mixture = peak.survival.i)|>
  dplyr::rename(peak.survival.j.mixture = peak.survival.j)|>
  dplyr::rename(mean.survival.i.mixture = mean.survival.i)|>
  dplyr::rename(mean.survival.j.mixture = mean.survival.j)|>
  dplyr::rename(median.survival.i.mixture = median.survival.i)|>
  dplyr::rename(median.survival.j.mixture = median.survival.j)|>
  dplyr::select(-"V1", -"X", -"coverage.mixture", -"coverage.llin")

standard.llin.only =  standard.llin.only |>
  dplyr::rename(peak.survival.i.standard = peak.survival.i)|>
  dplyr::rename(peak.survival.j.standard  = peak.survival.j)|>
  dplyr::rename(mean.survival.i.standard  = mean.survival.i)|>
  dplyr::rename(mean.survival.j.standard  = mean.survival.j)|>
  dplyr::rename(median.survival.i.standard  = median.survival.i)|>
  dplyr::rename(median.survival.j.standard  = median.survival.j)|>
  dplyr::select(-"V1", -"X", -"coverage.mixture", -"coverage.llin")

A = dplyr::right_join(micromosaics.hd.mixture.standard, hd.mixture.llin.only,
                      by = c("Heritability",
                             "Male.Insecticide.Exposure", "Female.Insecticide.Exposure", "Female.Fitness.Cost",
                             "Male.Fitness.Cost", "Intervention.Coverage", "Dispersal", "pyr.start.resistance"))

micromosaics.hd.mixture.standard.df = dplyr::right_join(A, standard.llin.only,
                                                        by = c("Heritability",
                                                               "Male.Insecticide.Exposure", "Female.Insecticide.Exposure", "Female.Fitness.Cost",
                                                               "Male.Fitness.Cost", "Intervention.Coverage", "Dispersal", "pyr.start.resistance"))



micromosaics.hd.mixture.standard.df = micromosaics.hd.mixture.standard.df|>
  dplyr::mutate(pyrethroid.start = round(convert_resistance_score_to_bioassay_survival(trait.mean = pyr.start.resistance)*100,
                                         1))

#Negative values will be micro-mosaics win (consistent with other graphs)


micromosaics.hd.mixture.standard.df = micromosaics.hd.mixture.standard.df|>
  dplyr::mutate(end.pyr.mm = peak.survival.i*100)|>
  dplyr::mutate(change.novel.mm = ((peak.survival.j*100) - 0.5))|>
  dplyr::mutate(change.pyr.mm = end.pyr.mm - pyrethroid.start)|>
  #standard llin only
  dplyr::mutate(end.pyr.standard = peak.survival.i.standard*100)|>
  dplyr::mutate(change.novel.standard = ((peak.survival.j.standard*100) - 0.5))|>
  dplyr::mutate(change.pyr.standard = end.pyr.standard - pyrethroid.start)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.standard.pyr = round((change.pyr.mm - change.pyr.standard)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.standard.novel =  round((change.novel.mm - change.novel.standard)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.standard.overall = round((mm.vs.standard.pyr + mm.vs.standard.novel)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.standard.pyr.outcome =  as.factor(ifelse(mm.vs.standard.pyr < 0,
                                                               yes = "micro-mosaics win",
                                                               no = ifelse(mm.vs.standard.pyr > 0,
                                                                           yes = "micro-mosaics lose",
                                                                           no = "draw"))))|>
  dplyr::mutate(mm.vs.standard.pyr.outcome = factor(mm.vs.standard.pyr.outcome,
                                                    levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                                    ordered = TRUE))|>
  dplyr::mutate(mm.vs.standard.novel.outcome = as.factor(ifelse(mm.vs.standard.novel < 0,
                                                                yes = "micro-mosaics win",
                                                                no = ifelse(mm.vs.standard.novel > 0,
                                                                            yes = "micro-mosaics lose",
                                                                            no = "draw"))))|>
  dplyr::mutate(mm.vs.standard.novel.outcome = factor(mm.vs.standard.novel.outcome,
                                                      levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                                      ordered = TRUE))|>
  dplyr::mutate(mm.vs.standard.overall.outcome = as.factor(ifelse(mm.vs.standard.overall < 0,
                                                                  yes = "micro-mosaics win",
                                                                  no = ifelse(mm.vs.standard.overall > 0,
                                                                              yes = "micro-mosaics lose",
                                                                              no = "draw"))))|>
  dplyr::mutate(mm.vs.standard.overall.outcome = factor(mm.vs.standard.overall.outcome,
                                                        levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                                        ordered = TRUE))|>
  #mixutre llin only
  dplyr::mutate(end.pyr.mixture = peak.survival.i.mixture*100)|>
  dplyr::mutate(change.novel.mixture = ((peak.survival.j.mixture*100) - 0.5))|>
  dplyr::mutate(change.pyr.mixture = end.pyr.mixture - pyrethroid.start)|>
  dplyr::mutate(mm.vs.mix.pyr = round((change.pyr.mm - change.pyr.mixture)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.mix.novel = round((change.novel.mm - change.novel.mixture)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.mix.overall = round((mm.vs.mix.pyr + mm.vs.mix.novel)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.mix.pyr.outcome =  as.factor(ifelse(mm.vs.mix.pyr < 0,
                                                          yes = "micro-mosaics wins",
                                                          no = ifelse(mm.vs.mix.pyr > 0,
                                                                      yes = "micro-mosaics lose",
                                                                      no = "draw"))))|>
  dplyr::mutate(mm.vs.mix.pyr.outcome = factor(mm.vs.mix.pyr.outcome,
                                               levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                               ordered = TRUE))|>
  dplyr::mutate(mm.vs.mix.novel.outcome = as.factor(ifelse(mm.vs.mix.novel < 0,
                                                           yes = "micro-mosaics win",
                                                           no = ifelse(mm.vs.standard.novel > 0,
                                                                       yes = "micro-mosaics lose",
                                                                       no = "draw"))))|>
  dplyr::mutate(mm.vs.mix.novel.outcome = factor(mm.vs.mix.novel.outcome,
                                                 levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                                 ordered = TRUE))|>
  dplyr::mutate(mm.vs.mix.overall.outcome = as.factor(ifelse(mm.vs.mix.overall < 0,
                                                             yes = "micro-mosaics win",
                                                             no = ifelse(mm.vs.mix.overall > 0,
                                                                         yes = "micro-mosaics lose",
                                                                         no = "draw"))))|>
  dplyr::mutate(mm.vs.mix.overall.outcome = factor(mm.vs.mix.overall.outcome,
                                                   levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                                   ordered = TRUE))

micromosaics.mixture.standard.df = micromosaics.mixture.standard.df|>
  dplyr::mutate(end.pyr.mm = peak.survival.i*100)|>
  dplyr::mutate(change.novel.mm = ((peak.survival.j*100) - 0.5))|>
  dplyr::mutate(change.pyr.mm = end.pyr.mm - pyrethroid.start)|>
  #standard llin only
  dplyr::mutate(end.pyr.standard = peak.survival.i.standard*100)|>
  dplyr::mutate(change.novel.standard = ((peak.survival.j.standard*100) - 0.5))|>
  dplyr::mutate(change.pyr.standard = end.pyr.standard - pyrethroid.start)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.standard.pyr = round((change.pyr.mm - change.pyr.standard)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.standard.novel =  round((change.novel.mm - change.novel.standard)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.standard.overall = round((mm.vs.standard.pyr + mm.vs.standard.novel)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.standard.pyr.outcome =  as.factor(ifelse(mm.vs.standard.pyr < 0,
                                                               yes = "micro-mosaics win",
                                                               no = ifelse(mm.vs.standard.pyr > 0,
                                                                           yes = "micro-mosaics lose",
                                                                           no = "draw"))))|>
  dplyr::mutate(mm.vs.standard.pyr.outcome = factor(mm.vs.standard.pyr.outcome,
                                                    levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                                    ordered = TRUE))|>
  dplyr::mutate(mm.vs.standard.novel.outcome = as.factor(ifelse(mm.vs.standard.novel < 0,
                                                                yes = "micro-mosaics win",
                                                                no = ifelse(mm.vs.standard.novel > 0,
                                                                            yes = "micro-mosaics lose",
                                                                            no = "draw"))))|>
  dplyr::mutate(mm.vs.standard.novel.outcome = factor(mm.vs.standard.novel.outcome,
                                                      levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                                      ordered = TRUE))|>
  dplyr::mutate(mm.vs.standard.overall.outcome = as.factor(ifelse(mm.vs.standard.overall < 0,
                                                                  yes = "micro-mosaics win",
                                                                  no = ifelse(mm.vs.standard.overall > 0,
                                                                              yes = "micro-mosaics lose",
                                                                              no = "draw"))))|>
  dplyr::mutate(mm.vs.standard.overall.outcome = factor(mm.vs.standard.overall.outcome,
                                                        levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                                        ordered = TRUE))|>
  #mixutre llin only
  dplyr::mutate(end.pyr.mixture = peak.survival.i.mixture*100)|>
  dplyr::mutate(change.novel.mixture = ((peak.survival.j.mixture*100) - 0.5))|>
  dplyr::mutate(change.pyr.mixture = end.pyr.mixture - pyrethroid.start)|>
  dplyr::mutate(mm.vs.mix.pyr = round((change.pyr.mm - change.pyr.mixture)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.mix.novel = round((change.novel.mm - change.novel.mixture)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.mix.overall = round((mm.vs.mix.pyr + mm.vs.mix.novel)/0.5)*0.5)|> #round to nearest 0.5% bioassay survival
  dplyr::mutate(mm.vs.mix.pyr.outcome =  as.factor(ifelse(mm.vs.mix.pyr < 0,
                                                          yes = "micro-mosaics wins",
                                                          no = ifelse(mm.vs.mix.pyr > 0,
                                                                      yes = "micro-mosaics lose",
                                                                      no = "draw"))))|>
  dplyr::mutate(mm.vs.mix.pyr.outcome = factor(mm.vs.mix.pyr.outcome,
                                               levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                               ordered = TRUE))|>
  dplyr::mutate(mm.vs.mix.novel.outcome = as.factor(ifelse(mm.vs.mix.novel < 0,
                                                           yes = "micro-mosaics win",
                                                           no = ifelse(mm.vs.standard.novel > 0,
                                                                       yes = "micro-mosaics lose",
                                                                       no = "draw"))))|>
  dplyr::mutate(mm.vs.mix.novel.outcome = factor(mm.vs.mix.novel.outcome,
                                                 levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                                 ordered = TRUE))|>
  dplyr::mutate(mm.vs.mix.overall.outcome = as.factor(ifelse(mm.vs.mix.overall < 0,
                                                             yes = "micro-mosaics win",
                                                             no = ifelse(mm.vs.mix.overall > 0,
                                                                         yes = "micro-mosaics lose",
                                                                         no = "draw"))))|>
  dplyr::mutate(mm.vs.mix.overall.outcome = factor(mm.vs.mix.overall.outcome,
                                                   levels = c("micro-mosaics win", "micro-mosaics lose", "draw"),
                                                   ordered = TRUE))




temp.df.hd.hd.standard.pyr = data.frame(table(micromosaics.hd.mixture.standard.df$mm.vs.standard.pyr.outcome,
                                              micromosaics.hd.mixture.standard.df$coverage.mixture,
                                              micromosaics.hd.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.pyr = Var1)|>
  dplyr::rename(pyr.freq = Freq)

temp.df.hd.hd.standard.novel = data.frame(table(micromosaics.hd.mixture.standard.df$mm.vs.standard.novel.outcome,
                                                micromosaics.hd.mixture.standard.df$coverage.mixture,
                                                micromosaics.hd.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.novel = Var1)|>
  dplyr::rename(novel.freq = Freq)


temp.df.hd.hd.standard.overall = data.frame(table(micromosaics.hd.mixture.standard.df$mm.vs.standard.overall.outcome,
                                                  micromosaics.hd.mixture.standard.df$coverage.mixture,
                                                  micromosaics.hd.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.overall = Var1)|>
  dplyr::rename(overall.freq = Freq)

temp.df.hd.hd.standard = data.frame(
  coverage = temp.df.hd.hd.standard.overall$Var2,
  initial.pyr.resistance = temp.df.hd.hd.standard.overall$Var3,
  outcome.overall = temp.df.hd.hd.standard.overall$outcome.overall,
  overall.freq = temp.df.hd.hd.standard.overall$overall.freq,
  outcome.novel = temp.df.hd.hd.standard.novel$outcome.novel,
  novel.freq = temp.df.hd.hd.standard.novel$novel.freq,
  outcome.pyr = temp.df.hd.hd.standard.pyr$outcome.pyr,
  pyr.freq = temp.df.hd.hd.standard.pyr$pyr.freq,
  comparator = paste0("Standard\nITNs Only"),
  dose = "half dose")


###

temp.df.hd.hd.mix.pyr = data.frame(table(micromosaics.hd.mixture.standard.df$mm.vs.mix.pyr.outcome,
                                         micromosaics.hd.mixture.standard.df$coverage.mixture,
                                         micromosaics.hd.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.pyr = Var1)|>
  dplyr::rename(pyr.freq = Freq)

temp.df.hd.hd.mix.novel = data.frame(table(micromosaics.hd.mixture.standard.df$mm.vs.mix.novel.outcome,
                                           micromosaics.hd.mixture.standard.df$coverage.mixture,
                                           micromosaics.hd.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.novel = Var1)|>
  dplyr::rename(novel.freq = Freq)


temp.df.hd.hd.mix.overall = data.frame(table(micromosaics.hd.mixture.standard.df$mm.vs.mix.overall.outcome,
                                             micromosaics.hd.mixture.standard.df$coverage.mixture,
                                             micromosaics.hd.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.overall = Var1)|>
  dplyr::rename(overall.freq = Freq)

temp.df.hd.hd.mix = data.frame(
  coverage = temp.df.hd.hd.mix.overall$Var2,
  initial.pyr.resistance = temp.df.hd.hd.mix.overall$Var3,
  outcome.overall = temp.df.hd.hd.mix.overall$outcome.overall,
  overall.freq = temp.df.hd.hd.mix.overall$overall.freq,
  outcome.novel = temp.df.hd.hd.mix.novel$outcome.novel,
  novel.freq = temp.df.hd.hd.mix.novel$novel.freq,
  outcome.pyr = temp.df.hd.hd.mix.pyr$outcome.pyr,
  pyr.freq = temp.df.hd.hd.mix.pyr$pyr.freq,
  comparator = paste0("Mixture\nITNs Only"),
  dose = "half dose")


###
temp.df.fd.fd.standard.pyr = data.frame(table(micromosaics.mixture.standard.df$mm.vs.standard.pyr.outcome,
                                              micromosaics.mixture.standard.df$coverage.mixture,
                                              micromosaics.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.pyr = Var1)|>
  dplyr::rename(pyr.freq = Freq)

temp.df.fd.fd.standard.novel = data.frame(table(micromosaics.mixture.standard.df$mm.vs.standard.novel.outcome,
                                                micromosaics.mixture.standard.df$coverage.mixture,
                                                micromosaics.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.novel = Var1)|>
  dplyr::rename(novel.freq = Freq)


temp.df.fd.fd.standard.overall = data.frame(table(micromosaics.mixture.standard.df$mm.vs.standard.overall.outcome,
                                                  micromosaics.mixture.standard.df$coverage.mixture,
                                                  micromosaics.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.overall = Var1)|>
  dplyr::rename(overall.freq = Freq)

temp.df.fd.fd.standard = data.frame(
  coverage = temp.df.hd.hd.standard.overall$Var2,
  initial.pyr.resistance = temp.df.hd.hd.standard.overall$Var3,
  outcome.overall = temp.df.hd.hd.standard.overall$outcome.overall,
  overall.freq = temp.df.hd.hd.standard.overall$overall.freq,
  outcome.novel = temp.df.hd.hd.standard.novel$outcome.novel,
  novel.freq = temp.df.hd.hd.standard.novel$novel.freq,
  outcome.pyr = temp.df.hd.hd.standard.pyr$outcome.pyr,
  pyr.freq = temp.df.hd.hd.standard.pyr$pyr.freq,
  comparator = paste0("Standard\nITNs Only"),
  dose = "full dose")


### #Full dose

temp.df.fd.fd.mix.pyr = data.frame(table(micromosaics.mixture.standard.df$mm.vs.mix.pyr.outcome,
                                         micromosaics.mixture.standard.df$coverage.mixture,
                                         micromosaics.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.pyr = Var1)|>
  dplyr::rename(pyr.freq = Freq)

temp.df.fd.fd.mix.novel = data.frame(table(micromosaics.mixture.standard.df$mm.vs.mix.novel.outcome,
                                           micromosaics.mixture.standard.df$coverage.mixture,
                                           micromosaics.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.novel = Var1)|>
  dplyr::rename(novel.freq = Freq)


temp.df.fd.fd.mix.overall = data.frame(table(micromosaics.mixture.standard.df$mm.vs.mix.overall.outcome,
                                             micromosaics.mixture.standard.df$coverage.mixture,
                                             micromosaics.mixture.standard.df$pyrethroid.start))|>
  dplyr::rename(outcome.overall = Var1)|>
  dplyr::rename(overall.freq = Freq)

temp.df.fd.fd.mix = data.frame(
  coverage = temp.df.hd.hd.mix.overall$Var2,
  initial.pyr.resistance = temp.df.hd.hd.mix.overall$Var3,
  outcome.overall = temp.df.hd.hd.mix.overall$outcome.overall,
  overall.freq = temp.df.hd.hd.mix.overall$overall.freq,
  outcome.novel = temp.df.hd.hd.mix.novel$outcome.novel,
  novel.freq = temp.df.hd.hd.mix.novel$novel.freq,
  outcome.pyr = temp.df.hd.hd.mix.pyr$outcome.pyr,
  pyr.freq = temp.df.hd.hd.mix.pyr$pyr.freq,
  comparator = paste0("Mixture\nITNs Only"),
  dose = "full dose")


final.df = rbind(temp.df.fd.fd.mix,
                 temp.df.hd.hd.mix,
                 temp.df.fd.fd.standard,
                 temp.df.hd.hd.standard)

final.df = subset(final.df,
                  coverage %in% c(0.25, 0.5, 0.75))


overall.plot = ggplot(final.df,
                      aes(x=coverage,
                          y=overall.freq,
                          fill = outcome.overall))+
  geom_col(colour = "black")+
  scale_fill_manual(values = c("#4575b4",
                               "#f46d43",
                               "grey"))+
  scale_x_discrete(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  xlab("Proportion coverage which\nis the mixture ITN")+
  ylab("Count")+
  facet_grid2(dose *comparator ~ paste0("Initial\nPyrethroid\nBioassay\nSurvival ", initial.pyr.resistance, "%" ),

              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = c("#88419d", "skyblue", "#88419d", "skyblue",
                                                       "#8dd3c7", "#8dd3c7",
                                                       "#bebada", "#bebada"
                )),
                background_x = elem_list_rect(fill = c(rep("white", 4))),
                by_layer_x = FALSE)

  )+
  ggtitle(paste0("Total"))+
  theme_bw()+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black"))


pyr.plot = ggplot(final.df,
                  aes(x=coverage,
                      y=pyr.freq,
                      fill = outcome.pyr))+
  geom_col(colour = "black")+
  scale_fill_manual(values = c("#4575b4",
                               "#f46d43",
                               "grey"))+
  scale_x_discrete(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  xlab("Proportion coverage which\nis the mixture ITN")+
  ylab("Count")+
  facet_grid2(dose *comparator ~ paste0("Initial\nPyrethroid\nBioassay\nSurvival ", initial.pyr.resistance, "%" ),

              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = c("#88419d", "skyblue", "#88419d", "skyblue",
                                                       "#8dd3c7", "#8dd3c7",
                                                       "#bebada", "#bebada"
                )),
                background_x = elem_list_rect(fill = c(rep("white", 4))),
                by_layer_x = FALSE)

  )+
  ggtitle(paste0("Pyrethroid Insecticide"))+
  theme_bw()+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black"))





novel.plot = ggplot(final.df,
                    aes(x=coverage,
                        y=novel.freq,
                        fill = outcome.novel))+
  geom_col(colour = "black")+
  scale_fill_manual(values = c("#4575b4",
                               "#f46d43",
                               "grey"))+
  scale_x_discrete(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  xlab("Proportion coverage which\nis the mixture ITN")+
  ylab("Count")+
  facet_grid2(dose *comparator ~ paste0("Initial\nPyrethroid\nBioassay\nSurvival ", initial.pyr.resistance, "%" ),

              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = c("#88419d", "skyblue", "#88419d", "skyblue",
                                                       "#8dd3c7", "#8dd3c7",
                                                       "#bebada", "#bebada"
                )),
                background_x = elem_list_rect(fill = c(rep("white", 4))),
                by_layer_x = FALSE)

  )+
  ggtitle(paste0("Novel Insecticide"))+
  theme_bw()+
  theme(legend.position = "none",
        strip.background = element_rect(colour = "black"))



legend.df = data.frame(outcomes = unique(final.df$outcome.pyr),
                       x.vals = seq(1, 3, 1))

legend.plot = ggplot(legend.df, aes(x = x.vals, y = 1, fill = outcomes))+
  geom_tile()+
  scale_x_discrete(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values = c("#4575b4",
                               "#f46d43",
                               "grey"))+
  geom_text(aes(label = outcomes), size = 12)+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

the.layout = "
AAABBBCCC
AAABBBCCC
AAABBBCCC
AAABBBCCC
AAABBBCCC
AAABBBCCC
AAABBBCCC
AAABBBCCC
AAABBBCCC
AAABBBCCC
AAABBBCCC
AAABBBCCC
AAABBBCCC
AAABBBCCC
#DDDDDDD#
"

pyr.plot + novel.plot + overall.plot + legend.plot + plot_layout(design = the.layout)




ggsave(plot = last_plot(),
       filename = "mixtures_manuscript_micromosaics_main_plot.jpeg",
       height = 1200,
       width =2400,
       dpi = 600,
       scale = 5,
       units = "px")


#################
#Now look at the magnitudes of wins/losses

plot_comparison_vs_mixtures = function(the.df,
                                         pyr.start.bio,
                                         mixture.type){

    A =  subset(the.df,
                coverage.mixture != 0 & #removes just ITN only if there
                  coverage.mixture != 1 & #removes just mixture only if there
                  pyrethroid.start %in% pyr.start.bio)


    A = A|>
      dplyr::mutate(mm.vs.mix.pyr = ifelse(mm.vs.mix.pyr < -10,
                                           yes = -10,
                                           no = ifelse(mm.vs.mix.pyr > 10,
                                                       yes = 0,
                                                       no = mm.vs.mix.pyr)))|>
      dplyr::mutate(mm.vs.mix.novel = ifelse(mm.vs.mix.novel < -10,
                                           yes = -10,
                                           no = ifelse(mm.vs.mix.novel > 10,
                                                       yes = 0,
                                                       no = mm.vs.mix.novel)))|>
      dplyr::mutate(mm.vs.mix.overall = ifelse(mm.vs.mix.overall < -10,
                                           yes = -10,
                                           no = ifelse(mm.vs.mix.overall > 10,
                                                       yes = 0,
                                                       no = mm.vs.mix.overall)))


    #This is how  micro-mosaic compared to mixture ITN only
    plot.pyr = ggplot(A, aes(
      x=mm.vs.mix.pyr,
      fill = mm.vs.mix.pyr.outcome))+
      geom_histogram(binwidth = 1)+
      geom_vline(xintercept = 0,
                 linetype = "dashed")+
      scale_fill_manual(values = c("blue", "red", "black"),
                        drop = FALSE)+
      scale_y_continuous(sec.axis = sec_axis(~ . , name = "Proportional Coverage Mixture ITN",
                                             breaks = NULL,
                                             labels = NULL))+
      scale_x_continuous(sec.axis = sec_axis(~ . , name = paste0("Initial Pyrethroid\nBioassay Survival (%)"),
                                             breaks = NULL,
                                             labels = NULL),
                         limits = c(-11, 11))+
      xlab(paste0("Change in Pyrethroid\nBioassay Survival"))+
      facet_grid(coverage.mixture ~ pyrethroid.start)+
      theme_bw()+
      theme(axis.text.x = element_text(size = 6),
            legend.position = "none")
    #Where negative values --> mixture loses
    #Where positive values --> mixtures wins


    plot.novel= ggplot(A, aes( x=mm.vs.mix.novel,
                               fill = mm.vs.mix.novel.outcome))+
      geom_histogram(binwidth = 1)+
      geom_vline(xintercept = 0,
                 linetype = "dashed")+
      scale_fill_manual(values = c("blue", "red", "black"),
                        drop = FALSE)+
      scale_y_continuous(sec.axis = sec_axis(~ . , name = "Proportional Coverage Mixture ITN",
                                             breaks = NULL,
                                             labels = NULL))+
      scale_x_continuous(sec.axis = sec_axis(~ . , name = paste0("Initial Pyrethroid\nBioassay Survival (%)"),
                                             breaks = NULL,
                                             labels = NULL),
                         limits = c(-11, 11))+
      xlab(paste0("Change in Novel\nBioassay Survival"))+
      facet_grid(coverage.mixture ~ pyrethroid.start)+
      theme_bw()+
      theme(axis.text.x = element_text(size = 6),
            legend.position = "none")
    #Where negative values --> mixture loses
    #Where positive values --> mixtures wins

    plot.overall = ggplot(A, aes( x=mm.vs.mix.overall,
                                  fill = mm.vs.mix.overall.outcome))+
      geom_histogram(binwidth = 1)+
      geom_vline(xintercept = 0,
                 linetype = "dashed")+
      scale_fill_manual(values = c("blue", "red", "black"),
                        drop = FALSE)+
      scale_y_continuous(sec.axis = sec_axis(~ . , name = "Proportional Coverage Mixture ITN",
                                             breaks = NULL,
                                             labels = NULL))+
      scale_x_continuous(sec.axis = sec_axis(~ . , name = paste0("Initial Pyrethroid\nBioassay Survival (%)"),
                                             breaks = NULL,
                                             labels = NULL),
                         limits = c(-11, 11))+
      xlab(paste0("Total Change in\nBioassay Survival"))+
      facet_grid(coverage.mixture ~ pyrethroid.start)+
      theme_bw()+
      theme(axis.text.x = element_text(size = 6),
            legend.position = "none")
    #Where negative values --> mixture loses
    #Where positive values --> mixtures wins

    the.plot = plot.pyr +
      plot.novel +
      plot.overall +
      plot_annotation(title =
                        paste0("Comparing Standard-Mixture Micro-Mosaics vs Full Coverage Mixtures:",  mixture.type)
      )


    return(the.plot)

  }


plot_comparison_vs_standard = function(the.df,
                                       pyr.start.bio,
                                       mixture.type){


  A =  subset(the.df,
              coverage.mixture != 0 & #removes just ITN only
                coverage.mixture != 1 & #removes just mixture only
                pyrethroid.start %in% pyr.start.bio)



  A = A|>
    dplyr::mutate(mm.vs.standard.pyr = ifelse(mm.vs.standard.pyr < -10,
                                         yes = -10,
                                         no = ifelse(mm.vs.standard.pyr > 10,
                                                     yes = 0,
                                                     no = mm.vs.standard.pyr)))|>
    dplyr::mutate(mm.vs.standard.novel = ifelse(mm.vs.standard.novel < -10,
                                           yes = -10,
                                           no = ifelse(mm.vs.standard.novel > 10,
                                                       yes = 0,
                                                       no = mm.vs.standard.novel)))|>
    dplyr::mutate(mm.vs.standard.overall = ifelse(mm.vs.standard.overall < -10,
                                             yes = -10,
                                             no = ifelse(mm.vs.standard.overall > 10,
                                                         yes = 0,
                                                         no = mm.vs.standard.overall)))






  #This is how the micro-mosaic performed against standard ITN only:
  plot.pyr = ggplot(A, aes(
    x=mm.vs.standard.pyr,
    fill = mm.vs.standard.pyr.outcome))+
    geom_histogram(binwidth = 1)+
    geom_vline(xintercept = 0,
               linetype = "dashed")+
    scale_fill_manual(values = c("blue", "red", "black"),
                      drop = FALSE)+
    xlab(paste0(" Change in Pyrethroid\nBioassay Survival"))+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Proportional Coverage Mixture ITNs",
                                           breaks = NULL,
                                           labels = NULL))+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = paste0("Initial Pyrethroid\nBioassay Survival (%)"),
                                           breaks = NULL,
                                           labels = NULL),
                       limits = c(-11, 11))+
    facet_grid(coverage.mixture ~ pyrethroid.start)+
    theme_bw()+
    theme(axis.text.x = element_text(size = 6),
          legend.position = "none")
  #Where negative values --> more resistance in ITN only
  #Where positive values --> more resistance in micro-mosaic


  plot.novel = ggplot(A, aes(
    x = mm.vs.standard.novel,
       fill = mm.vs.standard.novel.outcome))+
    geom_histogram(binwidth = 1)+
    geom_vline(xintercept = 0,
               linetype = "dashed")+
      scale_fill_manual(values = c("blue", "red", "black"),
                        drop = FALSE)+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Proportional Coverage Mixture ITNs",
                                           breaks = NULL,
                                           labels = NULL))+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = paste0("Initial Pyrethroid\nBioassay Survival (%)"),
                                           breaks = NULL,
                                           labels = NULL),
                       limits = c(-11, 11))+
    xlab(paste0("Change in Novel\nBioassay Survival"))+
    facet_grid(coverage.mixture ~ pyrethroid.start)+
    theme_bw()+
    theme(axis.text.x = element_text(size = 6),
          legend.position = "none")
  #Where negative values --> more resistance in ITN only
  #Where positive values --> more resistance in micro-mosaic

  plot.overall = ggplot(A, aes(
    x= mm.vs.standard.overall,
    fill = mm.vs.standard.overall.outcome))+
    geom_histogram(binwidth = 1)+
    geom_vline(xintercept = 0,
               linetype = "dashed")+
    scale_fill_manual(values = c("blue", "red", "black"),
                      drop = FALSE)+
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Proportional Coverage Mixture ITNs",
                                           breaks = NULL,
                                           labels = NULL))+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = paste0("Initial Pyrethroid\nBioassay Survival (%)"),
                                           breaks = NULL,
                                           labels = NULL),
                       limits = c(-11, 11))+
  xlab(paste0("Total Change in \nBioassay Survival"))+
    facet_grid(coverage.mixture ~ pyrethroid.start)+
    theme_bw()+
    theme(axis.text.x = element_text(size = 6),
          legend.position = "none")
  #Where negative values --> more resistance in ITN only
  #Where positive values --> more resistance in micro-mosaic

  the.plot = plot.pyr +
    plot.novel +
    plot.overall +
    plot_annotation(title =
                      paste0("Standard-Mixture Micro-Mosaics vs Standard ITNs only:", mixture.type)
    )



  return(the.plot)
}



plot_comparison_vs_mixtures(
  the.df = micromosaics.mixture.standard.df,
  pyr.start.bio = c(0.5, 10, 20, 50, 80),
  mixture.type = "Full Dose")

ggsave(
  filename = "chapter6_figure11a.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)

plot_comparison_vs_standard(
  the.df = micromosaics.mixture.standard.df,
  pyr.start.bio = c(0.5, 10, 20, 50, 80),
  mixture.type = "Full Dose")

ggsave(
  filename = "chapter6_figure11b.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)

plot_comparison_vs_mixtures(
  the.df = micromosaics.hd.mixture.standard.df,
  pyr.start.bio = c(0.5, 10, 20, 50, 80),
  mixture.type = "Half Dose")

ggsave(
  filename = "chapter6_figure12a.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)

plot_comparison_vs_standard(
  the.df = micromosaics.hd.mixture.standard.df,
  pyr.start.bio = c(0.5, 10, 20, 50, 80),
  mixture.type = "Half Dose")



ggsave(
  filename = "chapter6_figure12b.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)


###Sensitivity Analysis::::::

ggplot(subset(micromosaics.hd.mixture.standard.df,
              coverage.mixture %in% c(0.25, 0.5, 0.75)), aes(x=Heritability,
                                                y = mm.vs.standard.overall))+
  geom_smooth(method = "gam", colour = "darkgreen",
              fill= "palegreen")+
  geom_smooth(aes(x=Heritability,
                      y = mm.vs.mix.overall),
              method = "gam",
                  fill = "purple", colour = "purple1")+
  geom_hline(yintercept = 0, colour = "black",
             linetype = "dashed")+
  facet_grid(coverage.mixture ~ pyrethroid.start)+
    theme_bw()




sensitivity_analysis_gam = function(i){

  parameter.names = c("Heritability",
                      "Male Insecticide Exposure",
                      "Female Insecticide Exposure",
                      "Intervention Coverage",
                      "Dispersal")

  name.cols = colnames(micromosaics.hd.mixture.standard.df[, c(12, 13, 14, 17, 18)])


  micromosaics.bio.temp = micromosaics.hd.mixture.standard.df|>
    dplyr::select("coverage.mixture", "pyrethroid.start",
                  "mm.vs.standard.overall", "mm.vs.mix.overall",
                  name.cols[i])

  micromosaics.bio.temp$x.parameter = micromosaics.bio.temp[, 5]


  the.plot = ggplot(subset(micromosaics.bio.temp,
                coverage.mixture %in% c(0.25, 0.5, 0.75)), aes(x=x.parameter,
                                                               y = mm.vs.standard.overall))+
    geom_smooth(method = "gam", colour = "darkgreen",
                fill= "palegreen")+
    geom_smooth(aes(x=x.parameter,
                    y = mm.vs.mix.overall),
                method = "gam",
                fill = "purple", colour = "purple1")+
    geom_hline(yintercept = 0, colour = "black",
               linetype = "dashed")+
    ylab("Difference in Total Bioassay Change")+
    xlab(paste0(name.cols[i]))+
    facet_grid(coverage.mixture ~ pyrethroid.start)+
    theme_bw()+
    theme(axis.text = element_text(size = 14,
                                   angle = 90),
          axis.title = element_text(size = 15),
          strip.text = element_text(size = 15))

  return(the.plot)
}


plot.list = list()
for(j in 1:5){

  plot.list[[j]] = sensitivity_analysis_gam(i=j)

}

the.layout = "
AB
CD
E#
"

plot.list[[1]]+
  plot.list[[2]]+
  plot.list[[3]]+
  plot.list[[4]]+
  plot.list[[5]]+
  plot_layout(design = the.layout)



sensitivity_analysis_gam_fd = function(i){

  parameter.names = c("Heritability",
                      "Male Insecticide Exposure",
                      "Female Insecticide Exposure",
                      "Intervention Coverage",
                      "Dispersal")

  name.cols = colnames(micromosaics.mixture.standard.df[, c(11, 12, 13, 16 ,17)])


  micromosaics.bio.temp = micromosaics.mixture.standard.df|>
    dplyr::select("coverage.mixture", "pyrethroid.start",
                  "mm.vs.standard.overall", "mm.vs.mix.overall",
                  name.cols[i])

  micromosaics.bio.temp$x.parameter = micromosaics.bio.temp[, 5]


  the.plot = ggplot(subset(micromosaics.bio.temp,
                           coverage.mixture %in% c(0.25, 0.5, 0.75)), aes(x=x.parameter,
                                                                          y = mm.vs.standard.overall))+
    geom_smooth(method = "gam", colour = "darkgreen",
                fill= "palegreen")+
    geom_smooth(aes(x=x.parameter,
                    y = mm.vs.mix.overall),
                method = "gam",
                fill = "purple", colour = "purple1")+
    geom_hline(yintercept = 0, colour = "black",
               linetype = "dashed")+
    xlab(paste0(name.cols[i]))+
    facet_grid(coverage.mixture ~ pyrethroid.start)+
    theme_bw()+
    theme(axis.title.y = element_blank())

  return(the.plot)
}


plot.list = list()
for(j in 1:5){

  plot.list[[j]] = sensitivity_analysis_gam_fd(i=j)

}

#legend for plot
legend.df = data.frame(y.val = c(1, 2),
                       Comparison = c("Mixture ITN",
                                      "Standard ITN"))


the.legend = cowplot::get_legend(ggplot(legend.df, aes(y = y.val,
                                                       x=Comparison,
                                                       fill = Comparison))+
                                   geom_col()+
                                   scale_fill_manual(values = c("purple", "darkgreen")))

the.layout = "
AB
CD
EF
"

plot.list[[1]]+
  plot.list[[2]]+
  plot.list[[3]]+
  plot.list[[4]]+
  plot.list[[5]]+
  the.legend +
  plot_layout(design = the.layout)



#Regresssion Tree MODELS

#Need 4 models:
  #1. fd_mm --> mix
  #2 fd_mm --> standard
  #3 hd_mm --> mix
  #4 hd_mm --> standard

library(rpart)
library(rpart.plot)
#get as separate Dataframes containing expoloratory variables and overall outcomes


upperbenchmark.hd = subset(micromosaics.hd.mixture.standard.df,
                           coverage.mixture %in% c(0.25, 0.5, 0.75))

lowerbenchmark.hd = subset(micromosaics.hd.mixture.standard.df,
                           coverage.mixture %in% c(0.25, 0.5, 0.75))

upperbenchmark.fd = subset(micromosaics.mixture.standard.df,
                                   coverage.mixture %in% c(0.25, 0.5, 0.75))

lowerbenchmark.fd= subset(micromosaics.mixture.standard.df,
                                  coverage.mixture %in% c(0.25, 0.5, 0.75))

upperbenchmark.hd = upperbenchmark.hd|>
  dplyr::select( "Heritability", "Male.Insecticide.Exposure",
                 "Female.Insecticide.Exposure", "Intervention.Coverage",
                 "Dispersal", "pyr.start.resistance", "coverage.mixture", "mm.vs.mix.overall.outcome")|>
  dplyr::rename("outcome" = mm.vs.mix.overall.outcome)|>
  dplyr::mutate(pyr.resistance = as.factor(pyr.start.resistance))|>
  dplyr::mutate(pyr.resistance = plyr::mapvalues(pyr.start.resistance, from = c("4.5", "100",
                                                                                "225", "900", "3600"),
                                                 to = c("none", "low",
                                                        "moderate", "high", "substantial")))|>
  dplyr::mutate(pyr.resistance = factor(pyr.resistance, levels = c("none",
                                                                   "low",
                                                                   "moderate",
                                                                   "high",
                                                                   "substantial"
  )))|>
  dplyr::mutate(mixture.coverage = as.factor(coverage.mixture))|>
  dplyr::mutate(mixture.coverage = plyr::mapvalues(mixture.coverage, from = c("0.25", "0.5",
                                                                              "0.75"),
                                                   to = c("low", "moderate", "high")))|>
  dplyr::mutate(mixture.coverage = factor(mixture.coverage, levels = c("low",
                                                                       "moderate",
                                                                       "high" )))|>
  dplyr::select(-"coverage.mixture", -"pyr.start.resistance")|>
  dplyr::mutate(outcome = plyr::mapvalues(outcome, from = c("micro-mosaics win", "micro-mosaics lose",
                                                                                "draw"),
                                                 to = c(paste0("micro-mosaics\nwin"), paste0("micro-mosaics\nlose"),
                                                        "draw")))

upperbenchmark.fd =  upperbenchmark.fd|>
  dplyr::select( "Heritability", "Male.Insecticide.Exposure",
                                    "Female.Insecticide.Exposure", "Intervention.Coverage",
                                    "Dispersal", "pyr.start.resistance", "coverage.mixture", "mm.vs.mix.overall.outcome")|>
  dplyr::rename("outcome" = mm.vs.mix.overall.outcome)|>
  dplyr::mutate(pyr.resistance = as.factor(pyr.start.resistance))|>
  dplyr::mutate(pyr.resistance = plyr::mapvalues(pyr.start.resistance, from = c("4.5", "100",
                                                                                "225", "900", "3600"),
                                                 to = c("none", "low",
                                                        "moderate", "high", "substantial")))|>
  dplyr::mutate(pyr.resistance = factor(pyr.resistance, levels = c("none",
                                                                   "low",
                                                                   "moderate",
                                                                   "high",
                                                                   "substantial"
  )))|>
  dplyr::mutate(mixture.coverage = as.factor(coverage.mixture))|>
  dplyr::mutate(mixture.coverage = plyr::mapvalues(mixture.coverage, from = c("0.25", "0.5",
                                                                              "0.75"),
                                                   to = c("low", "moderate", "high")))|>
  dplyr::mutate(mixture.coverage = factor(mixture.coverage, levels = c("low",
                                                                       "moderate",
                                                                       "high" )))|>
  dplyr::select(-"coverage.mixture", -"pyr.start.resistance")|>
  dplyr::mutate(outcome = plyr::mapvalues(outcome, from = c("micro-mosaics win", "micro-mosaics lose",
                                                            "draw"),
                                          to = c(paste0("micro-mosaics\nwin"), paste0("micro-mosaics\nlose"),
                                                 "draw")))

lowerbenchmark.fd = lowerbenchmark.fd|>
dplyr::select( "Heritability", "Male.Insecticide.Exposure",
               "Female.Insecticide.Exposure", "Intervention.Coverage",
               "Dispersal", "pyr.start.resistance", "coverage.mixture", "mm.vs.standard.overall.outcome")|>
  dplyr::rename("outcome" = mm.vs.standard.overall.outcome)|>
  dplyr::mutate(pyr.resistance = as.factor(pyr.start.resistance))|>
  dplyr::mutate(pyr.resistance = plyr::mapvalues(pyr.start.resistance, from = c("4.5", "100",
                                                                                "225", "900", "3600"),
                                                 to = c("none", "low",
                                                        "moderate", "high", "substantial")))|>
  dplyr::mutate(pyr.resistance = factor(pyr.resistance, levels = c("none",
                                                                   "low",
                                                                   "moderate",
                                                                   "high",
                                                                   "substantial"
  )))|>
  dplyr::mutate(mixture.coverage = as.factor(coverage.mixture))|>
  dplyr::mutate(mixture.coverage = plyr::mapvalues(mixture.coverage, from = c("0.25", "0.5",
                                                                              "0.75"),
                                                   to = c("low", "moderate", "high")))|>
  dplyr::mutate(mixture.coverage = factor(mixture.coverage, levels = c("low",
                                                                       "moderate",
                                                                       "high" )))|>
  dplyr::select(-"coverage.mixture", -"pyr.start.resistance")|>
  dplyr::mutate(outcome = plyr::mapvalues(outcome, from = c("micro-mosaics win", "micro-mosaics lose",
                                                            "draw"),
                                          to = c(paste0("micro-mosaics\nwin"), paste0("micro-mosaics\nlose"),
                                                 "draw")))

lowerbenchmark.hd = lowerbenchmark.hd|>
  dplyr::select( "Heritability", "Male.Insecticide.Exposure",
                 "Female.Insecticide.Exposure", "Intervention.Coverage",
                 "Dispersal", "pyr.start.resistance", "coverage.mixture", "mm.vs.standard.overall.outcome")|>
  dplyr::rename("outcome" = mm.vs.standard.overall.outcome)|>
  dplyr::mutate(pyr.resistance = as.factor(pyr.start.resistance))|>
  dplyr::mutate(pyr.resistance = plyr::mapvalues(pyr.start.resistance, from = c("4.5", "100",
                                                                                "225", "900", "3600"),
                                                 to = c("none", "low",
                                                        "moderate", "high", "substantial")))|>
  dplyr::mutate(pyr.resistance = factor(pyr.resistance, levels = c("none",
                                                                   "low",
                                                                   "moderate",
                                                                   "high",
                                                                   "substantial"
                                                                   )))|>
  dplyr::mutate(mixture.coverage = as.factor(coverage.mixture))|>
  dplyr::mutate(mixture.coverage = plyr::mapvalues(mixture.coverage, from = c("0.25", "0.5",
                                                                                "0.75"),
                                                 to = c("low", "moderate", "high")))|>
  dplyr::mutate(mixture.coverage = factor(mixture.coverage, levels = c("low",
                                                                   "moderate",
                                                                   "high" )))|>
  dplyr::select(-"coverage.mixture", -"pyr.start.resistance")|>
  dplyr::mutate(outcome = plyr::mapvalues(outcome, from = c("micro-mosaics win", "micro-mosaics lose",
                                                            "draw"),
                                          to = c(paste0("micro-mosaics\nwin"), paste0("micro-mosaics\nlose"),
                                                 "draw")))

regression_tree = function(the.df,
                           the.seed){


#convert coverages and resistances to factors:::


  set.seed(the.seed)

## 70% of the sample size
sample.size = floor(0.7 * nrow(the.df))

the.df = the.df |>
  dplyr::mutate(id = 1:nrow(the.df))




train.data = data.frame(the.df |> dplyr::sample_frac(0.70))
test.data  = data.frame(dplyr::anti_join(the.df, train.data, by = 'id'))

the.model = rpart(formula = outcome ~  Heritability +
                  Male.Insecticide.Exposure +
                  Female.Insecticide.Exposure+
                  Intervention.Coverage +
                  Dispersal +
                  pyr.resistance +
                  mixture.coverage,
                 control = rpart.control(minbucket = 50,
                                                maxdepth = 5),
                  data = train.data,
                 model = TRUE,
                 method = "class")

prediction = predict(the.model, test.data,
                     type = "class")

model.accuracy = sum(ifelse(as.character(prediction) == as.character(test.data$outcome),
                            yes = 1,
                            no = 0))/(75000 - sample.size)*100

return(list(the.model, model.accuracy))

}


lower.fd = regression_tree(the.df = lowerbenchmark.fd,
                the.seed = 1445)

lower.hd = regression_tree(the.df = lowerbenchmark.hd,
                         the.seed = 1445)

upper.fd = regression_tree(the.df = upperbenchmark.fd,
                         the.seed = 1445)

upper.hd = regression_tree(the.df = upperbenchmark.hd,
                         the.seed = 1445)





  rpart.plot(lower.hd[[1]],
           box.palette = list("skyblue", "coral", "grey"),
           extra = 2,
           under = TRUE,
          main = (paste0("Lower Benchmark\n(half-dose mixtures)")),
          tweak = 1)

lower.hd[[2]]

jpeg("Chapter6_Figure16a.jpeg", width = 20000, height = 10000, res = 500,
     pointsize = 100)
rpart.plot(lower.hd[[1]],
           box.palette = list("skyblue", "coral", "grey"),
           extra = 2,
           under = TRUE,
           main = (paste0("Lower Benchmark\n(half-dose mixtures)")),
           tweak = 1.3)
dev.off()


  rpart.plot(lower.fd[[1]],
           box.palette = list("skyblue", "coral", "grey"),
           extra = 2,
           under = TRUE,
           main = (paste0("Lower Benchmark\n(full-dose mixtures)")),
           tweak = 1.25)

  lower.fd[[2]]




  rpart.plot(upper.hd[[1]],
             box.palette = list("skyblue", "coral", "grey"),
             extra = 2,
             under = TRUE,
             main = (paste0("Upper Benchmark\n(half-dose mixtures)")),
             tweak = 1)

  upper.hd[[2]]



  rpart.plot(upper.fd[[1]],
             box.palette = list("skyblue", "coral", "grey"),
             extra = 2,
             under = TRUE,
             main = (paste0("Upper Benchmark\n(full-dose mixtures)")),
             tweak = 1)

  upper.fd[[2]]
