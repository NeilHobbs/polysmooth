library(ggplot2)
library(data.table)
library(randomForest)
library(patchwork)
####################################################
# Insecticides given unique properties #############
####################################################
micromosaics.bio = fread("micromosaics_operational_biological_realism_micromosaics_simulations.csv")
rotations.bio = fread("micromosaics_operational_biological_realism_rotations_simulations.csv")
hd.hd.mixtures.bio = fread("micromosaics_operational_biological_realism_mixtures_HDHD_simulations.csv")
fd.fd.mixtures.bio = fread("micromosaics_operational_biological_realism_mixtures_FDFD_simulations.csv")

micromosaics.bio$mmvsrot = micromosaics.bio$sim.duration - rotations.bio$sim.duration
micromosaics.bio$mmvshd.hd = micromosaics.bio$sim.duration - hd.hd.mixtures.bio$sim.duration
micromosaics.bio$mmvsfd.fd = micromosaics.bio$sim.duration - fd.fd.mixtures.bio$sim.duration


micromosaics.bio$mmvsrot.outcome = ifelse(micromosaics.bio$mmvsrot > 0,
                                          yes = "micro-mosaics win",
                                          no = ifelse(micromosaics.bio$mmvsrot < 0,
                                                      yes = "micro-mosaics lose",
                                                      no = "draw"))

micromosaics.bio$mmvshd.hd.outcome = ifelse(micromosaics.bio$mmvshd.hd > 0,
                                            yes = "micro-mosaics win",
                                            no = ifelse(micromosaics.bio$mmvshd.hd < 0,
                                                        yes = "micro-mosaics lose",
                                                        no = "draw"))

micromosaics.bio$mmvsfd.fd.outcome = ifelse(micromosaics.bio$mmvsfd.fd > 0,
                                            yes = "micro-mosaics win",
                                            no = ifelse(micromosaics.bio$mmvsfd.fd < 0,
                                                        yes = "micro-mosaics lose",
                                                        no = "draw"))


temp.label.df = data.frame(table(micromosaics.bio$mmvsrot.outcome))

bio.plot.rot = ggplot(subset(micromosaics.bio, mmvsrot.outcome != "draw"), aes(x=-mmvsrot/10,
                                                                               fill = mmvsrot.outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("darkred", "darkblue"),
                    name = "Outcome")+
  geom_label(data = subset(temp.label.df, Var1 == "draw"), mapping = aes(x= -3, y = 4200, label = Freq),
             fill = "grey", inherit.aes = FALSE, alpha = 0.4)+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics win"), mapping = aes(x= -25, y = 1000, label = Freq),
             fill = "darkblue", inherit.aes = FALSE, alpha = 0.4)+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics lose"), mapping = aes(x= 15, y = 1000, label = Freq),
             fill = "darkred", inherit.aes = FALSE, alpha = 0.4)+
  xlab("Difference in Strategy Lifespan (years)")+
  ylab("Frequency")+
  xlim(-36, 16)+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")


temp.label.df = data.frame(table(micromosaics.bio$mmvshd.hd.outcome))

bio.plot.hd.hd = ggplot(subset(micromosaics.bio,
                               mmvshd.hd.outcome != "draw"), aes(x=-mmvshd.hd/10,
                                                                 fill = mmvshd.hd.outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("darkred", "darkblue"),
                    name = "Outcome")+
  geom_label(data = subset(temp.label.df, Var1 == "draw"), mapping = aes(x= -2, y = 4000, label = Freq),
             fill = "grey", inherit.aes = FALSE, alpha = 0.4)+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics win"), mapping = aes(x= -15, y = 1000, label = Freq),
             fill = "darkblue", inherit.aes = FALSE, alpha = 0.4)+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics lose"), mapping = aes(x= 25, y = 1000, label = Freq),
             fill = "darkred", inherit.aes = FALSE, alpha = 0.4)+
  xlab("Difference in Strategy Lifespan (years)")+
  ylab("Frequency")+
  xlim(-22, 36)+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")



temp.label.df = data.frame(table(micromosaics.bio$mmvsfd.fd.outcome))

bio.plot.fd.fd= ggplot(subset(micromosaics.bio,
                              mmvsfd.fd.outcome != "draw"), aes(x=-mmvsfd.fd/10,
                                                                fill = mmvsfd.fd.outcome))+
  geom_histogram(binwidth = 1)+
  scale_fill_manual(values = c("darkred", "darkblue"),
                    name = "Outcome")+
  geom_label(data = subset(temp.label.df, Var1 == "draw"), mapping = aes(x= -2, y = 1200, label = Freq),
             fill = "grey", inherit.aes = FALSE, alpha = 0.4)+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics win"), mapping = aes(x= -10, y = 1000, label = Freq),
             fill = "darkblue", inherit.aes = FALSE, alpha = 0.4)+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics lose"), mapping = aes(x= 12, y = 1000, label = Freq),
             fill = "darkred", inherit.aes = FALSE, alpha = 0.4)+
  xlab("Difference in Strategy Lifespan (years)")+
  ylab("Frequency")+
  xlim(-13, 46)+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")


micromosaics.bio$mmvsrot.diff.end.bioassay = (micromosaics.bio$peak.survival.i + micromosaics.bio$peak.survival.j) - (rotations.bio$peak.survival.i + rotations.bio$peak.survival.j)

micromosaics.bio$mmvsrot.diff.end.bioassay.outcome = ifelse(micromosaics.bio$mmvsrot.diff.end.bioassay == 0,
                                                            yes = "draw",
                                                            no = ifelse(micromosaics.bio$mmvsrot.diff.end.bioassay > 0,
                                                                        yes = "micro-mosaics lose",
                                                                        no = "micro-mosaics win"))


micromosaics.bio$mmvshdhdmix.diff.end.bioassay = (micromosaics.bio$peak.survival.i + micromosaics.bio$peak.survival.j) - (hd.hd.mixtures.bio$peak.survival.i + hd.hd.mixtures.bio$peak.survival.j)

micromosaics.bio$mmvshdhdmix.diff.end.bioassay.outcome = ifelse(micromosaics.bio$mmvshdhdmix.diff.end.bioassay == 0,
                                                                yes = "draw",
                                                                no = ifelse(micromosaics.bio$mmvshdhdmix.diff.end.bioassay > 0,
                                                                            yes = "micro-mosaics lose",
                                                                            no = "micro-mosaics win"))


micromosaics.bio$mmvsfdfdmix.diff.end.bioassay = (micromosaics.bio$peak.survival.i + micromosaics.bio$peak.survival.j) - (fd.fd.mixtures.bio$peak.survival.i + fd.fd.mixtures.bio$peak.survival.j)

micromosaics.bio$mmvsfdfdmix.diff.end.bioassay.outcome = ifelse(micromosaics.bio$mmvsfdfdmix.diff.end.bioassay == 0,
                                                                yes = "draw",
                                                                no = ifelse(micromosaics.bio$mmvsfdfdmix.diff.end.bioassay > 0,
                                                                            yes = "micro-mosaics lose",
                                                                            no = "micro-mosaics win"))


temp.label.df = data.frame(table(subset(micromosaics.bio,
                                        mmvsrot.outcome == "draw")$mmvsrot.diff.end.bioassay.outcome))

bio.plot.sec.rot = ggplot(subset(micromosaics.bio, mmvsrot.outcome == "draw"),
                          aes(x= mmvsrot.diff.end.bioassay*100,
                              fill =mmvsrot.diff.end.bioassay.outcome))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("coral", "skyblue"),
                    name = "Secondary Outcome")+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics win"), mapping = aes(x= -5, y = 100, label = Freq),
             fill = "skyblue", inherit.aes = FALSE, alpha = 0.4)+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics lose"), mapping = aes(x= 3, y = 100, label = Freq),
             fill = "coral", inherit.aes = FALSE, alpha = 0.4)+
  xlab("Difference in Total End Bioassay Survival (%)")+
  ylab("Frequency")+
  xlim(-10, 5)+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")

temp.label.df = data.frame(table(subset(micromosaics.bio,
                                        mmvshd.hd.outcome == "draw")$mmvshdhdmix.diff.end.bioassay.outcome))


bio.plot.sec.hdhd = ggplot(subset(micromosaics.bio, mmvshd.hd.outcome == "draw"),
                           aes(x= mmvshdhdmix.diff.end.bioassay*100,
                               fill =mmvshdhdmix.diff.end.bioassay.outcome))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("coral", "skyblue"),
                    name = "Secondary Outcome")+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics win"), mapping = aes(x= -5, y = 100, label = Freq),
             fill = "skyblue", inherit.aes = FALSE, alpha = 0.4)+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics lose"), mapping = aes(x= 5, y = 100, label = Freq),
             fill = "coral", inherit.aes = FALSE, alpha = 0.4)+
  xlab("Difference in Total End Bioassay Survival (%)")+
  ylab("Frequency")+
  xlim(-9, 12)+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")



temp.label.df = data.frame(table(subset(micromosaics.bio,
                                        mmvsfd.fd.outcome == "draw")$mmvsfdfdmix.diff.end.bioassay.outcome))

bio.plot.sec.fd.fd = ggplot(subset(micromosaics.bio, mmvsfd.fd.outcome == "draw"),
                            aes(x= mmvsfdfdmix.diff.end.bioassay*100,
                                fill =mmvsfdfdmix.diff.end.bioassay.outcome))+
  geom_histogram(binwidth = 0.1)+
  scale_fill_manual(values = c("coral", "skyblue"),
                    name = "Secondary Outcome")+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics win"), mapping = aes(x= -5, y = 100, label = Freq),
             fill = "skyblue", inherit.aes = FALSE, alpha = 0.4)+
  geom_label(data = subset(temp.label.df, Var1 == "micro-mosaics lose"), mapping = aes(x= 14, y = 30, label = Freq),
             fill = "coral", inherit.aes = FALSE, alpha = 0.4)+
  xlab("Difference in Total End Bioassay Survival (%)")+
  ylab("Frequency")+
  xlim(-7, 20)+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")

bio.plot.rot + bio.plot.sec.rot + plot_annotation(title = "Micro-Mosaics versus Rotations")
bio.plot.hd.hd + bio.plot.sec.hdhd + plot_annotation(title = "Micro-Mosaics versus Half-Dose Mixtures")
bio.plot.fd.fd + bio.plot.sec.fd.fd+ plot_annotation(title = "Micro-Mosaics versus Full-Dose Mixtures")



#############################################

durations = c(micromosaics.bio$sim.duration,
              rotations.bio$sim.duration,
              hd.hd.mixtures.bio$sim.duration,
              fd.fd.mixtures.bio$sim.duration)

strategy = rep(c("micro-mosaics", "rotations",
                 "half-dose mixtures", "full-dose mixtures"),
               each = 40000)

bio.parameters = rbind(micromosaics.bio[ , 9:23],
                       micromosaics.bio[ , 9:23],
                       micromosaics.bio[ , 9:23],
                       micromosaics.bio[ , 9:23])



################################
# Sensitivity Analysis #########
################################

micromosaics.bio$rot.duration = rotations.bio$sim.duration
micromosaics.bio$fdfd.mix.duration = fd.fd.mixtures.bio$sim.duration
micromosaics.bio$hdhd.mix.duration = hd.hd.mixtures.bio$sim.duration

bio_sensitivity_analysis_gam = function(i){

  parameter.names = c("Heritability i",
                      "Heritability j",
                      "Starting Polygenic Resistance Score i",
                      "Starting Polygenic Resistance Score j",
                      "Male Fitness Cost i",
                      "Male Fitness Cost j",
                      "Female Fitness Cost i",
                      "Female Fitness Cost j",
                      "Cross Resistance",
                      "Male Insecticide Exposure",
                      "Female Insecticide Exposure",
                      "Dispersal",
                      "Intervention Coverage",
                      "Micro-Mosaic Coverage i",
                      "Micro-Mosaic Coverage j")

  name.cols = colnames(micromosaics.bio)[9:23]

  micromosaics.bio.temp = micromosaics.bio|>
    dplyr::select("sim.duration", "rot.duration","fdfd.mix.duration", "hdhd.mix.duration",
                  name.cols[i])

  micromosaics.bio.temp$x.parameter = micromosaics.bio.temp[, 5]

  temp.plot = ggplot(micromosaics.bio.temp, aes(x= x.parameter,
                                                y = (rot.duration - sim.duration)/10))+
    geom_smooth(method = "gam",
                fill = "#023858", #blue = rotations
                colour = "#0570b0")+
    # geom_smooth(aes(x = x.parameter,
    #                 y = (fdfd.mix.duration - sim.duration)/10),
    #             method = "gam",
    #             fill = "#49006a", #purple = full dose mix
    #             colour = "#ae017e")+
    geom_smooth(aes(x = x.parameter,
                    y = (hdhd.mix.duration - sim.duration)/10),
                method = "gam",
                fill = "#800026", #red = half dose mixtures
                colour = "#e31a1c")+
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "black")+
    ylab("Difference From Micro-Mosaics (generations)")+
    xlab(paste0(parameter.names[i]))+
    theme_bw()+
    theme(axis.title.y =  element_blank(),
          axis.title.x =  element_text(size = 10),
          axis.text.x = element_text(size = 8, colour = "black", angle = 90),
          axis.text.y = element_text(size = 8, colour = "black"))

  return(temp.plot)
}




plot.list = list()
for(j in 1:15){

  plot.list[[j]] = bio_sensitivity_analysis_gam(i=j)

}

###get a legend ::

legend.df = data.frame(Strategy = c("Rotations", "Half-Dose Mixtures"#,
                                    #"Full Dose Mixtures"
))

the.legend = cowplot::get_legend(ggplot(legend.df, aes(fill = Strategy,
                                                       y = c(1, 2)))+
                                   geom_bar()+
                                   scale_fill_manual(values = c(#"purple",
                                     "red",
                                     "blue")))

the.layout = "
ABCD
EFGH
IJKL
MNOP
"

plot.list[[1]]+
  plot.list[[2]]+
  plot.list[[3]]+
  plot.list[[4]]+
  plot.list[[5]]+
  plot.list[[6]]+
  plot.list[[7]]+
  plot.list[[8]]+
  plot.list[[9]]+
  plot.list[[10]]+
  plot.list[[11]]+
  plot.list[[12]]+
  plot.list[[13]]+
  plot.list[[14]]+
  plot.list[[15]]+
  the.legend+
  plot_layout(design = the.layout)


bio_sensitivity_analysis_gam_parameter_mismatch = function(i){

  parameter.names.i = c("Heritability.i",
                        "Initial.Resistance.i",
                        "Male.Fitness.Cost.i",
                        "Female.Fitness.Cost.i",
                        "Coverage.i")

  parameter.names.j = c("Heritability.j",
                        "Initial.Resistance.j",
                        "Male.Fitness.Cost.j",
                        "Female.Fitness.Cost.j",
                        "Coverage.j")

  x.axis.title = c("Heritability Difference",
                   "Initial Resistance Difference",
                   "Male Fitness Cost Difference",
                   "Female Fitness Cost Difference",
                   "Micro-Mosaic Coverage Difference")

  micromosaics.bio.temp = micromosaics.bio|>
    dplyr::select("sim.duration", "rot.duration","fdfd.mix.duration", "hdhd.mix.duration", "Cross.Resistance",
                  parameter.names.i[i], parameter.names.j[i])

  micromosaics.bio.temp$x.parameter.i = micromosaics.bio.temp[, 6]
  micromosaics.bio.temp$x.parameter.j = micromosaics.bio.temp[, 7]


  micromosaics.bio.temp.neg = subset(micromosaics.bio.temp, Cross.Resistance < 0)
  micromosaics.bio.temp.pos = subset(micromosaics.bio.temp, Cross.Resistance > 0)


  temp.plot.neg = ggplot(micromosaics.bio.temp.neg, aes(x= abs(x.parameter.i - x.parameter.j),
                                                        y = (rot.duration - sim.duration)/10))+
    geom_smooth(method = "gam",
                fill = "#023858", #blue = rotations
                colour = "#0570b0")+
    # geom_smooth(aes(x = x.parameter,
    #                 y = (fdfd.mix.duration - sim.duration)/10),
    #             method = "gam",
    #             fill = "#49006a", #purple = full dose mix
    #             colour = "#ae017e")+
    geom_smooth(aes(x = abs(x.parameter.i - x.parameter.j),
                    y = (hdhd.mix.duration - sim.duration)/10),
                method = "gam",
                fill = "#800026", #red = half dose mixtures
                colour = "#e31a1c")+
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "black")+
    ylab("Difference From Micro-Mosaics (years)")+
    xlab(paste0(x.axis.title[i]))+
    theme_bw()+
    theme(axis.title.y =  element_blank(),
          axis.title.x =  element_text(size = 10),
          axis.text.x = element_text(size = 8, colour = "black", angle = 90),
          axis.text.y = element_text(size = 8, colour = "black"))


  temp.plot.pos = ggplot(micromosaics.bio.temp.pos, aes(x= abs(x.parameter.i - x.parameter.j),
                                                        y = (rot.duration - sim.duration)/10))+
    geom_smooth(method = "gam",
                fill = "#023858", #blue = rotations
                colour = "#0570b0")+
    # geom_smooth(aes(x = x.parameter,
    #                 y = (fdfd.mix.duration - sim.duration)/10),
    #             method = "gam",
    #             fill = "#49006a", #purple = full dose mix
    #             colour = "#ae017e")+
    geom_smooth(aes(x = abs(x.parameter.i - x.parameter.j),
                    y = (hdhd.mix.duration - sim.duration)/10),
                method = "gam",
                fill = "#800026", #red = half dose mixtures
                colour = "#e31a1c")+
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "black")+
    ylab("Difference From Micro-Mosaics (years)")+
    xlab(paste0(x.axis.title[i]))+
    theme_bw()+
    theme(axis.title.y =  element_blank(),
          axis.title.x =  element_text(size = 10),
          axis.text.x = element_text(size = 8, colour = "black", angle = 90),
          axis.text.y = element_text(size = 8, colour = "black"))


  return(list(temp.plot.pos, temp.plot.neg))
}




plot.list = list()
for(j in 1:5){

  plot.list[[j]] = bio_sensitivity_analysis_gam_parameter_mismatch(i=j)

}

the.layout = "
ABCDE
FGHIJ
"

plot.list[[1]][[2]]+
  plot.list[[2]][[2]]+
  plot.list[[3]][[2]]+
  plot.list[[4]][[2]]+
  plot.list[[5]][[2]]+
  plot.list[[1]][[1]]+
  plot.list[[2]][[1]]+
  plot.list[[3]][[1]]+
  plot.list[[4]][[1]]+
  plot.list[[5]][[1]]+
  plot_layout(design = the.layout)


micromosaics.bio$overall.outcome.rot =
  ifelse(micromosaics.bio$mmvsrot.outcome == "draw",
         yes = micromosaics.bio$mmvsrot.diff.end.bioassay.outcome,
         no = micromosaics.bio$mmvsrot.outcome)

micromosaics.bio$overall.outcome.hdhd =
  ifelse(micromosaics.bio$mmvshd.hd.outcome == "draw",
         yes = micromosaics.bio$mmvshdhdmix.diff.end.bioassay.outcome,
         no = micromosaics.bio$mmvshd.hd.outcome)

micromosaics.bio$overall.outcome.rot.numeric = ifelse(micromosaics.bio$overall.outcome.rot == "micro-mosaics lose",
                                                      yes = 0,
                                                      no = 1)


micromosaics.bio$overall.outcome.hdhd.numeric = ifelse(micromosaics.bio$overall.outcome.hdhd == "micro-mosaics lose",
                                                       yes = 0,
                                                       no = 1)






ggplot(micromosaics.bio, aes(x = abs(Coverage.i - Coverage.j),
                             y = overall.outcome.rot.numeric))+
  geom_smooth(method = "gam",
              fill = "#023858", #blue = rotations
              colour = "#0570b0")+
  geom_smooth(aes(x = abs(Coverage.i - Coverage.j),
                  y = overall.outcome.hdhd.numeric),
              method = "gam",
              fill = "#800026", #red = half dose mixtures
              colour = "#e31a1c")+
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey")+
  ylab("Micro-Mosaics Win Probability")+
  xlab(paste0("Coverage i and Coverage j Difference"))+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 8, colour = "black", angle = 90),
        axis.text.y = element_text(size = 8, colour = "black"))


#Regression Classification Models::

micromosaics.bio$mmvsrot_final.outcome = ifelse(micromosaics.bio$mmvsrot.outcome == "draw",
                                                yes = micromosaics.bio$mmvsrot.diff.end.bioassay.outcome,
                                                no = micromosaics.bio$mmvsrot.outcome)


micromosaics.bio$mmvshdhd_final.outcome = ifelse(micromosaics.bio$mmvshd.hd.outcome == "draw",
                                                 yes = micromosaics.bio$mmvshdhdmix.diff.end.bioassay.outcome,
                                                 no = micromosaics.bio$mmvshd.hd.outcome)


micromosaics.bio$mmvsfdfd_final.outcome = ifelse(micromosaics.bio$mmvsfd.fd.outcome == "draw",
                                                 yes = micromosaics.bio$mmvsfdfdmix.diff.end.bioassay.outcome,
                                                 no = micromosaics.bio$mmvsfd.fd.outcome)


mmvsrot.df = micromosaics.bio|>
  dplyr::select("Heritability.i",
                "Heritability.j",
                "Initial.Resistance.i",
                "Initial.Resistance.j",
                "Male.Fitness.Cost.i",
                "Male.Fitness.Cost.j",
                "Female.Fitness.Cost.i",
                "Female.Fitness.Cost.j",
                "Cross.Resistance",
                "Male.Exposure",
                "Female.Exposure",
                "Dispersal",
                "Coverage.i",
                "Coverage.j",
                "Intervention.Coverage",
                "mmvsrot_final.outcome")|>
  dplyr::rename(primary.outcome = "mmvsrot_final.outcome")|>
  dplyr::rename(Female.Insecticide.Exposure = "Female.Exposure")|>
  dplyr::rename(Male.Insecticide.Exposure = "Male.Exposure")|>
  dplyr::mutate(Difference.Coverage = abs(Coverage.i - Coverage.j))



mmvshdhd.df = micromosaics.bio|>
  dplyr::select("Heritability.i",
                "Heritability.j",
                "Initial.Resistance.i",
                "Initial.Resistance.j",
                "Male.Fitness.Cost.i",
                "Male.Fitness.Cost.j",
                "Female.Fitness.Cost.i",
                "Female.Fitness.Cost.j",
                "Cross.Resistance",
                "Male.Exposure",
                "Female.Exposure",
                "Dispersal",
                "Coverage.i",
                "Coverage.j",
                "Intervention.Coverage",
                "mmvshdhd_final.outcome")|>
  dplyr::rename(primary.outcome = "mmvshdhd_final.outcome")|>
  dplyr::rename(Female.Insecticide.Exposure = "Female.Exposure")|>
  dplyr::rename(Male.Insecticide.Exposure = "Male.Exposure")|>
  dplyr::mutate(Difference.Coverage = abs(Coverage.i - Coverage.j))


mmvsfdfd.df = micromosaics.bio|>
  dplyr::select("Heritability.i",
                "Heritability.j",
                "Initial.Resistance.i",
                "Initial.Resistance.j",
                "Male.Fitness.Cost.i",
                "Male.Fitness.Cost.j",
                "Female.Fitness.Cost.i",
                "Female.Fitness.Cost.j",
                "Cross.Resistance",
                "Male.Exposure",
                "Female.Exposure",
                "Dispersal",
                "Coverage.i",
                "Coverage.j",
                "Intervention.Coverage",
                "mmvsfdfd_final.outcome")|>
  dplyr::rename(primary.outcome = "mmvsfdfd_final.outcome")|>
  dplyr::rename(Female.Insecticide.Exposure = "Female.Exposure")|>
  dplyr::rename(Male.Insecticide.Exposure = "Male.Exposure")|>
  dplyr::mutate(Difference.Coverage = abs(Coverage.i - Coverage.j))



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

  the.model = rpart(formula = primary.outcome ~
                      Heritability.i +
                      Heritability.j +
                      Initial.Resistance.i +
                      Initial.Resistance.j +
                      Male.Fitness.Cost.i +
                      Male.Fitness.Cost.j+
                      Female.Fitness.Cost.i +
                      Female.Fitness.Cost.j+
                      Difference.Coverage +
                      Cross.Resistance+
                      Male.Insecticide.Exposure +
                      Female.Insecticide.Exposure +
                      Dispersal +
                      Intervention.Coverage,
                    cp = 0,
                    control = rpart.control(minbucket = 50,
                                            maxdepth = 5,
                    ),
                    data = train.data,
                    model = TRUE,
                    method = "class")

  prediction = predict(the.model, test.data,
                       type = "class")

  model.accuracy = sum(ifelse(as.character(prediction) == as.character(test.data$primary.outcome),
                              yes = 1,
                              no = 0))/(nrow(the.df) - sample.size)*100

  return(list(the.model, model.accuracy))

}


tree.rot = regression_tree(the.df = mmvsrot.df,
                    the.seed = 1541)

tree.hdhd = regression_tree(the.df = mmvshdhd.df,
                           the.seed = 1541)


rpart.plot(tree.rot[[1]],
           box.palette = list("skyblue", "coral", "grey"),
           extra = 2,
           under = TRUE,
           tweak = 1,
           main = "Micro-Mosaics vs Rotations")
tree.rot[[2]]

rpart.plot(tree.hdhd[[1]],
           box.palette = list("skyblue", "coral", "grey"),
           extra = 2,
           under = TRUE,
           tweak = 1,
           main = "Micro-Mosaics vs Half-Dose Mixtures")
tree.hdhd[[2]]




