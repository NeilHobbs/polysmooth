#required Packages
library(devtools)
load_all() #polysmooth --> includes ggplot2
library(patchwork)
library(data.table)
library(ggplot2)
####################
# Read in Datasets #
####################

##########################
# Insecticides are Novel #
##########################

rotations = fread("mm_simulations_rotation_comparator.csv")
micromosaics = fread("mm_simulations_micromosaic_comparator.csv")
fd.fd.mixtures = fread("mm_simulations_mixtures_FDFD_comparator.csv")
hd.hd.mixtures = fread("mm_simulations_mixtures_HDHD_comparator.csv")
##################################
# Wins, Loses, Draws, longevity  #
##################################

micromosaics$rot.vs.mm = rotations$sim.duration - micromosaics$sim.duration
micromosaics$mixfdfd.vs.mm = fd.fd.mixtures$sim.duration - micromosaics$sim.duration
micromosaics$mixhdhd.vs.mm = hd.hd.mixtures$sim.duration - micromosaics$sim.duration



micromosaics$rot.vs.mm.outcome = as.factor(ifelse(micromosaics$rot.vs.mm > 0,
                                                  yes = "micromosaics lose",
                                                  no = ifelse(micromosaics$rot.vs.mm < 0,
                                                              yes = "micromosaics win",
                                                              no = "draw")))


micromosaics$mixfdfd.vs.mm.outcome = as.factor(ifelse(micromosaics$mixfdfd.vs.mm > 0,
                                                      yes = "micromosaics lose",
                                                      no = ifelse(micromosaics$mixfdfd.vs.mm < 0,
                                                                  yes = "micromosaics win",
                                                                  no = "draw")))

micromosaics$mixhdhd.vs.mm.outcome = as.factor(ifelse(micromosaics$mixhdhd.vs.mm > 0,
                                                      yes = "micromosaics lose",
                                                      no = ifelse(micromosaics$mixhdhd.vs.mm < 0,
                                                                  yes = "micromosaics win",
                                                                  no = "draw")))




micromosaics$rot.vs.mm.peak.diff = micromosaics$peak.survival - rotations$peak.survival

micromosaics$rot.vs.mm.peak.diff.outcome = ifelse(micromosaics$peak.survival - rotations$peak.survival > 0,
                                                  yes = "micromosaics lose",
                                                  no = ifelse(micromosaics$peak.survival - rotations$peak.survival < 0,
                                                              yes = "micromosaics win",
                                                              no = "draw"))

micromosaics$mixfdfd.peak.diff = micromosaics$peak.survival - fd.fd.mixtures$peak.survival

micromosaics$mixfdfd.peak.diff.outcome = ifelse(micromosaics$peak.survival - fd.fd.mixtures$peak.survival > 0,
                                                yes = "micromosaics lose",
                                                no = ifelse(micromosaics$peak.survival - fd.fd.mixtures$peak.survival < 0,
                                                            yes = "micromosaics win",
                                                            no = "draw"))

micromosaics$mixhdhd.peak.diff = micromosaics$peak.survival - hd.hd.mixtures$peak.survival

micromosaics$mixhdhd.peak.diff.outcome = ifelse(micromosaics$peak.survival - hd.hd.mixtures$peak.survival > 0,
                                                yes = "micromosaics lose",
                                                no = ifelse(micromosaics$peak.survival - hd.hd.mixtures$peak.survival < 0,
                                                            yes = "micromosaics win",
                                                            no = "draw"))

micromosaics$rot.duration = rotations$sim.duration
micromosaics$fdfd.mix.duration = fd.fd.mixtures$sim.duration
micromosaics$hdhd.mix.duration = hd.hd.mixtures$sim.duration



nrow(subset(micromosaics, rot.vs.mm >= 50))
nrow(subset(micromosaics, rot.vs.mm <= -50))
2087/50000*100 #rot win
6680/50000*100 #micromisaic win



nrow(subset(micromosaics, mixhdhd.vs.mm >= 50))
nrow(subset(micromosaics, mixhdhd.vs.mm <= -50))
4827/50000*100 #hdhd win
12/50000*100 #micromosaic win


nrow(subset(micromosaics, mixfdfd.vs.mm >= 50))
nrow(subset(micromosaics, mixfdfd.vs.mm <= -50))
15359/50000*100 #hdhd win
0/50000*100 #micromosaic win




###############################################
# Histograms of Primary and Secondary Outcomes#
###############################################

##Micro-Mosaics vs Rotations
label.df = data.frame(table(micromosaics$cross.resistance,
                            micromosaics$rot.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

plot.rotvmm1 = ggplot(subset(micromosaics,
                             rot.vs.mm.outcome != "draw"), aes(x=rot.vs.mm/10,
                                                               fill = rot.vs.mm.outcome))+
  geom_histogram(binwidth = 1, colour = "black")+
  geom_vline(xintercept = 0, linewidth = 1, colour = "black",
             linetype = "dashed")+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("A) Micro-Mosaics vs Rotations - Primary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL), expand = c(0, 0))+
  scale_x_continuous(limits = c(-30, 45))+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  facet_grid2(cross.resistance ~ .,
              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = rev(c("#b2182b",
                                                           "#ef8a62",
                                                           "#fddbc7",
                                                           "#f7f7f7",
                                                           "#d1e5f0",
                                                           "#67a9cf",
                                                           "#2166ac")),
                                              by_layer_x = FALSE)))+
  guides(fill = guide_legend(title = "Outcome"))+
  geom_label(data = subset(label.df,
                           Var2 == "draw"), mapping = aes(x= 0, y = 600, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4)+
  geom_label(data = subset(label.df, Var2 == "micromosaics win"),
             mapping = aes(x= -12, y = 400, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4)+
  geom_label(data = subset(label.df, Var2 == "micromosaics lose"),
             mapping = aes(x= 12, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


#This finding seems really counter-intuitive: can this be explained?
#1. Negative cross resistance: micromosaics best
#     Benefitting from negative cross resistance continually?

#2. Positive cross resistance: rotations best
#

#3. No cross resistance: micromosaics best

#Question then is what if we run on a resistance scale 10-20%.
#i.e. does the "mixture effect" only come into play later.

#secondary outcome:
rot.mm.draws = subset(micromosaics, rot.vs.mm.outcome == "draw")

draw.label.df = data.frame(table(rot.mm.draws$cross.resistance,
                                 rot.mm.draws$rot.vs.mm.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)


plot.rotvmm2 = ggplot(rot.mm.draws,
                      aes(x=rot.vs.mm.peak.diff*100,
                          fill = rot.vs.mm.peak.diff.outcome))+
  geom_histogram(binwidth = 0.25, colour = "black")+
  geom_vline(xintercept = 0, linewidth = 1, colour = "black",
             linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("B) Micro-Mosaics vs Rotations - Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL),
                     expand = c(0,0))+
  scale_x_continuous(limits = c(-7.5, 11))+
  scale_fill_manual(values = c("coral", "skyblue"))+
  facet_grid2(cross.resistance ~ .,
              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = rev(c("#b2182b",
                                                           "#ef8a62",
                                                           "#fddbc7",
                                                           "#f7f7f7",
                                                           "#d1e5f0",
                                                           "#67a9cf",
                                                           "#2166ac")),
                                              by_layer_x = FALSE)))+  guides(fill = guide_legend(title = "Secondary Outcome"))+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics lose"), mapping = aes(x= 3, y = 300, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics win"), mapping = aes(x= -7, y = 300, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


#Full Dose Mixtures:

label.df.1 = data.frame(table(micromosaics$cross.resistance,
                              micromosaics$mixfdfd.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

plot.fdfdvmm1 = ggplot(subset(micromosaics,
                              mixfdfd.vs.mm.outcome != "draw"), aes(x=mixfdfd.vs.mm/10,
                                                                    fill = mixfdfd.vs.mm.outcome))+
  geom_histogram(binwidth = 1, colour = "black")+
  geom_vline(xintercept = 0, linewidth = 1, colour = "black",
             linetype = "dashed")+
  scale_x_continuous(limits = c(-30, 45))+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("C) Micro-Mosaics vs Full-Dose Mixtures - Primary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  facet_grid2(cross.resistance ~ .,
              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = rev(c("#b2182b",
                                                           "#ef8a62",
                                                           "#fddbc7",
                                                           "#f7f7f7",
                                                           "#d1e5f0",
                                                           "#67a9cf",
                                                           "#2166ac")),
                                              by_layer_x = FALSE)))+  guides(fill = guide_legend(title = "Outcome"))+
  geom_label(data = subset(label.df.1,
                           Var2 == "draw"), mapping = aes(x= 0, y = 120, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4)+
  geom_label(data = subset(label.df.1, Var2 == "micromosaics lose"),
             mapping = aes(x= 37.5, y = 120, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


#secondary outcome:
fdfdmix.mm.draws = subset(micromosaics, mixfdfd.vs.mm.outcome  == "draw")

draw.label.df = data.frame(table(fdfdmix.mm.draws$cross.resistance,
                                 fdfdmix.mm.draws$mixfdfd.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)


plot.fdfdvmm2 = ggplot(fdfdmix.mm.draws,
                       aes(x=mixfdfd.peak.diff*100,
                           fill = mixfdfd.peak.diff.outcome))+
  geom_histogram(binwidth = 0.25, colour = "black")+
  geom_vline(xintercept = 0, linewidth = 1, colour = "black",
             linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("D) Micro-Mosaics vs Full-Dose Mixtures - Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL),
                     expand = c(0,0))+
  scale_x_continuous(limits = c(-7.5, 11))+
  scale_fill_manual(values = c("coral", "skyblue"))+
  facet_grid2(cross.resistance ~ .,
              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = rev(c("#b2182b",
                                                           "#ef8a62",
                                                           "#fddbc7",
                                                           "#f7f7f7",
                                                           "#d1e5f0",
                                                           "#67a9cf",
                                                           "#2166ac")),
                                              by_layer_x = FALSE)))+
  guides(fill = guide_legend(title = "Secondary Outcome"))+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics lose"), mapping = aes(x= 9, y = 150, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics win"), mapping = aes(x= -7, y = 150, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


##Micro-Mosaics vs half dose mixtures
label.df = data.frame(table(micromosaics$cross.resistance,
                            micromosaics$mixhdhd.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

plot.hdhdvmm1 = ggplot(subset(micromosaics,
                              mixhdhd.vs.mm.outcome != "draw"), aes(x=mixhdhd.vs.mm/10,
                                                                    fill = mixhdhd.vs.mm.outcome))+
  geom_histogram(binwidth = 1, colour = "black")+
  geom_vline(xintercept = 0, linewidth = 1, colour = "black",
             linetype = "dashed")+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("E) Micro-Mosaics vs Half-Dose Mixtures Primary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_x_continuous(limits = c(-30, 45))+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  facet_grid2(cross.resistance ~ .,
              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = rev(c("#b2182b",
                                                           "#ef8a62",
                                                           "#fddbc7",
                                                           "#f7f7f7",
                                                           "#d1e5f0",
                                                           "#67a9cf",
                                                           "#2166ac")),
                                              by_layer_x = FALSE)))+
  guides(fill = guide_legend(title = "Outcome"))+
  geom_label(data = subset(label.df,
                           Var2 == "draw"), mapping = aes(x= -2, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4)+
  geom_label(data = subset(label.df, Var2 == "micromosaics win"),
             mapping = aes(x= -15, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4)+
  geom_label(data = subset(label.df, Var2 == "micromosaics lose"),
             mapping = aes(x= 10, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4)+
  ylab("Frequency")+

  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


hdhdmix.mm.draws = subset(micromosaics, mixhdhd.vs.mm.outcome  == "draw")

draw.label.df = data.frame(table(hdhdmix.mm.draws$cross.resistance,
                                 hdhdmix.mm.draws$mixhdhd.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)


plot.hdhdvmm2 = ggplot(hdhdmix.mm.draws,
                       aes(x=mixhdhd.peak.diff*100,
                           fill = mixhdhd.peak.diff.outcome))+
  geom_histogram(binwidth = 0.25, colour = "black")+
  geom_vline(xintercept = 0, linewidth = 1, colour = "black",
             linetype = "dashed")+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("F) Micro-Mosaics vs Half-Dose Mixtures - Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL),
                     expand = c(0,0))+
  scale_x_continuous(limits = c(-7.5, 11))+
  scale_fill_manual(values = c("coral", "skyblue"))+
  facet_grid2(cross.resistance ~ .,
              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = rev(c("#b2182b",
                                                           "#ef8a62",
                                                           "#fddbc7",
                                                           "#f7f7f7",
                                                           "#d1e5f0",
                                                           "#67a9cf",
                                                           "#2166ac")),
                                              by_layer_x = FALSE)))+
  guides(fill = guide_legend(title = "Secondary Outcome"))+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics lose"), mapping = aes(x= 5, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics win"), mapping = aes(x= -2, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")



legend.df = data.frame(x.vals = c(1:4),
                       winner = c("Primary Outcome\nMicro-Mosaics Win", "Primary Outcome\nMicro-Mosaics Lose",
                                  "Secondary Outcome\nMicro-Mosaics Win", "Secondary Outcome\nMicro-Mosaics Lose"))


legend.plot = ggplot(legend.df, aes(x=x.vals, y = 1, fill = winner))+
  geom_tile()+
  geom_text(aes(x=x.vals, y = 1, label = winner), size = 6,
            colour = "white")+
  scale_fill_manual(values = c("darkred", "darkblue", "coral", "skyblue"))+
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")


the.layout = "
AAABBB
AAABBB
AAABBB
AAABBB
AAABBB
CCCDDD
CCCDDD
CCCDDD
CCCDDD
CCCDDD
EEEFFF
EEEFFF
EEEFFF
EEEFFF
EEEFFF
#GGGG#
"
plot.rotvmm1 + plot.rotvmm2 +
  plot.fdfdvmm1 + plot.fdfdvmm2 +
  plot.hdhdvmm1 + plot.hdhdvmm2 + legend.plot+
  plot_layout(design = the.layout)


ggsave(
  filename = "Micromosaics_scenario1.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 600)


# ggsave(
#   filename = "Micromosaics_scenario1A_figure3.jpeg",
#   plot = last_plot(),
#   scale = 5,
#   width = 1800,
#   height = 2000,
#   units = "px",
#   dpi = 600)
########################
# SENSITIVITY ANALYSIS #
########################

colnames(micromosaics)
##GAMs for all Parameters stratified by cross resistance
sensitivity_analysis_gam = function(i){

  parameter.names= c("Heritability",
                     "Male.Insecticide.Exposure",
                     "Female.Insecticide.Exposure",
                     "Female.Fitness.Cost",
                     "Male.Fitness.Cost",
                     "Intervention.Coverage",
                     "Dispersal")

  x.axis.title = c("Heritability",
                   "Male Insecticide Exposure",
                   "Female Insecticide Exposure",
                   "Female Fitness Cost",
                   "Male Fitness Cost",
                   "Intervention Coverage",
                   "Dispersal")

  micromosaics.temp = micromosaics|>
    dplyr::select("sim.duration", "rot.duration","fdfd.mix.duration", "hdhd.mix.duration", "cross.resistance",
                  parameter.names[i])

  micromosaics.temp$x.parameter = micromosaics.temp[, 6]


  micromosaics.temp.neg = subset(micromosaics.temp, cross.resistance < 0)
  micromosaics.temp.zero = subset(micromosaics.temp, cross.resistance == 0)
  micromosaics.temp.pos = subset(micromosaics.temp, cross.resistance > 0)


  temp.plot.neg = ggplot(micromosaics.temp.neg, aes(x= x.parameter,
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
    ylab("Difference From Micro-Mosaics (years)")+
    xlab(paste0(x.axis.title[i]))+
    theme_bw()+
    theme(axis.title.y =  element_blank(),
          axis.title.x =  element_text(size = 10),
          axis.text.x = element_text(size = 8, colour = "black", angle = 90),
          axis.text.y = element_text(size = 8, colour = "black"))


  temp.plot.zero = ggplot(micromosaics.temp.zero, aes(x= x.parameter,
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
    ylab("Difference From Micro-Mosaics (years)")+
    xlab(paste0(x.axis.title[i]))+
    theme_bw()+
    theme(axis.title.y =  element_blank(),
          axis.title.x =  element_text(size = 10),
          axis.text.x = element_text(size = 8, colour = "black", angle = 90),
          axis.text.y = element_text(size = 8, colour = "black"))

  temp.plot.pos = ggplot(micromosaics.temp.pos, aes(x= x.parameter,
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
    ylab("Difference From Micro-Mosaics (years)")+
    xlab(paste0(x.axis.title[i]))+
    theme_bw()+
    theme(axis.title.y =  element_blank(),
          axis.title.x =  element_text(size = 10),
          axis.text.x = element_text(size = 8, colour = "black", angle = 90),
          axis.text.y = element_text(size = 8, colour = "black"))


  return(list(temp.plot.pos, temp.plot.zero, temp.plot.neg))
}

plot.list = list()
for(j in 1:7){

  plot.list[[j]] = sensitivity_analysis_gam(i=j)

}

#negative on top ; zero in the middle ; positive on the bottom to match other graphs

the.layout = "
ABCDEFG
HIJKLMO
PQRSTUV
"

plot.list[[1]][[3]]+
plot.list[[2]][[3]]+
plot.list[[3]][[3]]+
plot.list[[4]][[3]] +
plot.list[[5]][[3]] +
plot.list[[6]][[3]] +
plot.list[[7]][[3]] +
plot.list[[1]][[2]]  +
plot.list[[2]][[2]] +
plot.list[[3]][[2]] +
plot.list[[4]][[2]] +
plot.list[[5]][[2]] +
plot.list[[6]][[2]] +
plot.list[[7]][[2]] +
plot.list[[1]][[1]]  +
plot.list[[2]][[1]] +
plot.list[[3]][[1]] +
plot.list[[4]][[1]] +
plot.list[[5]][[1]] +
plot.list[[6]][[1]] +
plot.list[[7]][[1]] +
plot_layout(design = the.layout)


ggsave(
  filename = "chapter6_figure4.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)


senstivity_analysis_gam = function(param, x_label,
                                   mm.df, the.method){

  the.plot = ggplot(mm.df, aes(x=param,
                               y=sim.duration/10))+
    geom_smooth(method = the.method, colour = "#1b9e77", fill = "#1b9e77")+
    geom_smooth(aes(y=rot.duration/10),
                method = the.method,
                colour = "#d95f02", fill = "#d95f02")+
    geom_smooth(aes(y=hdhd.mix.duration/10),
                method = the.method,
                colour = "#7570b3",
                fill = "#7570b3")+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0, 0))+
    xlab(x_label)+
    coord_cartesian(ylim = c(15, 50),
                    xlim = c(min(param),
                             max(param)),
                    expand = FALSE)+
    ylab("Strategy Lifespan (years)")+
    facet_grid(. ~ cross.resistance)+
    theme_bw()+
    theme(axis.title.y =  element_text(size = 12),
          axis.title.x =  element_text(size = 12),
          axis.text.x = element_text(size = 8, colour = "black", angle = 90),
          axis.text.y = element_text(size = 10, colour = "black"),
          legend.position = "bottom")

  return(the.plot)

}
#Where purple = hd mixtures
# orange = rotations
# green = micromosaics

senstivity_analysis_gam(param =micromosaics$Heritability, x_label = "Heritability",
                        mm.df = micromosaics,
                        the.method = "gam")
senstivity_analysis_gam(param =micromosaics$Intervention.Coverage, x_label = "Intervention Coverage",
                        mm.df = micromosaics,
                        the.method = "gam")
senstivity_analysis_gam(param =micromosaics$Dispersal, x_label = "Dispersal",
                        mm.df = micromosaics,
                        the.method = "gam")
senstivity_analysis_gam(param =micromosaics$Female.Fitness.Cost, x_label = "Female Fitness Cost",
                        mm.df = micromosaics,
                        the.method = "gam")
senstivity_analysis_gam(param =micromosaics$Male.Fitness.Cost, x_label = "Male Fitness Cost",
                        mm.df = micromosaics, the.method = "gam")
senstivity_analysis_gam(param =micromosaics$Female.Insecticide.Exposure, x_label = "Female Insecticide Exposure",
                        mm.df = micromosaics, the.method = "gam")
senstivity_analysis_gam(param =micromosaics$Male.Insecticide.Exposure, x_label = "Male Insecticide Exposure",
                        mm.df = micromosaics,
                        the.method = "gam")

#Partial Rank Correlation
cross.resistance.values = c(-0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5)

prcc.list.micromosaic = list()
for(i in 1:length(cross.resistance.values)){
  A = subset(micromosaics[, c(6:13, 4)], cross.resistance == cross.resistance.values[i])[, c(1:7, 9)]
  B = epiR::epi.prcc(dat = A, sided.test = 2, conf.level = 0.95)
  B$parameter = c(colnames(A)[1:7])
  B$cross.resistance = cross.resistance.values[i]
  B$strategy = "micromosaic"
  prcc.list.micromosaic[[i]] = B

}

prcc.df.micromosaic = do.call("rbind", prcc.list.micromosaic)

prcc.list.rotation = list()
for(i in 1:length(cross.resistance.values)){
  A = subset(rotations[, c(6:13, 4)], cross.resistance == cross.resistance.values[i])[, c(1:7, 9)]
  B = epiR::epi.prcc(dat = A, sided.test = 2, conf.level = 0.95)
  B$parameter = c(colnames(A)[1:7])
  B$cross.resistance = cross.resistance.values[i]
  B$strategy = "rotations"
  prcc.list.rotation[[i]] = B
}

prcc.df.rotation = do.call("rbind", prcc.list.rotation)

prcc.list.fdfdmixtures = list()
for(i in 1:length(cross.resistance.values)){
  A = subset(fd.fd.mixtures[, c(6:13, 4)], cross.resistance == cross.resistance.values[i])[, c(1:7, 9)]
  B = epiR::epi.prcc(dat = A, sided.test = 2, conf.level = 0.95)
  B$parameter = c(colnames(A)[1:7])
  B$cross.resistance = cross.resistance.values[i]
  B$strategy = "FD_FD mixtures"
  prcc.list.fdfdmixtures[[i]] = B
}

prcc.df.fdfd.mixtures = do.call("rbind", prcc.list.fdfdmixtures)

prcc.list.hdhdmixtures = list()
for(i in 1:length(cross.resistance.values)){
  A = subset(hd.hd.mixtures[, c(6:13, 4)], cross.resistance == cross.resistance.values[i])[, c(1:7, 9)]
  B = epiR::epi.prcc(dat = A, sided.test = 2, conf.level = 0.95)
  B$parameter = c(colnames(A)[1:7])
  B$cross.resistance = cross.resistance.values[i]
  B$strategy = "HD_HD mixtures"
  prcc.list.hdhdmixtures[[i]] = B
}
prcc.df.hdhd.mixtures = do.call("rbind", prcc.list.hdhdmixtures)


prcc.df = rbind(prcc.df.micromosaic, prcc.df.rotation, prcc.df.fdfd.mixtures, prcc.df.hdhd.mixtures)


ggplot(subset(prcc.df,
              strategy != "FD_FD mixtures"), aes(y=parameter, x = est,
                                                 fill = strategy))+
  geom_vline(xintercept = 0, linewidth = 1,
             colour = "grey")+
  geom_col(position = "dodge")+
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = "dodge")+
  ylab("Parameter")+
  xlim(-0.9, 0.25)+
  xlab("Estimate of the Correlation Coefficient")+
  #Where purple = hd mixtures
  # orange = rotations
  # green = micromosaics
  scale_fill_manual(values =  c("#7570b3", #purple
                                "#1b9e77", #green
                                "#d95f02" #orange
  ))+
  facet_grid(cross.resistance ~ .)+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 10),
        axis.title.x =  element_text(size = 10),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 7, colour = "black"),
        legend.position = "right")









