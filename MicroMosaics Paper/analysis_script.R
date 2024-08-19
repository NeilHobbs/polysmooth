#required Packages
library(devtools)
load_all() #polysmooth --> includes ggplot2
library(patchwork)
library(data.table)
library(ggh4x)
####################
# Read in Datasets #
####################

rotations = fread("mm_simulations_rotation_comparator.csv")
micromosaics = fread("mm_simulations_micromosaic_comparator.csv")
fd.fd.mixtures = fread("mm_simulations_mixtures_FDFD_comparator.csv")
hd.hd.mixtures = fread("mm_simulations_mixtures_HDHD_comparator.csv")

##
#These sims start at z=100 ; and with a withdrawal threshold of 20%
rotations.high = fread("mm_simulations_rotation_comparator_high_resistance.csv")
micromosaics.high = fread("mm_simulations_micromosaics_comparator_high_resistance.csv")
fd.fd.mixtures.high = fread("mm_simulations_mixtures_FDFD_comparator_high_resistance.csv")
hd.hd.mixtures.high = fread("mm_simulations_mixtures_HDHD_comparator_high_resistance.csv")

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



micromosaics.high$rot.vs.mm = rotations.high$sim.duration - micromosaics.high$sim.duration
micromosaics.high$mixfdfd.vs.mm = fd.fd.mixtures.high$sim.duration - micromosaics.high$sim.duration
micromosaics.high$mixhdhd.vs.mm = hd.hd.mixtures.high$sim.duration - micromosaics.high$sim.duration



micromosaics.high$rot.vs.mm.outcome = as.factor(ifelse(micromosaics.high$rot.vs.mm > 0,
                                                       yes = "micromosaics.high lose",
                                                       no = ifelse(micromosaics.high$rot.vs.mm < 0,
                                                                   yes = "micromosaics.high win",
                                                                   no = "draw")))


micromosaics.high$mixfdfd.vs.mm.outcome = as.factor(ifelse(micromosaics.high$mixfdfd.vs.mm > 0,
                                                           yes = "micromosaics.high lose",
                                                           no = ifelse(micromosaics.high$mixfdfd.vs.mm < 0,
                                                                       yes = "micromosaics.high win",
                                                                       no = "draw")))

micromosaics.high$mixhdhd.vs.mm.outcome = as.factor(ifelse(micromosaics.high$mixhdhd.vs.mm > 0,
                                                           yes = "micromosaics.high lose",
                                                           no = ifelse(micromosaics.high$mixhdhd.vs.mm < 0,
                                                                       yes = "micromosaics.high win",
                                                                       no = "draw")))




micromosaics.high$rot.vs.mm.peak.diff = micromosaics.high$peak.survival - rotations.high$peak.survival

micromosaics.high$rot.vs.mm.peak.diff.outcome = ifelse(micromosaics.high$peak.survival - rotations.high$peak.survival > 0,
                                                       yes = "micromosaics.high lose",
                                                       no = ifelse(micromosaics.high$peak.survival - rotations.high$peak.survival < 0,
                                                                   yes = "micromosaics.high win",
                                                                   no = "draw"))

micromosaics.high$mixfdfd.peak.diff = micromosaics.high$peak.survival - fd.fd.mixtures.high$peak.survival

micromosaics.high$mixfdfd.peak.diff.outcome = ifelse(micromosaics.high$peak.survival - fd.fd.mixtures.high$peak.survival > 0,
                                                     yes = "micromosaics.high lose",
                                                     no = ifelse(micromosaics.high$peak.survival - fd.fd.mixtures.high$peak.survival < 0,
                                                                 yes = "micromosaics.high win",
                                                                 no = "draw"))

micromosaics.high$mixhdhd.peak.diff = micromosaics.high$peak.survival - hd.hd.mixtures.high$peak.survival

micromosaics.high$mixhdhd.peak.diff.outcome = ifelse(micromosaics.high$peak.survival - hd.hd.mixtures.high$peak.survival > 0,
                                                     yes = "micromosaics.high lose",
                                                     no = ifelse(micromosaics.high$peak.survival - hd.hd.mixtures.high$peak.survival < 0,
                                                                 yes = "micromosaics.high win",
                                                                 no = "draw"))

micromosaics.high$rot.duration = rotations.high$sim.duration
micromosaics.high$fdfd.mix.duration = fd.fd.mixtures.high$sim.duration
micromosaics.high$hdhd.mix.duration = hd.hd.mixtures.high$sim.duration

###############################################
# Histograms of Primary and Secondary Outcomes#
###############################################

label.df = data.frame(table(micromosaics$rot.vs.mm.outcome))

plot.1 = ggplot(micromosaics, aes(x=rot.vs.mm/10,
                         fill = rot.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("Rotations vs Micro-Mosaics")+
  scale_fill_manual(values = c("grey", "darkred", "darkblue"))+
  guides(fill = guide_legend(title = "Outcome"))+
  geom_label(data = subset(label.df,
                           Var1 == "draw"), mapping = aes(x= 2, y = 10500, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4,
             size = 15)+
  geom_label(data = subset(label.df, Var1 == "micromosaics win"),
             mapping = aes(x= -15, y = 5500, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4,
             size = 15)+
  geom_label(data = subset(label.df, Var1 == "micromosaics lose"),
             mapping = aes(x= 10, y = 5500, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4,
             size = 15)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 15),
        axis.title.x =  element_text(size = 15),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")

##Micro-Mosaics vs Rotations
label.df = data.frame(table(micromosaics$cross.resistance,
                            micromosaics$rot.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

plot.2 = ggplot(subset(micromosaics,
                       rot.vs.mm.outcome != "draw"), aes(x=rot.vs.mm/10,
                         fill = rot.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("A) Primary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  facet_grid(cross.resistance ~ .)+
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
        legend.position = "bottom")


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


plot.3 = ggplot(rot.mm.draws,
       aes(x=rot.vs.mm.peak.diff*100,
           fill = rot.vs.mm.peak.diff.outcome))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("B) Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("coral", "skyblue"))+
  facet_grid(cross.resistance ~ .)+
  guides(fill = guide_legend(title = "Secondary Outcome"))+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics lose"), mapping = aes(x= 3, y = 100, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics win"), mapping = aes(x= -7, y = 100, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")


plot.2 + plot.3 + plot_annotation(title = "Micromosaics vs Rotations")

ggsave(
  filename = "chapter6_figure3a.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)

#Full Dose Mixtures:

label.df.1 = data.frame(table(micromosaics$cross.resistance,
                              micromosaics$mixfdfd.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

plot.1 = ggplot(subset(micromosaics,
              mixfdfd.vs.mm.outcome != "draw"), aes(x=mixfdfd.vs.mm/10,
                                   fill = mixfdfd.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("A) Primary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  facet_grid(cross.resistance ~ .)+
  guides(fill = guide_legend(title = "Outcome"))+
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
        legend.position = "bottom")


#secondary outcome:
fdfdmix.mm.draws = subset(micromosaics, mixfdfd.vs.mm.outcome  == "draw")

draw.label.df = data.frame(table(fdfdmix.mm.draws$cross.resistance,
                                 fdfdmix.mm.draws$mixfdfd.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)


plot.2 = ggplot(fdfdmix.mm.draws,
                aes(x=mixfdfd.peak.diff*100,
                    fill = mixfdfd.peak.diff.outcome))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("B) Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("coral", "skyblue"))+
  facet_grid(cross.resistance ~ .)+
  guides(fill = guide_legend(title = "Secondary Outcome"))+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics lose"), mapping = aes(x= 9, y = 65, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics win"), mapping = aes(x= -7, y = 100, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")


plot.1 + plot.2 + plot_annotation(title = "Micromosaics vs Full-Dose Mixtures")

ggsave(
  filename = "chapter6_figure3b.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)

##Micro-Mosaics vs half dose mixtures
label.df = data.frame(table(micromosaics$cross.resistance,
                            micromosaics$mixhdhd.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

plot.hdhd.1 = ggplot(subset(micromosaics,
                            mixhdhd.vs.mm.outcome != "draw"), aes(x=mixhdhd.vs.mm/10,
                                  fill = mixhdhd.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("A) Primary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  facet_grid(cross.resistance ~ .)+
  guides(fill = guide_legend(title = "Outcome"))+
  geom_label(data = subset(label.df,
                           Var2 == "draw"), mapping = aes(x= -1, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4)+
  geom_label(data = subset(label.df, Var2 == "micromosaics win"),
             mapping = aes(x= -6, y = 500, label = Freq),
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
        legend.position = "bottom")


hdhdmix.mm.draws = subset(micromosaics, mixhdhd.vs.mm.outcome  == "draw")

draw.label.df = data.frame(table(hdhdmix.mm.draws$cross.resistance,
                                 hdhdmix.mm.draws$mixfdfd.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)


plot.hdhd.2 = ggplot(hdhdmix.mm.draws,
                aes(x=mixhdhd.peak.diff*100,
                    fill = mixhdhd.peak.diff.outcome))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("B) Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("coral", "skyblue"))+
  facet_grid(cross.resistance ~ .)+
  guides(fill = guide_legend(title = "Secondary Outcome"))+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics lose"), mapping = aes(x= 5, y = 200, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics win"), mapping = aes(x= -2, y = 200, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")


plot.hdhd.1 + plot.hdhd.2 + plot_annotation(title = "Micromosaics vs Half-Dose Mixtures")


ggsave(
  filename = "chapter6_figure3c.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)





##Micro-Mosaics vs Rotations
label.df = data.frame(table(micromosaics$cross.resistance,
                            micromosaics$rot.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

plot.rotvmm1 = ggplot(subset(micromosaics,
                             rot.vs.mm.outcome != "draw"), aes(x=rot.vs.mm/10,
                                                               fill = rot.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
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
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("B) Micro-Mosaics vs Rotations - Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
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
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics lose"), mapping = aes(x= 3, y = 100, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics win"), mapping = aes(x= -7, y = 100, label = Freq),
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
  geom_histogram(binwidth = 1)+
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
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("D) Micro-Mosaics vs Full-Dose Mixtures - Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
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
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics lose"), mapping = aes(x= 9, y = 65, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics win"), mapping = aes(x= -7, y = 100, label = Freq),
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
  geom_histogram(binwidth = 1)+
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
                                 hdhdmix.mm.draws$mixfdfd.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)


plot.hdhdvmm2 = ggplot(hdhdmix.mm.draws,
                       aes(x=mixhdhd.peak.diff*100,
                           fill = mixhdhd.peak.diff.outcome))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("F) Micro-Mosaics vs Half-Dose Mixtures - Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
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
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics lose"), mapping = aes(x= 5, y = 200, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics win"), mapping = aes(x= -2, y = 200, label = Freq),
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
  scale_fill_manual(values = c("darkblue", "darkred", "skyblue", "coral"))+
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
  filename = "Micromosaics_scenario1A_figure3.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 1800,
  height = 2000,
  units = "px",
  dpi = 600)



############################
#Summary Tables ############
############################

table(micromosaics$rot.vs.mm.outcome)
table(subset(micromosaics, rot.vs.mm.outcome == "draw")$rot.vs.mm.peak.diff.outcome)

mm.win = 11985 + 12733
rot.win = 7300 + 2982
mm.win
mm.win/35000*100
rot.win/35000*100


##########################################
# Plots with higher initial resistance & #
# higher withdrawal threshold            #
##########################################


label.df = data.frame(table(micromosaics.high$rot.vs.mm.outcome))

plot.1 = ggplot(micromosaics.high, aes(x=rot.vs.mm/10,
                                       fill = rot.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("Rotations vs Micro-Mosaics")+
  scale_fill_manual(values = c("grey", "darkred", "darkblue"))+
  guides(fill = guide_legend(title = "Outcome"))+
  geom_label(data = subset(label.df,
                           Var1 == "draw"), mapping = aes(x= 2, y = 10500, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4,
             size = 15)+
  geom_label(data = subset(label.df, Var1 == "micromosaics win"),
             mapping = aes(x= -15, y = 5500, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4,
             size = 15)+
  geom_label(data = subset(label.df, Var1 == "micromosaics lose"),
             mapping = aes(x= 10, y = 5500, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4,
             size = 15)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 15),
        axis.title.x =  element_text(size = 15),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")

##Micro-Mosaics vs Rotations
label.df = data.frame(table(micromosaics.high$cross.resistance,
                            micromosaics.high$rot.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

plot.2 = ggplot(subset(micromosaics.high,
                       rot.vs.mm.outcome != "draw"), aes(x=rot.vs.mm/10,
                                                         fill = rot.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("A) Primary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("darkred", "darkblue"),
                    labels = c("micromosaics lose", "micromosaics win"))+
  facet_grid(cross.resistance ~ .)+
  guides(fill = guide_legend(title = "Outcome"))+
  geom_label(data = subset(label.df,
                           Var2 == "draw"), mapping = aes(x= -3, y = 780, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4)+
  geom_label(data = subset(label.df, Var2 == "micromosaics.high win"),
             mapping = aes(x= -12, y = 780, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4)+
  geom_label(data = subset(label.df, Var2 == "micromosaics.high lose"),
             mapping = aes(x= 12, y = 780, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")



rot.mm.draws.high = subset(micromosaics.high, rot.vs.mm.outcome == "draw")

draw.label.df = data.frame(table(rot.mm.draws.high$cross.resistance,
                                 rot.mm.draws.high$rot.vs.mm.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)


plot.3 = ggplot(rot.mm.draws.high,
                aes(x=rot.vs.mm.peak.diff*100,
                    fill = rot.vs.mm.peak.diff.outcome))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("B) Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("coral", "skyblue"),
                    labels = c("micromosaics lose", "micromosaics win"))+
  facet_grid(cross.resistance ~ .)+
  guides(fill = guide_legend(title = "Secondary Outcome"))+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics.high lose"), mapping = aes(x= 5, y = 50, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics.high win"), mapping = aes(x= -5, y = 50, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")


plot.2 + plot.3 + plot_annotation(title = "Micromosaics vs Rotations: Higher Starting Resistance")


ggsave(
  filename = "chapter6_figure5a.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)

#Full Dose Mixtures:

label.df.1 = data.frame(table(micromosaics.high$cross.resistance,
                              micromosaics.high$mixfdfd.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

plot.1 = ggplot(subset(micromosaics.high,
                       mixfdfd.vs.mm.outcome != "draw"), aes(x=mixfdfd.vs.mm/10,
                                                             fill = mixfdfd.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("A) Primary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("darkred", "darkblue"),
                    labels = c("micromosaics lose", "micromosaics win"))+
  facet_grid(cross.resistance ~ .)+
  guides(fill = guide_legend(title = "Outcome"))+
  geom_label(data = subset(label.df.1,
                           Var2 == "draw"), mapping = aes(x= 0, y = 350, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4)+
  geom_label(data = subset(label.df.1, Var2 == "micromosaics.high lose"),
             mapping = aes(x= 25, y = 350, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4)+
  geom_label(data = subset(label.df.1, Var2 == "micromosaics.high win"),
             mapping = aes(x= -10, y = 350, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")


#secondary outcome:
fdfdmix.mm.draws.high = subset(micromosaics.high, mixfdfd.vs.mm.outcome  == "draw")

draw.label.df = data.frame(table(fdfdmix.mm.draws.high$cross.resistance,
                                 fdfdmix.mm.draws.high$mixfdfd.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)


plot.2 = ggplot(fdfdmix.mm.draws.high,
                aes(x=mixfdfd.peak.diff*100,
                    fill = mixfdfd.peak.diff.outcome))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("B) Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("coral", "skyblue"),
                    labels = c("micromosaics lose", "micromosaics win"))+
  facet_grid(cross.resistance ~ .)+
  guides(fill = guide_legend(title = "Secondary Outcome"))+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics.high lose"), mapping = aes(x= 5, y = 30, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics.high win"), mapping = aes(x= -5, y = 30, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")


plot.1 + plot.2 + plot_annotation(title = "Micromosaics vs Full-Dose Mixtures: Higher Starting Resistance")


ggsave(
  filename = "chapter6_figure5b.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)
##Micro-Mosaics vs half dose mixtures
label.df = data.frame(table(micromosaics.high$cross.resistance,
                            micromosaics.high$mixhdhd.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

plot.hdhd.1 = ggplot(subset(micromosaics.high,
                            mixhdhd.vs.mm.outcome != "draw"), aes(x=mixhdhd.vs.mm/10,
                                                                  fill = mixhdhd.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("A) Primary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("darkblue", "darkred"),
                    labels = c("micromosaics win", "micromosaics lose"))+
  facet_grid(cross.resistance ~ .)+
  guides(fill = guide_legend(title = "Outcome"))+
  geom_label(data = subset(label.df,
                           Var2 == "draw"), mapping = aes(x= -3, y = 900, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4)+
  geom_label(data = subset(label.df, Var2 == "micromosaics.high win"),
             mapping = aes(x= -20, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4)+
  geom_label(data = subset(label.df, Var2 == "micromosaics.high lose"),
             mapping = aes(x= 10, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4)+
  ylab("Frequency")+

  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")


hdhdmix.mm.draws.high = subset(micromosaics.high, mixhdhd.vs.mm.outcome  == "draw")

draw.label.df = data.frame(table(hdhdmix.mm.draws.high$cross.resistance,
                                 hdhdmix.mm.draws.high$mixhdhd.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)


plot.hdhd.2 = ggplot(hdhdmix.mm.draws.high,
                     aes(x=mixhdhd.peak.diff*100,
                         fill = mixhdhd.peak.diff.outcome))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("B) Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_fill_manual(values = c("skyblue", "coral"),
                    labels = c("micromosaics win", "micromosaics lose"))+
  facet_grid(cross.resistance ~ .)+
  guides(fill = guide_legend(title = "Secondary Outcome"))+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics.high lose"), mapping = aes(x= 5, y = 50, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, Var2 == "micromosaics.high win"), mapping = aes(x= -5, y = 50, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "bottom")


plot.hdhd.1 + plot.hdhd.2 + plot_annotation(title = "Micromosaics vs Half-Dose Mixtures: Higher Starting Resistance")

ggsave(
  filename = "chapter6_figure5c.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)




##Micro-Mosaics vs Rotations
label.df = data.frame(table(micromosaics.high$cross.resistance,
                            micromosaics.high$rot.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

label.df$outcome = rep(c("Draw", "Micro-Mosaics Win", "Micro-Mosaics Lose"), each = 7)

plot.rotvmm1 = ggplot(subset(micromosaics.high,
                             rot.vs.mm.outcome != "draw"), aes(x=rot.vs.mm/10,
                                                               fill = rot.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
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
                           outcome == "Draw"), mapping = aes(x= -5, y = 600, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4)+
  geom_label(data = subset(label.df, outcome == "Micro-Mosaics Win"),
             mapping = aes(x= -20, y = 400, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4)+
  geom_label(data = subset(label.df, outcome == "Micro-Mosaics Lose"),
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
rot.mm.draws = subset(micromosaics.high, rot.vs.mm.outcome == "draw")

draw.label.df = data.frame(table(rot.mm.draws$cross.resistance,
                                 rot.mm.draws$rot.vs.mm.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

draw.label.df$outcome = rep(c("Micro-Mosaics Lose", "Micro-Mosaics Win"), each = 7)


plot.rotvmm2 = ggplot(rot.mm.draws,
                      aes(x=rot.vs.mm.peak.diff*100,
                          fill = rot.vs.mm.peak.diff.outcome))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("B) Micro-Mosaics vs Rotations - Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
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
  geom_label(data = subset(draw.label.df, outcome == "Micro-Mosaics Lose"), mapping = aes(x= 4, y = 50, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df, outcome == "Micro-Mosaics Win"), mapping = aes(x= -5, y = 50, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


#Full Dose Mixtures:

label.df.1 = data.frame(table(micromosaics.high$cross.resistance,
                              micromosaics.high$mixfdfd.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)

label.df.1$outcome = rep(c("Draw", "Micro-Mosaics Lose", "Micro-Mosaics Win"), each = 7)


plot.fdfdvmm1 = ggplot(subset(micromosaics.high,
                              mixfdfd.vs.mm.outcome != "draw"), aes(x=mixfdfd.vs.mm/10,
                                                                    fill = mixfdfd.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
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
                           outcome == "Draw"), mapping = aes(x= 0, y = 400, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4)+
  geom_label(data = subset(label.df.1, outcome == "Micro-Mosaics Win"),
             mapping = aes(x= -12, y = 400, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4)+
  geom_label(data = subset(label.df.1, outcome == "Micro-Mosaics Lose"),
             mapping = aes(x= 20, y = 300, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


#secondary outcome:
fdfdmix.mm.draws = subset(micromosaics.high, mixfdfd.vs.mm.outcome  == "draw")

draw.label.df.1 = data.frame(table(fdfdmix.mm.draws$cross.resistance,
                                   fdfdmix.mm.draws$mixfdfd.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)
draw.label.df.1$outcome = rep(c("Micro-Mosaics Lose", "Micro-Mosaics Win"), each = 7)


plot.fdfdvmm2 = ggplot(fdfdmix.mm.draws,
                       aes(x=mixfdfd.peak.diff*100,
                           fill = mixfdfd.peak.diff.outcome))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("D) Micro-Mosaics vs Full-Dose Mixtures - Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
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
  geom_label(data = subset(draw.label.df.1, outcome == "Micro-Mosaics Lose"), mapping = aes(x= 7, y = 25, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df.1, outcome == "Micro-Mosaics Win"), mapping = aes(x= -5, y = 25, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


##Micro-Mosaics vs half dose mixtures
label.df.2 = data.frame(table(micromosaics.high$cross.resistance,
                              micromosaics.high$mixhdhd.vs.mm.outcome))|>
  dplyr::rename("cross.resistance" = Var1)
label.df.2$outcome = rep(c("Draw", "Micro-Mosaics Win"), each = 7)


plot.hdhdvmm1 = ggplot(subset(micromosaics.high,
                              mixhdhd.vs.mm.outcome != "draw"), aes(x=mixhdhd.vs.mm/10,
                                                                    fill = mixhdhd.vs.mm.outcome))+
  geom_histogram(binwidth = 1)+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("E) Micro-Mosaics vs Half-Dose Mixtures Primary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_x_continuous(limits = c(-30, 45))+
  scale_fill_manual(values = c("darkblue"))+
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
  geom_label(data = subset(label.df.2,
                           outcome == "Draw"), mapping = aes(x= 3, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4)+
  geom_label(data = subset(label.df.2, outcome == "Micro-Mosaics Win"),
             mapping = aes(x= -12, y = 600, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4)+
  ylab("Frequency")+

  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


hdhdmix.mm.draws = subset(micromosaics.high, mixhdhd.vs.mm.outcome  == "draw")

draw.label.df.2 = data.frame(table(hdhdmix.mm.draws$cross.resistance,
                                   hdhdmix.mm.draws$mixfdfd.peak.diff.outcome))|>
  dplyr::rename("cross.resistance" = Var1)
draw.label.df.2$outcome = rep(c("Micro-Mosaics Lose", "Micro-Mosaics Win"), each = 7)


plot.hdhdvmm2 = ggplot(hdhdmix.mm.draws,
                       aes(x=mixhdhd.peak.diff*100,
                           fill = mixhdhd.peak.diff.outcome))+
  geom_histogram(binwidth = 0.1)+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey")+
  xlab("Difference in Peak Bioassay Survival Percentage")+
  ggtitle("F) Micro-Mosaics vs Half-Dose Mixtures - Secondary Outcome")+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                         breaks = NULL,
                                         labels = NULL))+
  scale_x_continuous(limits = c(-7.5, 11))+
  scale_fill_manual(values = c("skyblue"))+
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
  geom_label(data = subset(draw.label.df.2, outcome == "Micro-Mosaics Lose"), mapping = aes(x= 4, y = 50, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4)+
  geom_label(data = subset(draw.label.df.2, outcome == "Micro-Mosaics Win"), mapping = aes(x= -5, y = 50, label = Freq),
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
  scale_fill_manual(values = c("darkblue", "darkred", "skyblue", "coral"))+
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
  filename = "Micromosaics_scenario1B_figure5.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 1800,
  height = 2000,
  units = "px",
  dpi = 600)





























########################
# SENSITIVITY ANALYSIS #
########################

colnames(micromosaics)
##GAMs for all Parameters stratified by cross resistance



senstivity_analysis_gam = function(param, x_label,
                                   mm.df){

  the.plot = ggplot(mm.df, aes(x=param,
                                      y=sim.duration))+
    geom_smooth(method = "gam", colour = "darkred", fill = "red")+
    geom_smooth(aes(y=rot.duration),
                method = "gam",
                colour = "darkblue", fill = "blue")+
    geom_smooth(aes(y=fdfd.mix.duration),
                method = "gam",
                colour = "darkorchid4",
                fill = "darkorchid")+
    geom_smooth(aes(y=hdhd.mix.duration),
                method = "gam",
                colour = "springgreen4",
                fill = "springgreen")+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Cross Resistance",
                                           breaks = NULL,
                                           labels = NULL),
                       expand = c(0, 0))+
    xlab(x_label)+
    coord_cartesian(ylim = c(150, 502),
                    expand = FALSE)+
    ylab("Simulation Duration (generations)")+
    ggtitle(x_label)+
    facet_grid(. ~ cross.resistance)+
    theme_bw()+
    theme(axis.title.y =  element_text(size = 12),
          axis.title.x =  element_text(size = 12),
          axis.text.x = element_text(size = 8, colour = "black", angle = 90),
          axis.text.y = element_text(size = 8, colour = "black"),
          legend.position = "bottom")

  return(the.plot)

}

senstivity_analysis_gam(param =micromosaics$Heritability, x_label = "Heritability",
                        mm.df = micromosaics)
senstivity_analysis_gam(param =micromosaics$Intervention.Coverage, x_label = "Intervention Coverage",
                        mm.df = micromosaics)
senstivity_analysis_gam(param =micromosaics$Dispersal, x_label = "Dispersal",
                        mm.df = micromosaics)
senstivity_analysis_gam(param =micromosaics$Female.Fitness.Cost, x_label = "Female Fitness Cost", mm.df = micromosaics)
senstivity_analysis_gam(param =micromosaics$Male.Fitness.Cost, x_label = "Male Fitness Cost", mm.df = micromosaics)
senstivity_analysis_gam(param =micromosaics$Female.Insecticide.Exposure, x_label = "Female Insecticide Exposure",
                        mm.df = micromosaics)
senstivity_analysis_gam(param =micromosaics$Male.Insecticide.Exposure, x_label = "Male Insecticide Exposure",
                        mm.df = micromosaics)


senstivity_analysis_gam(param =micromosaics.high$Male.Fitness.Cost, x_label = "Male Fitness Cost",
                        mm.df = micromosaics.high)

senstivity_analysis_gam(param =micromosaics.high$Female.Fitness.Cost, x_label = "Female Fitness Cost",
                        mm.df = micromosaics.high)



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


ggplot(prcc.df, aes(y=parameter, x = est,
                    fill = strategy))+
  geom_vline(xintercept = 0, linewidth = 1,
             colour = "grey")+
  geom_col(position = "dodge")+
  geom_errorbarh(aes(xmin = lower,
                    xmax = upper), position = "dodge")+
  ylab("Parameter")+
  xlim(-0.9, 0.25)+
  xlab("Estimate of the Correlation Coefficient")+
  scale_fill_manual(values =  c("darkorchid",
                                "springgreen",
                                "darkred",
                                "darkblue"))+
  facet_wrap(cross.resistance ~ .)+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 10),
        axis.title.x =  element_text(size = 10),
      axis.text.x = element_text(size = 10, colour = "black"),
      axis.text.y = element_text(size = 10, colour = "black"),
      legend.position = "bottom")



#Random Forest Models
library(randomForest)

#First for choosing between micromosaics and rotations:::
micromosaics$overall.outcome = ifelse(micromosaics$rot.vs.mm.outcome == "draw",
                                      yes = micromosaics$rot.vs.mm.peak.diff.outcome,
                                      no = micromosaics$rot.vs.mm.outcome)


temp.df.1 = micromosaics[, c(6:13, 30) ]


## 75% of the sample size
sample.size = floor(0.7 * nrow(temp.df.1))

## set the seed to make your partition reproducible
set.seed(42)
train.ind = sample(seq_len(nrow(temp.df.1)), size = sample.size)

train.rf. = temp.df.1[train.ind, ]
test.rf. = temp.df.1[-train.ind, ]



rf.model = randomForest::randomForest(as.factor(overall.outcome) ~ .,
                                      data = train.rf.,
                                      type = "classification",
                                      importance = TRUE,
                                      ntree = 100,
                                      mtry = 4,
                                      node.size = 100)

rf.model.df = data.frame(rf.model$importance)
rf.model.df.sd = data.frame(rf.model$importanceSD)

#colnames(temp.df.1[1:11 , ])

rf.model.df$parameter = colnames(temp.df.1[ ,1:8 ])


rf.predict = predict(rf.model, test.rf.)

accuracy = ifelse(rf.predict == test.rf.$overall.outcome,
                  yes = 1,
                  no = 0)

sum(accuracy)/10500*100
#Model accuracy is 87.65%




rf.plot.acc = ggplot(rf.model.df, aes(x=MeanDecreaseAccuracy,
                                      y=reorder(parameter, MeanDecreaseAccuracy)))+

  geom_col(colour = "blue", fill= "skyblue")+
  xlab("Mean Decrease Accuracy")+
  ylab("Parameter")+
  ggtitle("A")+
  theme_classic()+
  theme(legend.position = "none")

rf.plot.acc
####
micromosaics$win.magnitude.primary = ifelse(micromosaics$rot.mm.percentage <= -10,
                                            yes = "micromosaics",
                                            no = ifelse(micromosaics$rot.mm.percentage >= 10,
                                                        yes = "rotations",
                                                        no = "draw"))

micromosaics$win.magnitude.secondary = ifelse(micromosaics$rot.mm.sec.percentage <= -10,
                                              yes = "rotations",
                                              no = ifelse(micromosaics$rot.mm.sec.percentage >= 10,
                                                          yes = "micromosaics",
                                                          no = "draw"))

micromosaics$win.magnitude = ifelse(micromosaics$win.magnitude.primary != "draw",
                                    yes = micromosaics$win.magnitude.primary,
                                    no = micromosaics$win.magnitude.secondary)

colnames(micromosaics)



ggplot(micromosaics, aes(x = Female.Fitness.Cost,
                         y = Male.Fitness.Cost,
                         colour = win.magnitude))+
  geom_point()+
  facet_grid(cross.resistance ~ .)



library(rpart)
library(rpart.plot)
regression.tree = rpart(win.magnitude ~
                          Female.Fitness.Cost +
                          Male.Fitness.Cost +
                          Heritability +
                          Intervention.Coverage +
                          Dispersal +
                          Female.Insecticide.Exposure +
                          Male.Insecticide.Exposure +
                          cross.resistance,
                        data = micromosaics,
                        method = "class",
                        control = rpart.control(minsplit = 100,
                                                min.bucket = 100,
                                                maxdepth = 5))

rpart.plot(regression.tree)


####################################################
# Insecticides given unique properties #############
####################################################
micromosaics.bio = fread("micromosaics_operational_biological_realism_micromosaics_simulations.csv")
rotations.bio = fread("micromosaics_operational_biological_realism_rotations_simulations.csv")
hd.hd.mixtures.bio = fread("micromosaics_operational_biological_realism_mixtures_HDHD_simulations.csv")
fd.fd.mixtures.bio = fread("micromosaics_operational_biological_realism_mixtures_FDFD_simulations.csv")



