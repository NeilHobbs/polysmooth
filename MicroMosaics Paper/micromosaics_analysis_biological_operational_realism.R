library(ggplot2)
library(data.table)
library(randomForest)
library(patchwork)
library(ggh4x)


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


range(micromosaics.bio$mmvsrot)
range(micromosaics.bio$mmvshd.hd)
range(micromosaics.bio$mmvsfd.fd)
range(micromosaics.bio$mmvsrot.diff.end.bioassay)
range(micromosaics.bio$mmvshdhdmix.diff.end.bioassay)
range(micromosaics.bio$mmvsfdfdmix.diff.end.bioassay)

##Micro-Mosaics vs Rotations
label.df = data.frame(table(micromosaics.bio$mmvsrot.outcome))

data.frame(table(subset(micromosaics.bio,
                                   mmvsrot > 50)$mmvsrot.outcome))
#only 6220 wins are longer than 5 years
data.frame(table(subset(micromosaics.bio,
                        mmvsrot > 100)$mmvsrot.outcome))
#and only 2561 are longer than 10 years

6220/label.df$Freq[3]

2561/label.df$Freq[3]

label.df$Freq[3]/40000


plot.rotvmm1 = ggplot(subset(micromosaics.bio,
                             mmvsrot.outcome != "draw"), aes(x=-mmvsrot/10,
                                                             fill = mmvsrot.outcome))+
  geom_histogram(binwidth = 1, colour = "black")+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1,
             colour = "black")+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("A) Micro-Mosaics vs Rotations - Primary Outcome")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(limits = c(-36, 46),
                     breaks = seq(-35, 50, 5))+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  guides(fill = guide_legend(title = "Outcome"))+
  geom_label(data = subset(label.df,
                           Var1 == "draw"), mapping = aes(x= -5, y = 4500, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4, size = 6)+
  geom_label(data = subset(label.df, Var1 == "micro-mosaics win"),
             mapping = aes(x= -20, y = 2000, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4, size = 6)+
  geom_label(data = subset(label.df, Var1 == "micro-mosaics lose"),
             mapping = aes(x= 12, y = 2000, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4, size = 6)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")



#secondary outcome:
rot.mm.draws = subset(micromosaics.bio, mmvsrot.outcome == "draw")

draw.label.df = data.frame(table(rot.mm.draws$mmvsrot.diff.end.bioassay.outcome))

plot.rotvmm2 = ggplot(rot.mm.draws,
                      aes(x=mmvsrot.diff.end.bioassay *100,
                          fill = mmvsrot.diff.end.bioassay.outcome))+
  geom_histogram(binwidth = 0.5, colour = "black")+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1,
             colour = "black")+
  xlab("Difference in Total End Bioassay Survival")+
  ggtitle("B) Micro-Mosaics vs Rotations - Secondary Outcome")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(limits = c(-10, 20),
                     breaks = seq(-10, 20, 5))+
  scale_fill_manual(values = c( "coral", "skyblue"))+
  geom_label(data = subset(draw.label.df, Var1 == "micro-mosaics lose"), mapping = aes(x= 10, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4, size = 6)+
  geom_label(data = subset(draw.label.df, Var1 == "micro-mosaics win"), mapping = aes(x= -5, y = 500, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4, size = 6)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


#Full Dose Mixtures:

label.df.1 = data.frame(table(micromosaics.bio$mmvsfd.fd.outcome))

plot.fdfdvmm1 = ggplot(subset(micromosaics.bio,
                              mmvsfd.fd.outcome != "draw"), aes(x=-mmvsfd.fd/10,
                                                                fill = mmvsfd.fd.outcome))+
  geom_histogram(binwidth = 1, colour = "black")+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1,
             colour = "black")+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("C) Micro-Mosaics vs Full-Dose Mixtures - Primary Outcome")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(limits = c(-36, 46),
                     breaks = seq(-35, 50, 5))+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  geom_label(data = subset(label.df.1,
                           Var1 == "draw"), mapping = aes(x= 0, y = 1300, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4, size = 6)+
  geom_label(data = subset(label.df.1, Var1 == "micro-mosaics win"),
             mapping = aes(x= -12, y = 1000, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4, size = 6)+
  geom_label(data = subset(label.df.1, Var1 == "micro-mosaics lose"),
             mapping = aes(x= 10, y = 1000, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4, size = 6)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


#secondary outcome:
fdfdmix.mm.draws = subset(micromosaics.bio, mmvsfd.fd.outcome  == "draw")

draw.label.df.1 = data.frame(table(fdfdmix.mm.draws$mmvsfdfdmix.diff.end.bioassay.outcome))


plot.fdfdvmm2 = ggplot(fdfdmix.mm.draws,
                       aes(x=mmvsfdfdmix.diff.end.bioassay*100,
                           fill = mmvsfdfdmix.diff.end.bioassay.outcome))+
  geom_histogram(binwidth = 0.5, colour = "black")+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1,
             colour = "black")+
  xlab("Difference in Total End Bioassay Survival")+
  ggtitle("D) Micro-Mosaics vs Full-Dose Mixtures - Secondary Outcome")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(limits = c(-10, 20),
                     breaks = seq(-10, 20, 5))+
  scale_fill_manual(values = c("coral", "skyblue"))+
  guides(fill = guide_legend(title = "Secondary Outcome"))+
  geom_label(data = subset(draw.label.df.1, Var1 == "micro-mosaics lose"),
             mapping = aes(x= 15, y = 100, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4, size = 6)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


##Micro-Mosaics vs half dose mixtures
label.df.2 = data.frame(table(micromosaics.bio$mmvshd.hd.outcome))

plot.hdhdvmm1 = ggplot(subset(micromosaics.bio,
                              mmvshd.hd.outcome != "draw"), aes(x=-mmvshd.hd/10,
                                                                fill = mmvshd.hd.outcome))+
  geom_histogram(binwidth = 1, colour = "black")+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1,
             colour = "black")+
  xlab("Change in Operational Lifespan (years)")+
  ggtitle("E) Micro-Mosaics vs Half-Dose Mixtures Primary Outcome")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(limits = c(-36, 46),
                     breaks = seq(-35, 50, 5))+
  scale_fill_manual(values = c("darkred", "darkblue"))+
  guides(fill = guide_legend(title = "Outcome"))+
  geom_label(data = subset(label.df.2,
                           Var1 == "draw"),
             mapping = aes(x= -7, y = 2500, label = Freq),
             inherit.aes = FALSE, fill = "grey", alpha = 0.4, size = 6)+
  geom_label(data = subset(label.df.2, Var1 == "micro-mosaics win"),
             mapping = aes(x= -20, y = 1600, label = Freq),
             inherit.aes = FALSE, fill = "darkblue", alpha = 0.4, size = 6)+
  geom_label(data = subset(label.df.2, Var1 == "micro-mosaics lose"),
             mapping = aes(x= 20, y = 1600, label = Freq),
             inherit.aes = FALSE, fill = "darkred", alpha = 0.4, size = 6)+
  ylab("Frequency")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 12),
        axis.title.x =  element_text(size = 12),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        legend.position = "none")


hdhdmix.mm.draws = subset(micromosaics.bio, mmvshd.hd.outcome  == "draw")

draw.label.df.2 = data.frame(table(hdhdmix.mm.draws$mmvshdhdmix.diff.end.bioassay.outcome))


plot.hdhdvmm2 = ggplot(hdhdmix.mm.draws,
                       aes(x=mmvshdhdmix.diff.end.bioassay*100,
                           fill = mmvshdhdmix.diff.end.bioassay.outcome))+
  geom_histogram(binwidth = 0.5, colour = "black")+
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1,
             colour = "black")+
  xlab("Difference in Total End Bioassay Survival")+
  ggtitle("F) Micro-Mosaics vs Half-Dose Mixtures - Secondary Outcome")+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(limits = c(-10, 20),
                     breaks = seq(-10, 20, 5))+
  scale_fill_manual(values = c("coral", "skyblue"))+
  guides(fill = guide_legend(title = "Secondary Outcome"))+
  geom_label(data = subset(draw.label.df.2, Var1 == "micro-mosaics lose"),
             mapping = aes(x= 4, y = 2000, label = Freq),
             inherit.aes = FALSE, fill = "coral", alpha = 0.4, size = 6)+
  geom_label(data = subset(draw.label.df.2, Var1 == "micro-mosaics win"),
             mapping = aes(x= -5, y = 2000, label = Freq),
             inherit.aes = FALSE, fill = "skyblue", alpha = 0.4, size = 6)+
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
  filename = "Micromosaics_scenario3.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 2000,
  height = 1400,
  units = "px",
  dpi = 600)



# ggsave(
#   filename = "Micromosaics_scenario1C_figure7.jpeg",
#   plot = last_plot(),
#   scale = 5,
#   width = 1800,
#   height = 2000,
#   units = "px",
#   dpi = 600)


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

ggsave(
    filename = "chapter6_figure8.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)


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




