library(ggplot2)
library(patchwork)

fdfd.decay.df = data.table::fread("mm_simulations_mixtures_FDFD_comparator_decay.csv")
hdhd.decay.df = data.table::fread("mm_simulations_mixtures_HDHD_comparator_decay.csv")
rotations.decay.df = data.table::fread("mm_simulations_rotation_comparator_decay.csv")
micromosaics.decay.df = data.table::fread("mm_simulations_micromosaic_comparator_decay.csv")


comparison_plot = function(comparator.df,
                           strategy,
                           prim.pos,
                           prim.zero,
                           prim.neg,
                           prim.x.min,
                           prim.x.max,
                           sec.pos,
                           sec.zero,
                           sec.neg,
                          sec.x.min,
                          sec.x.max){


#makes it so negative numbers --> micromosaics win
    #consistent with other graphs in the micro-mosaics paper

duration.difference = comparator.df$sim.duration - micromosaics.decay.df$sim.duration

  #positive numbers --> micro-mosaics win
peak.survival.difference =  micromosaics.decay.df$peak.survival - comparator.df$peak.survival

decay.rate.description = ifelse(micromosaics.decay.df$decay.rate.i == micromosaics.decay.df$decay.rate.j,
                                yes = paste0("Same\nDecay\nRate"),
                                no = ifelse(micromosaics.decay.df$decay.rate.i > micromosaics.decay.df$decay.rate.j,
                                yes = paste0("Different\nDecay Rate:\nFaster"),
                                no = paste0("Different\nDecay Rate:\nSlower")))

decay.rate.description = factor(decay.rate.description, levels = c(paste0("Different\nDecay Rate:\nFaster"),
                                                                   paste0("Same\nDecay\nRate"),
                                                                          paste0("Different\nDecay Rate:\nSlower")))

micromosaics.vs.comparator= data.frame(duration.difference,
                                       peak.survival.difference,
                                       cross.resistance = micromosaics.decay.df$cross.resistance,
                                       decay.rate.description)


#light colours = micromosaics lose
#dark colours = micromosaics win
  #colours = cross resistance

micromosaics.vs.comparator$primary.outcome = as.factor(ifelse(micromosaics.vs.comparator$duration.difference == 0,
                                                   yes = "draw",
                                                   no = ifelse(micromosaics.vs.comparator$duration.difference < 0,
                                                               yes = "micro-mosaics win",
                                                               no = paste0(strategy, "win"))))

micromosaics.vs.comparator$secondary.outcome = as.factor(ifelse(micromosaics.vs.comparator$peak.survival.difference == 0,
                                                    yes = "draw",
                                                    no = ifelse(micromosaics.vs.comparator$peak.survival.difference < 0,
                                                                yes = "micro-mosaics win",
                                                                no = paste0(strategy, "win"))))



primary.df = data.frame(table(micromosaics.vs.comparator$primary.outcome, micromosaics.vs.comparator$cross.resistance,
                              micromosaics.vs.comparator$decay.rate.description))

primary.df = subset(primary.df, Var1 != "draw")|>
  dplyr::rename("primary.outcome" = Var1)|>
  dplyr::rename("cross.resistance" = Var2)|>
  dplyr::rename("decay.rate.description" = Var3)

draws.df = subset(micromosaics.vs.comparator, primary.outcome == "draw")

secondary.df = data.frame(table(draws.df$secondary.outcome, draws.df$cross.resistance,
                                draws.df$decay.rate.description))

secondary.df = secondary.df|>
  dplyr::rename("secondary.outcome" = Var1)|>
  dplyr::rename("cross.resistance" = Var2)|>
  dplyr::rename("decay.rate.description" = Var3)





#Values need changing for each plot
primary.df$x.val = ifelse(primary.df$primary.outcome == "micro-mosaics win",
                          yes = prim.x.max,
                          no = prim.x.min)

primary.df$y.val = ifelse(primary.df$cross.resistance == -0.3,
                          yes = prim.neg,
                          no = ifelse(primary.df$cross.resistance == 0,
                                      yes = prim.zero,
                                      no = prim.pos))

secondary.df$x.val = ifelse(secondary.df$secondary.outcome == "micro-mosaics win",
                            yes = sec.x.min,
                            no = sec.x.max)

secondary.df$y.val = ifelse(primary.df$cross.resistance == -0.3,
                            yes = sec.neg,
                            no = ifelse(primary.df$cross.resistance == 0,
                                        yes = sec.zero,
                                        no = sec.pos))



primary.plot = ggplot(subset(micromosaics.vs.comparator,
              duration.difference != 0), aes(x=duration.difference/10,
                                             fill = as.factor(cross.resistance)))+
  geom_histogram(binwidth = 3, colour = "black")+
  scale_fill_manual(values = c("#ef8a62",
                               "#f7f7f7",
                               "#67a9cf"))+
  geom_label(data = primary.df, mapping =aes(x=x.val,
                                       y = y.val,
                                       label = Freq), size = 3)+

  geom_vline(xintercept = 0, linetype = "dashed")+
  xlim(-24, 35)+
  facet_grid(decay.rate.description~.)+
  xlab(paste0("Difference in Strategy Lifespan (years)"))+
  ggtitle(paste0("Micro-Mosaics vs ", strategy))+
  ylab("Count")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12),
        title = element_text(size = 15))


secondary.plot = ggplot(subset(micromosaics.vs.comparator,
              duration.difference == 0), aes(x=peak.survival.difference*100,
                                             fill = as.factor(cross.resistance)))+
  geom_histogram(binwidth = 0.25,
                  colour = "black")+
  scale_fill_manual(values = c("#ef8a62",
                               "#f7f7f7",
                               "#67a9cf"))+
  geom_label(data = secondary.df, mapping =aes(x=x.val,
                                             y = y.val,
                                             label = Freq), size = 3)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  facet_grid(decay.rate.description~.)+
  # ggtitle(paste0("Secondary Outcome"))+
  xlab(paste0("Absolute Difference in Peak Bioassay Survival"))+
  xlim(-7, 11.1)+
  ylab("Count")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        strip.text = element_text(size = 12),
        title = element_text(size = 15))

#
# the.legend = cowplot::get_legend(ggplot(subset(micromosaics.vs.comparator,
#                            duration.difference == 0), aes(x=peak.survival.difference*100,
#                                                           fill = as.factor(cross.resistance)))+
#                              geom_histogram()+
#   scale_fill_manual(values = c("#8da0cb", "#66c2a5", "#fc8d62"))+
#     guides(fill=guide_legend(title=paste0("Cross\nResistance")))+
#     theme_bw()+
#   theme(legend.position = "bottom"))

the.layout = "
AAAAABBBBB
AAAAABBBBB
AAAAABBBBB
AAAAABBBBB
AAAAABBBBB
AAAAABBBBB
AAAAABBBBB
AAAAABBBBB
AAAAABBBBB
AAAAABBBBB
AAAAABBBBB
###CCCC###
"

end.plot = primary.plot +
  secondary.plot +
   # the.legend +
  # plot_annotation(title = paste0("Micro-Mosaics vs ", strategy)) +
  plot_layout(design = the.layout)

return(end.plot)
}

# comparison_plot(
#   comparator.df = hdhd.decay.df,
#   strategy = "Half Dose Mixtures",
#   prim.pos = 2000,
#   prim.zero = 1500,
#   prim.neg = 1000,
#   prim.x.min = 20,
#   prim.x.max = -5,
#   sec.pos = 1500,
#   sec.zero = 1200,
#   sec.neg = 900,
#   sec.x.min = -2,
#   sec.x.max = 3)
#
# # ggsave(
# #   filename = "chapter6_figure9a.jpeg",
# #   plot = last_plot(),
# #   scale = 10,
# #   width = 400,
# #   height = 200,
# #   units = "px",
# #   dpi = 300)
#
# comparison_plot(
#   comparator.df = rotations.decay.df,
#   strategy = "Rotations",
#   prim.pos = 1500,
#   prim.zero = 1200,
#   prim.neg = 800,
#   prim.x.min = 15,
#   prim.x.max = -15,
#   sec.pos = 750,
#   sec.zero = 600,
#   sec.neg = 450,
#   sec.x.min = 4,
#   sec.x.max = -4)
#
# ggsave(
#   filename = "chapter6_figure9b.jpeg",
#   plot = last_plot(),
#   scale = 10,
#   width = 400,
#   height = 200,
#   units = "px",
#   dpi = 300)
#
# comparison_plot(
#   comparator.df = fdfd.decay.df,
#   strategy = "Full Dose Mixtures",
#   prim.pos = 1000,
#   prim.zero = 800,
#   prim.neg = 600,
#   prim.x.min = 30,
#   prim.x.max = -1.5,
#   sec.pos = 350,
#   sec.zero = 250,
#   sec.neg = 150,
#   sec.x.min = -1,
#   sec.x.max = 10)
#
#
# ggsave(
#   filename = "chapter6_figure9c.jpeg",
#   plot = last_plot(),
#   scale = 10,
#   width = 400,
#   height = 200,
#   units = "px",
#   dpi = 300)
#
#
#
rot.plot = comparison_plot(
  comparator.df = rotations.decay.df,
  strategy = "Rotations",
  prim.pos = 1500,
  prim.zero = 1200,
  prim.neg = 800,
  prim.x.min = 15,
  prim.x.max = -15,
  sec.pos = 750,
  sec.zero = 600,
  sec.neg = 450,
  sec.x.min = 4,
  sec.x.max = -4)

fdfd.plot = comparison_plot(
  comparator.df = fdfd.decay.df,
  strategy = "Full Dose Mixtures",
  prim.pos = 1000,
  prim.zero = 800,
  prim.neg = 600,
  prim.x.min = 30,
  prim.x.max = -1.5,
  sec.pos = 350,
  sec.zero = 250,
  sec.neg = 150,
  sec.x.min = -1,
  sec.x.max = 10)

hdhd.plot = comparison_plot(
  comparator.df = hdhd.decay.df,
  strategy = "Half Dose Mixtures",
  prim.pos = 2000,
  prim.zero = 1500,
  prim.neg = 1000,
  prim.x.min = 20,
  prim.x.max = -5,
  sec.pos = 1500,
  sec.zero = 1200,
  sec.neg = 900,
  sec.x.min = -2,
  sec.x.max = 3)

rot.plot/fdfd.plot/hdhd.plot





#
# the.legend = cowplot::get_legend(ggplot(subset(micromosaics.vs.comparator,
#                            duration.difference == 0), aes(x=peak.survival.difference*100,
#                                                           fill = as.factor(cross.resistance)))+
#                              geom_histogram()+
#   scale_fill_manual(values = c("#8da0cb", "#66c2a5", "#fc8d62"))+
#     guides(fill=guide_legend(title=paste0("Cross\nResistance")))+
#     theme_bw()+
#   theme(legend.position = "bottom"))


legend.df = data.frame(x = c(1, 2, 3),
                       y = c(1, 1, 1),
                       cr = c("Cross Resistance: -0.3",
                              "Cross Resistance: 0",
                              "Cross Resistance: 0.3"))


legend.plot = ggplot(legend.df, aes(x = x, y=y, fill = cr))+
  geom_tile(colour = "black", size = 3)+
  geom_text(aes(label = cr), size = 10)+
  scale_fill_manual(values = c("#67a9cf",
                               "#f7f7f7",
                               "#ef8a62"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")


the.design = "
A
A
A
A
A
A
B
B
B
B
B
B
C
C
C
C
C
C
D"


A = rot.plot/fdfd.plot/hdhd.plot

A + legend.plot + plot_layout(design = the.design)



ggsave(
  filename = "micromosaics_insecticide_decay.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 1400,
  height = 1700,
  units = "px",
  dpi = 500)



#Sensitivity Analysis::::

micromosaics.decay.df$rot.duration = rotations.decay.df$sim.duration
micromosaics.decay.df$hdhd.mix.duration = hdhd.decay.df$sim.duration
micromosaics.decay.df$fdfd.mix.duration = fdfd.decay.df$sim.duration

sensitivity_analysis_gam = function(i){

  parameter.names = c("Heritability",
                      "Male Insecticide Exposure",
                      "Female Insecticide Exposure",
                      "Female Fitness Cost",
                      "Male Fitness Cost",
                      "Intervention Coverage",
                      "Dispersal")

  name.cols = colnames(micromosaics.decay.df)[6:12]

  micromosaics.bio.temp = micromosaics.decay.df|>
    dplyr::select("sim.duration", "rot.duration","fdfd.mix.duration", "hdhd.mix.duration",
                  "cross.resistance", "decay.rate.i", name.cols[i])

  micromosaics.bio.temp$x.parameter = micromosaics.bio.temp[, 7]

  micromosaics.temp.neg = subset(micromosaics.bio.temp, cross.resistance == -0.3)
  micromosaics.temp.zero = subset(micromosaics.bio.temp, cross.resistance == 0)
  micromosaics.temp.pos = subset(micromosaics.bio.temp ,cross.resistance == 0.3)


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
    ylab("Difference From Micro-Mosaics (generations)")+
    facet_grid(. ~ decay.rate.i)+
    xlab(paste0(parameter.names[i]))+
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
    ylab("Difference From Micro-Mosaics (generations)")+
    facet_grid(. ~ decay.rate.i)+
    xlab(paste0(parameter.names[i]))+
    theme_bw()+
    theme(axis.title.y =  element_blank(),
          axis.title.x =  element_text(size = 10),
          axis.text.x = element_text(size = 8, colour = "black", angle = 90),
          axis.text.y = element_text(size = 8, colour = "black"))

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
    ylab("Difference From Micro-Mosaics (generations)")+
    facet_grid(. ~ decay.rate.i)+
    xlab(paste0(parameter.names[i]))+
    theme_bw()+
    theme(axis.title.y =  element_blank(),
          axis.title.x =  element_text(size = 10),
          axis.text.x = element_text(size = 8, colour = "black", angle = 90),
          axis.text.y = element_text(size = 8, colour = "black"))


  return(list(temp.plot.neg, temp.plot.zero, temp.plot.pos))
}


plot.list = list()
for(j in 1:7){

  plot.list[[j]] = sensitivity_analysis_gam(i=j)

}



the.layout = "
ABCDEFG
HIJKLMN
OPQRSTU
"

plot.list[[1]][[1]]+
  plot.list[[2]][[1]]+
  plot.list[[3]][[1]]+
  plot.list[[4]][[1]]+
  plot.list[[5]][[1]]+
  plot.list[[6]][[1]]+
  plot.list[[7]][[1]]+
  plot.list[[1]][[2]]+
  plot.list[[2]][[2]]+
  plot.list[[3]][[2]]+
  plot.list[[4]][[2]]+
  plot.list[[5]][[2]]+
  plot.list[[6]][[2]]+
  plot.list[[7]][[2]]+
  plot.list[[1]][[3]]+
  plot.list[[2]][[3]]+
  plot.list[[3]][[3]]+
  plot.list[[4]][[3]]+
  plot.list[[5]][[3]]+
  plot.list[[6]][[3]]+
  plot.list[[7]][[3]]+
  plot_layout(design = the.layout)

ggsave(
  filename = "chapter6_figure10.jpeg",
  plot = last_plot(),
  scale = 10,
  width = 400,
  height = 200,
  units = "px",
  dpi = 300)




