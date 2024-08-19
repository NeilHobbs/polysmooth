decay_profile = function(deployed.efficacy = 1){

generations.vector = seq(0, 30, by = 1)
efficacy.vector.dr1 = c()
efficacy.vector.dr2 = c()
efficacy.vector.dr3 = c()
efficacy.vector.dr4 = c()
efficacy.vector.dr5 = c()
efficacy.vector.dr6 = c()
efficacy.vector.dr7 = c()
efficacy.vector.dr8 = c()
efficacy.vector.dr9 = c()
efficacy.vector.dr10 = c()

for(i in 1:length(generations.vector)){
  efficacy.vector.dr1[i] = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                  threshold.generations = 30,
                                                                  initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                  base.efficacy.decay.rate = 0,
                                                                  rapid.decay.rate = 0)

  efficacy.vector.dr2[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                   threshold.generations = 15,
                                                                   initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                   base.efficacy.decay.rate = 0.015,
                                                                   rapid.decay.rate = 0.08)

  efficacy.vector.dr3[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                   threshold.generations = 15,
                                                                   initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                   base.efficacy.decay.rate = 0.025,
                                                                   rapid.decay.rate = 0.08)

  efficacy.vector.dr4[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                   threshold.generations = 20,
                                                                   initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                   base.efficacy.decay.rate = 0.025,
                                                                   rapid.decay.rate = 0.08)

  efficacy.vector.dr5[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                   threshold.generations = 10,
                                                                   initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                   base.efficacy.decay.rate = 0.025,
                                                                   rapid.decay.rate = 0.08)

  efficacy.vector.dr6[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                   threshold.generations = 15,
                                                                   initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                   base.efficacy.decay.rate = 0.005,
                                                                   rapid.decay.rate = 0.08)

  efficacy.vector.dr7[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                   threshold.generations = 20,
                                                                   initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                   base.efficacy.decay.rate = 0.005,
                                                                   rapid.decay.rate = 0.08)

  efficacy.vector.dr8[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                   threshold.generations = 10,
                                                                   initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                   base.efficacy.decay.rate = 0.005,
                                                                   rapid.decay.rate = 0.08)

  efficacy.vector.dr9[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                   threshold.generations = 10,
                                                                   initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                   base.efficacy.decay.rate = 0.015,
                                                                   rapid.decay.rate = 0.08)

  efficacy.vector.dr10[i]  = calculate_current_insecticide_efficacy(generations.since.deployment = generations.vector[i],
                                                                    threshold.generations = 20,
                                                                    initial.insecticide.efficacy = (deployed.efficacy/1),
                                                                    base.efficacy.decay.rate = 0.015,
                                                                    rapid.decay.rate = 0.08)



}


insecticide.efficacy = c(efficacy.vector.dr2,
                         efficacy.vector.dr3,
                         efficacy.vector.dr4,
                         efficacy.vector.dr5,
                         efficacy.vector.dr6,
                         efficacy.vector.dr7,
                         efficacy.vector.dr8,
                         efficacy.vector.dr9,
                         efficacy.vector.dr10)

mosquito.generation = rep(generations.vector, 9)

threshold.generations = rep(c(15,
                              15,
                              20,
                              10,
                              15,
                              20,
                              10,
                              10,
                              20), each = 31)

base.decay.rate = rep(c(0.015, 0.025, 0.025, 0.025, 0.005, 0.005, 0.005, 0.015, 0.015), each = 31)

base.decay.rate.descriptor = factor(rep(c("Default", "Faster",
                                          "Faster", "Faster",
                                          "Slower", "Slower",
                                          "Slower", "Default", "Default"), each = 31),
                                    levels = c("Slower", "Default", "Faster"))


threshold.generations.descriptor = factor(rep(c("Default",
                                                "Default",
                                                "Later",
                                                "Earlier",
                                                "Default",
                                                "Later",
                                                "Earlier",
                                                "Earlier",
                                                "Later"), each = 31),
                                          levels = c("Earlier", "Default", "Later"))

df = data.frame(mosquito.generation,
                threshold.generations,
                base.decay.rate,
                insecticide.efficacy,
                threshold.generations.descriptor,
                base.decay.rate.descriptor,
                start.efficacy = deployed.efficacy)

return(df)
}

df.1 = decay_profile(1)
df.0.75 = decay_profile(0.75)
df.0.5 = decay_profile(0.5)

df = rbind(df.1, df.0.75, df.0.5)



fd = ggplot(subset(df, threshold.generations.descriptor == "Default" & start.efficacy == 1), aes(x=mosquito.generation, y = insecticide.efficacy))+
  geom_line(aes(colour = base.decay.rate.descriptor),
            linewidth = 1)+

  facet_grid2(base.decay.rate.descriptor ~ .,
              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = c("#fdb462",
                                                       "#8dd3c7",
                                                       "#bc80bd")),
                by_layer_x = FALSE))+
  geom_vline(aes(xintercept = threshold.generations),
             linetype = "dashed")+

  scale_colour_manual(values = c("#fdb462",
                                 "#8dd3c7",
                                 "#bc80bd"
  ))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  xlab("Mosquito Generations Since Insecticide Deployment")+
  ylim(0, 1)+
  ylab(paste0("Killing Efficacy of Insecticide\n Against Fully Susceptible Mosquitoes in Bioassay"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 4),
        plot.margin = margin(0, 0, 0, 0, "pt"))


hd50 = ggplot(subset(df, threshold.generations.descriptor == "Default" & start.efficacy == 0.5), aes(x=mosquito.generation, y = insecticide.efficacy))+
  geom_line(aes(colour = base.decay.rate.descriptor),
            linewidth = 1)+

  facet_grid2(base.decay.rate.descriptor ~ .,
              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = c("#fdb462",
                                                       "#8dd3c7",
                                                       "#bc80bd")),
                by_layer_x = FALSE))+
  geom_vline(aes(xintercept = threshold.generations),
             linetype = "dashed")+

  scale_colour_manual(values = c("#fdb462",
                                 "#8dd3c7",
                                 "#bc80bd"
  ))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  xlab("Mosquito Generations Since Insecticide Deployment")+
  ylim(0, 1)+
  ylab(paste0("Killing Efficacy of Insecticide\n Against Fully Susceptible Mosquitoes in Bioassay"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 4),
        plot.margin = margin(0, 0, 0, 0, "pt"))

hd75 = ggplot(subset(df, threshold.generations.descriptor == "Default" & start.efficacy == 0.75), aes(x=mosquito.generation, y = insecticide.efficacy))+
  geom_line(aes(colour = base.decay.rate.descriptor),
            linewidth = 1)+

  facet_grid2(base.decay.rate.descriptor ~ .,
              strip =   strip_themed(

                # Horizontal strips
                background_y = elem_list_rect(fill = c("#fdb462",
                                                       "#8dd3c7",
                                                       "#bc80bd")),
                by_layer_x = FALSE))+
  geom_vline(aes(xintercept = threshold.generations),
             linetype = "dashed")+

  scale_colour_manual(values = c("#fdb462",
                                 "#8dd3c7",
                                 "#bc80bd"
  ))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  xlab("Mosquito Generations Since Insecticide Deployment")+
  ylim(0, 1)+
  ylab(paste0("Killing Efficacy of Insecticide\n Against Fully Susceptible Mosquitoes in Bioassay"))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 4),
        plot.margin = margin(0, 0, 0, 0, "pt"))


the.layout = "
A#B"

the.layout2 = "
##B"

the.layout3 = "
ABB"


theme_fdfd = theme_gray() +
  theme(plot.background = element_rect(fill = NA, colour = "#8dd3c7", size = 5))

theme_hdhd75 = theme_gray() +
  theme(plot.background = element_rect(fill = NA, colour = "#ffffb3", size = 5))

theme_hdhd50 = theme_gray() +
  theme(plot.background = element_rect(fill = NA, colour = "#bebada", size = 5))

theme_fdhd = theme_gray() +
  theme(plot.background = element_rect(fill = NA, colour = "#fb8072", size = 5))

theme_hdfd = theme_gray() +
  theme(plot.background = element_rect(fill = NA, colour = "#80b1d3", size = 5))

theme_mono = theme_gray() +
  theme(plot.background = element_rect(fill = NA, colour = "grey", size = 5))



fd_fd.plot = wrap_elements(fd + fd + plot_layout(design = the.layout) + plot_annotation(theme = theme_fdfd))

fd_hd.plot =  wrap_elements(fd + hd50 + plot_layout(design = the.layout) + plot_annotation(theme = theme_fdhd))
hd_fd.plot =  wrap_elements(hd50 + fd + plot_layout(design = the.layout) + plot_annotation(theme = theme_hdfd))
hd_hd.50plot =  wrap_elements(hd50 + hd50 + plot_layout(design = the.layout) + plot_annotation(theme = theme_hdhd50))
hd_hd.75plot =  wrap_elements(hd75 + hd75 + plot_layout(design = the.layout) + plot_annotation(theme = theme_hdhd75))

novel.plot = wrap_elements(fd + plot_layout(design = the.layout2) + plot_annotation(theme = theme_mono))
pyrethroid.plot = wrap_elements(fd + plot_layout(design = the.layout3) + plot_annotation(theme = theme_mono))



decay.profiles.plot = fd_fd.plot / fd_hd.plot / hd_fd.plot / hd_hd.50plot / hd_hd.75plot / pyrethroid.plot / novel.plot +
 plot_annotation(title = "Pyrethroid                                 Novel      ") & theme(plot.title = element_text(hjust = 0.5, size = 20))


pyr.res = c("0%", "10%", "50%", "80%")
x.values = rep(1, 4)

resistance.df = data.frame(pyr.res, x.values)

ggplot(resistance.df, aes(x = x.values, y = pyr.res, fill = rev(pyr.res)))+
  geom_tile() +
  scale_fill_viridis_d(option = "plasma", direction = -1)+
  geom_text(aes(label = rev(pyr.res)), size = 12)+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+
  theme_bw()+
  ggtitle("Initial Pyrethroid\nBioassay Survival")+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18))









pyr.res = c("0%", "10%", "50%", "80%")
x.values = rep(1, 4)

resistance.df = data.frame(pyr.res, x.values)

res.plot = ggplot(resistance.df, aes(x = x.values, y = pyr.res, fill = rev(pyr.res)))+
  geom_tile() +
  scale_fill_viridis_d(option = "plasma", direction = -1)+
  geom_text(aes(label = rev(pyr.res)), size = 12)+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+
  theme_bw()+
  ggtitle("Initial\nPyrethroid\nBioassay\nSurvival")+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18))





parameters = c("Heritability\nUniform(0.05 – 0.3)",
               "Female Exposure\nUniform(0.4 – 0.9)",
               "Coverage\nUniform(0.5 – 0.9)",
               "Male Exposure as a \nproportion of\nfemale exposure\nUniform(0 – 1)",
               "Dispersal\n Uniform(0.1 – 0.9)")
x.values = rep(1, 5)


param.df = data.frame(parameters, x.values)

param.plot = ggplot(param.df, aes(x = x.values, y = parameters))+
  geom_tile(colour = "#525252", fill = "white", size = 4) +
  geom_text(aes(label = parameters), size = 6)+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+
  theme_bw()+
  ggtitle("Random\nParameter\nValues")+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18))



the.layout4 = "
A#BBBBCC"


res.plot + decay.profiles.plot + param.plot + plot_layout(design = the.layout4)


ggsave(plot = last_plot(),
       filename = "mixtures_manuscript_scenario1_design.jpeg",
       height = 2000,
       width =1400,
       dpi = 600,
       scale = 5,
       units = "px")








































































































