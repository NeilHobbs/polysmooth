
deployed.efficacy = 1

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
                base.decay.rate.descriptor)


ggplot(df, aes(x=mosquito.generation, y = insecticide.efficacy))+
geom_line(aes(colour = base.decay.rate.descriptor),
                      linewidth = 2)+

  facet_grid2(threshold.generations.descriptor ~ base.decay.rate.descriptor,
              strip =   strip_themed(

                # Horizontal strips
                background_x = elem_list_rect(fill = c("#fdb462",
                                                       "#8dd3c7",
                                                       "#bc80bd")),
                background_y = elem_list_rect(fill = c("#fde0dd",
                                                     "#fa9fb5",
                                                     "#c51b8a")),
                                              by_layer_x = FALSE))+
  geom_vline(aes(xintercept = threshold.generations),
             linetype = "dashed")+

  scale_colour_manual(values = c("#fdb462",
                                 "#8dd3c7",
                                 "#bc80bd"
  ))+
  xlab("Mosquito Generations Since Insecticide Deployment")+
  ylim(0, 1)+
  ylab(paste0("Killing Efficacy of Insecticide\n Against Fully Susceptible Mosquitoes in Bioassay"))+
  theme_bw()+
  theme(legend.position = "none")



ggsave(
  filename = "mixtures_manuscript_figure1.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 600,
  height = 600,
  units = "px",
  dpi = 600)

