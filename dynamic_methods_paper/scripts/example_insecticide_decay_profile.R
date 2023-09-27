gens.vector = seq(0, 30)
eff.vector = calculate_current_insecticide_efficacy(generations.since.deployment = gens.vector,
                                                    threshold.generations = 20,
                                                    base.efficacy.decay.rate = 0.015,
                                                    rapid.decay.rate = 0.08,
                                                    initial.insecticide.efficacy = 1)
df.2 = data.frame(gens.vector, eff.vector)

ggplot(df.2, aes(x=gens.vector, y=eff.vector))+
  geom_line(data = df.2[1:21, ], aes(x=gens.vector,
                                                           y=eff.vector),
            colour = "blue",
            size = 3)+
  geom_line(data = df.2[21:31, ], aes(x=gens.vector,
                                                           y=eff.vector),
            colour = "red",
            size = 3)+
  geom_vline(xintercept = 20,
             colour = "#ffd92f",
             size =2,
             linetype="dashed")+
  geom_text(label = paste0("The longevity threshold of\nthe insecticide is reached"),
            size = 6,
            x = 20.2, y=0.3, angle = 270,
            colour = "black")+
  geom_text(label = paste0("On initial deployment\nthe insecticide\ndecays slowly"),
            size = 6,
            x = 10, y=0.7,
            colour = "blue")+
  geom_text(label = paste0("After longevity threshold\nexceeded insecticide\ndecays rapidly"),
            size = 6,
            x = 26, y=0.7,
            colour = "red")+
  xlab("Mosquito Generations Since Deployment")+
  ylab("Insecticide Efficacy")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 18),
        axis.title.x =  element_text(size = 18),
        axis.text.x = element_text(size= 15, colour = "black"),
        axis.text.y = element_text(size = 15, colour = "black"))
