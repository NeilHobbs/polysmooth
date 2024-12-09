#Impact of Insecticide Decay on the Efficacy Rate of Evolution

insecticide.efficacy = rep(rep(seq(1.2, 0, by = -0.01), 3), 3)
trait.mean.val = rep(c(rep(0, 121), rep(50, 121), rep(100, 121)), 3)
initial.bioassay = rep(c(rep(0, 121), rep(5, 121), rep(10, 121)), 3)
f.exposure = rep(c(0.4, 0.7, 0.9), each = 363)

response.vals = c()

for(i in 1:length(insecticide.efficacy)){
  response.vals[i] = wrapper_breeders_equation_male_female_insecticide_fitness(trait.mean = trait.mean.val[i],
                                                                               female.fitness.cost = 0,
                                                                               male.fitness.cost= 0,
                                                                               female.insecticide.exposure = f.exposure[i],
                                                                               male.insecticide.exposure = 0.5,
                                                                               standard.deviation = 50,
                                                                               vector.length = 10000,
                                                                               maximum.bioassay.survival.proportion = 1,
                                                                               michaelis.menten.slope = 1,
                                                                               half.population.bioassay.survival.resistance = 900,
                                                                               regression.coefficient = 0.48,
                                                                               regression.intercept = 0.15,
                                                                               current.insecticide.efficacy = insecticide.efficacy[i],
                                                                               exposure.scaling.factor = 10,
                                                                               heritability = 0.3)

  print(i)
}



bioassay.change = ((convert_resistance_score_to_bioassay_survival(trait.mean = trait.mean.val + response.vals)) - (convert_resistance_score_to_bioassay_survival(trait.mean = trait.mean.val)))*100

df.insecticide.decay = data.frame(insecticide.efficacy, response.vals, trait.mean.val,
                                  bioassay.change, initial.bioassay, f.exposure)

ggplot(df.insecticide.decay, aes(x=insecticide.efficacy, y=bioassay.change,
                                 group = interaction(initial.bioassay, f.exposure),
                                 colour = interaction(initial.bioassay, f.exposure)))+
  geom_line(size = 2.5)+
  scale_colour_manual(values = c("pink", "lightgreen", "lightblue", "red", "green", "blue", "darkred", "darkgreen", "darkblue"))+
  xlab("Insecticide Efficacy")+
  ylab("Single Generation Change in Bioassay Survival")+
  labs(colour = paste0("Initial \nBioassay \nSurvival (%)"))+
  ggtitle("polysmooth")+
  theme_bw()+
  theme(legend.position = "none")
