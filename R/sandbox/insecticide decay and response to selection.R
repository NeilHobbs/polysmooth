#Impact of Insecticide Decay on the Efficacy Rate of Evolution

insecticide.efficacy = rep(seq(1.2, 0, by = -0.01), 5)
trait.mean.val = c(rep(0, 121), rep(50, 121), rep(100, 121), rep(900, 121), rep(3600, 121))
initial.bioassay = c(rep(0, 121), rep(5, 121), rep(10, 121), rep(50, 121), rep(80, 121))

response.vals = c()


for(i in 1:length(insecticide.efficacy)){
  response.vals[i] = wrapper_breeders_equation_male_female_insecticide_fitness(trait.mean = trait.mean.val[i],
                                                                               female.fitness.cost = 0,
                                                                               male.fitness.cost= 0,
                                                                               female.insecticide.exposure = 0.7,
                                                                               male.insecticide.exposure = 0.7,
                                                                               standard.deviation = (0.4*trait.mean.val[i]) + 18,
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
                                  bioassay.change, initial.bioassay)

ggplot(df.insecticide.decay, aes(x=insecticide.efficacy, y=bioassay.change,
               colour = as.factor(initial.bioassay)))+
  geom_line(size = 2)+
  xlab("Insecticide Efficacy")+
  ylab("Single Generation Change in Bioassay Survival")+
  labs(colour = paste0("Initial \nBioassay \nSurvival (%)"))+
  ggtitle("polysmooth")+
  theme_bw()+
  theme()
