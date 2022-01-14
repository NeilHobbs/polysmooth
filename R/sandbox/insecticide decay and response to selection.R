#Impact of Insecticide Decay on the Efficacy Rate of Evolution

insecticide.efficacy = rep(seq(1.2, 0, by = -0.01), 3)
trait.mean.val = c(rep(0, 121), rep(50, 121), rep(100, 121))
response.vals = c()


for(i in 1:length(insecticide.efficacy)){
  response.vals[i] = wrapper_breeders_equation_male_female_insecticide_fitness(trait.mean = trait.mean.val[i],
                                                                               female.fitness.cost = 0,
                                                                               male.fitness.cost= 0,
                                                                               female.insecticide.exposure = 0.7,
                                                                               male.insecticide.exposure = 0.7,
                                                                               standard.deviation = 20,
                                                                               vector.length = 10000,
                                                                               maximum.bioassay.survival.proportion = 1,
                                                                               michaelis.menten.slope = 1,
                                                                               half.population.bioassay.survival.resistance = 900,
                                                                               regression.coefficient = 0.48,
                                                                               regression.intercept = 0.15,
                                                                               current.insecticide.efficacy = insecticide.efficacy[i],
                                                                               exposure.scaling.factor = 20,
                                                                               heritability = 0.3)
}


df = data.frame(insecticide.efficacy, response.vals, trait.mean.val)

ggplot(df, aes(x=insecticide.efficacy, y=response.vals,
               colour = as.factor(trait.mean.val)))+
  geom_line(size = 2)+
  xlab("Insecticide Efficacy")+
  ylab("Response to Selection")+
  labs(colour = "Trait Mean")+
  theme_classic()
