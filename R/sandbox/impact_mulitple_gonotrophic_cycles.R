####Multiple Gonotrophic cycle models:

g.cycles = seq(1, 20, 1)
response.vals.int = c()
response.vals.ref = c()

for(i in 1:length(g.cycles)){

x = multiple_gonotrophic_cycles_singles_dispersal(intervention.trait.mean.i = 0,
                                              refugia.trait.mean.i = 0,
                                              standard.deviation = 50,
                                              vector.length = 250,
                                              female.exposure = 0.7,
                                              exposure.scaling.factor = 10,
                                              coverage = 1,
                                              dispersal.rate = 0.3,
                                              male.differential.intervention.i = wrapper_calculate_male_insecticide_fitness_selection_differential(male.trait.mean = 0,
                                                                                                                                                   female.insecticide.exposure = 0.7,
                                                                                                                                                   male.insecticide.exposure = 0.7,
                                                                                                                                                   standard.deviation = 50,
                                                                                                                                                   male.fitness.cost = 1,
                                                                                                                                                   vector.length = 250,
                                                                                                                                                   maximum.bioassay.survival.proportion = 1,
                                                                                                                                                   michaelis.menten.slope = 1,
                                                                                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                                                                                   regression.coefficient = 0.48,
                                                                                                                                                   regression.intercept = 0.15,
                                                                                                                                                   current.insecticide.efficacy = 1,
                                                                                                                                                   exposure.scaling.factor = 10),
                                              male.differential.refugia.i = wrapper_calculate_male_insecticide_fitness_selection_differential(male.trait.mean = 0,
                                                                                                                                              female.insecticide.exposure = 0.7,
                                                                                                                                              male.insecticide.exposure = 0.7,
                                                                                                                                              standard.deviation = 50,
                                                                                                                                              male.fitness.cost = 1,
                                                                                                                                              vector.length = 250,
                                                                                                                                              maximum.bioassay.survival.proportion = 1,
                                                                                                                                              michaelis.menten.slope = 1,
                                                                                                                                              half.population.bioassay.survival.resistance = 900,
                                                                                                                                              regression.coefficient = 0.48,
                                                                                                                                              regression.intercept = 0.15,
                                                                                                                                              current.insecticide.efficacy = 0,
                                                                                                                                              exposure.scaling.factor = 10),
                                              female.fitness.cost.i = 2,
                                              heritability.i = 0.3,
                                              n.cycles = g.cycles[i],
                                              half.population.bioassay.survival.resistance = 900,
                                              michaelis.menten.slope = 1,
                                              maximum.bioassay.survival.proportion = 1,
                                              regression.coefficient = 0.48,
                                              regression.intercept = 0.15,
                                              current.insecticide.efficacy.i = 1)

response.vals.int[i] = x[[1]]
response.vals.ref[i] = x[[2]]
}


df = data.frame(g.cycles, response.vals.int, response.vals.ref)

ggplot(df, aes(x=g.cycles, y = response.vals.int))+
  geom_line(colour = "red", size = 2)+
  geom_line(aes(x=g.cycles, y=response.vals.ref), colour = "green", size = 2)+
  xlab("Number of Gonotrophic Cycles")+
  ylab("Change in Next Generation (PRS)")+
  theme_classic()



