#Impact of mulitple gonotrophic cycles on the expected response to insecticide selection:

#One of the novel aspects of the micro-mosaics methodology is the development of process for allow
  #mulitple gonotrophic cycles into a model of IRM. This is often a process not considered in the literature,
  #partly due to complexity, and partly due to unknown issue as to how important such a process would be.

g.cycles = rep(rep(rep(rep(seq(1, 20, by = 1), 3), 6), 11), 5)
trait.mean = rep(rep(rep(c(rep(0, 20), rep(50, 20), rep(100, 20)), 6), 11), 5)
female.exposure = rep(rep(c(rep(0.4, 60), rep(0.5, 60), rep(0.6, 60), rep(0.7, 60), rep(0.8, 60), rep(0.9, 60)), 11), 5)
current.efficacy = rep(c(rep(0, 360), rep(0.1, 360), rep(0.2, 360), rep(0.3, 360), rep(0.4, 360), rep(0.5, 360),
                    rep(0.6, 360), rep(0.7, 360), rep(0.8, 360), rep(0.9, 360), rep(1, 360)), 5)
male.diff = c(rep(-1, 3960), rep(-0.5, 3960), rep(0, 3960), rep(0.5, 3960), rep(1, 3960))

response.value.1 = c()

for(i in 1:19800){
  response.value.1[i] = perform_multiple_gonotrophic_cycles_smooth(max.cycles = g.cycles[i],
                                                                trait.mean.1 = trait.mean[i],
                                                                standard.deviation = 50,
                                                                vector.length = 10000,
                                                                female.exposure = female.exposure[i],
                                                                male.selection.diff.1 = male.diff[i],
                                                                current.insecticide.efficacy.1 = current.efficacy[i],
                                                                regression.coefficient = 0.48,
                                                                regression.intercept = 0.15,
                                                                heritability.trait.1 = 0.3,
                                                                exposure.scaling.factor = 10,
                                                                half.population.bioassay.survival.resistance = 900,
                                                                michaelis.menten.slope = 1,
                                                                maximum.bioassay.survival.proportion = 1,
                                                                female.natural.survival.probability = 0.9)
  if(i %% 50 == 0){print(i)}


}

library(RColorBrewer)

mulitple.g.df = data.frame(g.cycles, trait.mean, female.exposure, current.efficacy, male.diff, reponse.value, response.value.1)

mulitple.g.df.1 = mulitple.g.df%>%
  dplyr::filter(female.exposure == 0.4)%>%
  dplyr::filter(trait.mean == 0)%>%
  dplyr::filter(male.diff == -0.5)%>%
  dplyr::filter(current.efficacy > 0)

ggplot(mulitple.g.df.1, aes(x=g.cycles, y = response.value.1,
                            colour = as.character(current.efficacy)))+
  geom_line(alpha = 1,
            size = 3)+
  geom_vline(xintercept = 10, linetype = "dashed", size = 2,
             colour = "grey")+
  scale_color_brewer(palette = "RdYlBu",
                     direction = -1)+
  xlab("Number of Gonotrophic Cycles")+
  ylab("Response")+
  guides(colour=guide_legend(title="Insecticide Efficacy"))+
  theme_classic()





perform_multiple_gonotrophic_cycles_smooth(max.cycles = 10,
                                           trait.mean.1 = 100,
                                           standard.deviation = 50,
                                           vector.length = 10000000,
                                           female.exposure = 0.9,
                                           male.selection.diff.1 = 0,
                                           current.insecticide.efficacy.1 = 1,
                                           regression.coefficient = 0.48,
                                           regression.intercept = 0.15,
                                           heritability.trait.1 = 0.3,
                                           exposure.scaling.factor = 10,
                                           half.population.bioassay.survival.resistance = 900,
                                           michaelis.menten.slope = 1,
                                           maximum.bioassay.survival.proportion = 1,
                                           female.natural.survival.probability = 1)

