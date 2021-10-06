

run_simulation_micromosaic_smooth = function(number.of.insecticides,
                                             maximum.generations,
                                             starting.intervention.resistance.score,
                                             applied.insecticide.dose,
                                             recommended.insecticide.dose,
                                             threshold.generations,
                                             base.efficacy.decay.rate,
                                             rapid.decay.rate,
                                             deployment.interval,
                                             max.cycles,
                                             intervention.coverage.1,
                                             intervention.coverage.2,
                                             standard.deviation,
                                             vector.length,
                                             female.insecticide.exposure,
                                             male.insecticide.exposure,
                                             heritability,
                                             regression.coefficient,
                                             regression.intercept,
                                             exposure.scaling.factor,
                                             male.fitness.cost,
                                             half.population.bioassay.survival.resistance,
                                             michaelis.menten.slope,
                                             maximum.bioassay.survival.proportion){

  #Start by creating an array (calls the array_named function):
  #dimension 1: site = c("refugia", "intervention"), which hold resistance scores
  #Easier to include both, but refugia won't happen if no dispersal
  #dimension 2: insectide to which the resistance intensity corresponds to
  #dimension 3: generation.
  sim.array = create_starting_array(n.insecticides = number.of.insecticides,
                                    maximum.generations = maximum.generations)

  #Set the starting resistance scores for the insecticides:
  sim.array = set_starting_resistance_scores(sim.array = sim.array,
                                             starting.refugia.resistance.score = NA,
                                             starting.intervention.resistance.score = starting.intervention.resistance.score,
                                             number.of.insecticides = number.of.insecticides)

  #Make a vector of the available insecticides@
  available.vector = seq(1, number.of.insecticides, by = 1)#Creates a vector of the insecticides that are available for deployment.
  #At the beginning all insecticides are available for deployment.
  withdrawn.vector = c() #creates an empty vector to hold the withdrawn insecticides.

  #Make a dataframe of the insecticide parameters:
  insecticide.parameters.df = create_insecticide_parameters_dataframe(number.of.insecticides = number.of.insecticides,
                                                                      applied.insecticide.dose = applied.insecticide.dose,
                                                                      recommended.insecticide.dose = recommended.insecticide.dose,
                                                                      threshold.generation = threshold.generations,
                                                                      base.efficacy.decay.rate = base.efficacy.decay.rate,
                                                                      rapid.decay.rate = rapid.decay.rate)



  #The first insecticide deployed is always insecticide 1
  insecticide.efficacy.vector.1 = rep(create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[1,2],
                                                                   recommended.insecticide.dose = insecticide.parameters.df[1,3],
                                                                   threshold.generations = insecticide.parameters.df[1,4],
                                                                   base.efficacy.decay.rate = insecticide.parameters.df[1,5],
                                                                   rapid.decay.rate = insecticide.parameters.df[1,6],
                                                                   deployment.frequency = deployment.interval), (maximum.generations/deployment.interval))


  #The first insecticide deployed is always insecticide 2
  insecticide.efficacy.vector.2 = rep(create_insecticide_efficacy_vector(applied.insecticide.dose = insecticide.parameters.df[2,2],
                                                                     recommended.insecticide.dose = insecticide.parameters.df[2,3],
                                                                     threshold.generations = insecticide.parameters.df[2,4],
                                                                     base.efficacy.decay.rate = insecticide.parameters.df[2,5],
                                                                     rapid.decay.rate = insecticide.parameters.df[2,6],
                                                                     deployment.frequency = deployment.interval), (maximum.generations/deployment.interval))


  for(generation in 2:maximum.generations){
          tracked.response = perform_micromosaic_smooth(max.cycles = max.cycles,
                                                          insecticide.coverage.1 = intervention.coverage.1,
                                                          insecticide.coverage.2 = intervention.coverage.2,
                                                          trait.mean.1 = sim.array['intervention', 1, generation-1],
                                                          trait.mean.2 = sim.array['intervention', 2, generation-1],
                                                          standard.deviation = standard.deviation,
                                                          vector.length = vector.length,
                                                          female.exposure = female.insecticide.exposure,
                                                          male.selection.diff.1 = wrapper_calculate_male_insecticide_fitness_selection_differential(male.trait.mean = sim.array['intervention', 1, generation-1],
                                                                                                                                                    female.insecticide.exposure = female.insecticide.exposure,
                                                                                                                                                    male.insecticide.exposure = male.insecticide.exposure*intervention.coverage.1,#scaled by coverage of insecticide 1
                                                                                                                                                    standard.deviation = standard.deviation,
                                                                                                                                                    male.fitness.cost = male.fitness.cost,
                                                                                                                                                    vector.length = vector.length,
                                                                                                                                                    maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                    michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                    half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                    regression.coefficient = regression.coefficient,
                                                                                                                                                    regression.intercept = regression.intercept,
                                                                                                                                                    current.insecticide.efficacy = insecticide.efficacy.vector.1[generation],
                                                                                                                                                    exposure.scaling.factor),
                                                          male.selection.diff.2 = wrapper_calculate_male_insecticide_fitness_selection_differential(male.trait.mean = sim.array['intervention', 2, generation-1],
                                                                                                                                                    female.insecticide.exposure = female.insecticide.exposure,
                                                                                                                                                    male.insecticide.exposure = male.insecticide.exposure*intervention.coverage.2, #scaled by coverage of insecticide 2
                                                                                                                                                    standard.deviation = standard.deviation,
                                                                                                                                                    male.fitness.cost = male.fitness.cost,
                                                                                                                                                    vector.length = vector.length,
                                                                                                                                                    maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                                                    michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                                                    half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                                                    regression.coefficient = regression.coefficient,
                                                                                                                                                    regression.intercept = regression.intercept,
                                                                                                                                                    current.insecticide.efficacy = insecticide.efficacy.vector.2[generation],
                                                                                                                                                    exposure.scaling.factor = exposure.scaling.factor),
                                                          current.insecticide.efficacy.1 = insecticide.efficacy.vector.1[generation],
                                                          current.insecticide.efficacy.2 = insecticide.efficacy.vector.2[generation],
                                                          regression.coefficient = regression.coefficient,
                                                          regression.intercept = regression.intercept,
                                                          heritability = heritability,
                                                          exposure.scaling.factor = exposure.scaling.factor,
                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                          michaelis.menten.slope = michaelis.menten.slope,
                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion)

          sim.array['intervention', 1, generation] = tracked.response[[1]] + sim.array['intervention', 1, generation-1]
          sim.array['intervention', 2, generation] = tracked.response[[2]] + sim.array['intervention', 2, generation-1]

          sim.array['intervention', 1, generation] = ifelse(sim.array['intervention', 1, generation] < 0,
                                                            yes = 0,
                                                            no = sim.array['intervention', 1, generation])
          sim.array['intervention', 2, generation] = ifelse(sim.array['intervention', 2, generation] < 0,
                                                            yes = 0,
                                                            no = sim.array['intervention', 2, generation])


    }

  return(list(sim.array, insecticide.efficacy.vector.1, insecticide.efficacy.vector.2))
}



# intervention.coverage.1 = rep(seq(1, 0, by = -0.01), 51)
# intervention.coverage.2 = rep(seq(0, 1, by = 0.01), 51)
# female.exposure = rep(seq(0.4, 0.9, by = 0.01), 101)
#
#
# sim.list = list()
# for(i in 1:5151){
#   A = run_simulation_micromosaic_smooth(number.of.insecticides = 2,
#                                   maximum.generations = 200,
#                                   starting.intervention.resistance.score = 0,
#                                   applied.insecticide.dose = 1,
#                                   recommended.insecticide.dose = 1,
#                                   threshold.generations = 15,
#                                   base.efficacy.decay.rate = 0,
#                                   rapid.decay.rate =0,
#                                   deployment.interval = 20,
#                                   max.cycles = 6,
#                                   intervention.coverage.1 = intervention.coverage.1[i],
#                                   intervention.coverage.2 = intervention.coverage.2[i],
#                                   standard.deviation = 20,
#                                   vector.length = 1000,
#                                   female.insecticide.exposure = female.exposure[i],
#                                   male.insecticide.exposure = 1,
#                                   heritability = 0.3,
#                                   regression.coefficient = 0.48,
#                                   regression.intercept = 0.15,
#                                   exposure.scaling.factor = 1,
#                                   male.fitness.cost = 0,
#                                   half.population.bioassay.survival.resistance = 900,
#                                   michaelis.menten.slope = 1,
#                                   maximum.bioassay.survival.proportion = 1)
#
#  B = get_simulation_dataframe(simulation.array = A,
#                          maximum.generations = 200,
#                          number.of.insecticides = 2)
#  B$coverage.1 = intervention.coverage.1[i]
#  B$coverage.2 = intervention.coverage.2[i]
#  B$female.exposure = female.exposure[i]
#
#  B.1= B%>%
#    dplyr::filter(site == "intervention")%>%
#    dplyr::filter(time.in.generations == 200)%>%
#    dplyr::filter(insecticide.tracked == 1)
#
#  B.2 = B%>%
#    dplyr::filter(site == "intervention")%>%
#    dplyr::filter(time.in.generations == 200)%>%
#    dplyr::filter(insecticide.tracked == 2)
#
# B.3 = rbind(B.1, B.2)
#
#  sim.list[[i]] = B.3
# }
#
#
# sim.list[[1]]
#
# df = do.call(rbind, sim.list)
#
# library(ggplot2)
#
# df.1 = df%>%
#   dplyr::filter(insecticide.tracked == 1)
#
# df.2 = df%>%
#   dplyr::filter(insecticide.tracked == 2)
#
#
# df.1$total.resistance = df.1$resistance.score + df.2$resistance.score
#
# df.x.1 = df.1%>%
#   dplyr::filter(female.exposure == 0.9)
#
# df.x.2 = df.2%>%
#   dplyr::filter(female.exposure == 0.9)
#
#
# df.y.1 = df.1%>%
#   dplyr::filter(female.exposure == 0.7)
#
# df.y.2 = df.2%>%
#   dplyr::filter(female.exposure == 0.7)
#
#
# df.z.1 = df.1%>%
#   dplyr::filter(female.exposure == 0.4)
#
# df.z.2 = df.2%>%
#   dplyr::filter(female.exposure == 0.4)
#
#
#
#
#
# plot.a = ggplot(df.x.1, aes(x=coverage.1, y = resistance.score))+
#   geom_point(size = 3, alpha = 0.7,
#              colour = "red")+
#    geom_line(aes(x=coverage.1, y=total.resistance),
#              colour = "purple", size = 3, alpha=0.7)+
#    geom_point(data = df.x.2, aes(x=coverage.1, y=resistance.score),
#              colour = "blue", size = 3, alpha = 0.7)+
#   ylab("Polygenic Resistance Score")+
#   xlab("Proportion Coverage Insecticide 1")+
#   ylim(0, 20)+
#   theme_classic()
#
# plot.b = ggplot(df.y.1, aes(x=coverage.1, y = resistance.score))+
#   geom_point(size = 3, alpha = 0.7,
#              colour = "red")+
#   geom_line(aes(x=coverage.1, y=total.resistance),
#             colour = "purple", size = 3, alpha=0.7)+
#   geom_point(data = df.y.2, aes(x=coverage.1, y=resistance.score),
#              colour = "blue", size = 3, alpha = 0.7)+
#   ylab("Polygenic Resistance Score")+
#   xlab("Proportion Coverage Insecticide 1")+
#   ylim(0, 20)+
#   theme_classic()
#
# plot.c = ggplot(df.z.1, aes(x=coverage.1, y = resistance.score))+
#   geom_point(size = 3, alpha = 0.7,
#              colour = "red")+
#   geom_line(aes(x=coverage.1, y=total.resistance),
#             colour = "purple", size = 3, alpha=0.7)+
#   geom_point(data = df.z.2, aes(x=coverage.1, y=resistance.score),
#              colour = "blue", size = 3, alpha = 0.7)+
#   ylab("Polygenic Resistance Score")+
#   xlab("Proportion Coverage Insecticide 1")+
#   ylim(0, 20)+
#   theme_classic()

