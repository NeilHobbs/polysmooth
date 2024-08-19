library(devtools)
load_all()

#What is the implication of deploying pyrethroid-mixtures in sequence for the time to 10% bioassay survival
#for the "novel" insecticide????


#Create the Parameter Space Dataframe for Running the Simulations

df = data.frame(lhs::randomLHS(2000, 5)) #2000 random samples of the 6 input parameters.

parameter_space_smooth = df%>%
  dplyr::rename(Heritability = X1)%>%
  dplyr::rename(`Male Insecticide Exposure` = X2)%>%
  dplyr::rename(`Female Insecticide Exposure` = X3)%>%
  dplyr::rename(`Intervention Coverage` = X4)%>%
  dplyr::rename(Dispersal = X5)%>%
  dplyr::mutate(Heritability = qunif(Heritability, 0.15, 0.3))%>%
  dplyr::mutate(`Male Insecticide Exposure` = qunif(`Male Insecticide Exposure`, 0, 1))%>% #
  dplyr::mutate(`Female Insecticide Exposure` = qunif(`Female Insecticide Exposure`, 0.4, 0.9))%>% #Defaults from Ian
  dplyr::mutate(`Intervention Coverage` = qunif(`Intervention Coverage`, 0.6, 1))%>%
  dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))


parameter_space_smooth = dae::rep.data.frame(parameter_space_smooth, each = 15)
parameter_space_smooth$Female.Fitness.Cost = 0  #remove fitness costs for ease
parameter_space_smooth$Male.Fitness.Cost = 0 #remove fitness costs for ease


parameter_space_smooth$dose.novel.1 = rep(c(1,
                                            1,
                                            1,
                                            0.5,
                                            1,
                                            1,
                                            0.5,
                                            1,
                                            0.5,
                                            1,
                                            0.5,
                                            0.5,
                                            0.5,
                                            0.5,
                                            0.5
), 2000)
parameter_space_smooth$dose.novel.2 = rep(c(1,
                                            0.5,
                                            1,
                                            1,
                                            1,
                                            0.5,
                                            0.5,
                                            0.5,
                                            1,
                                            1,
                                            1,
                                            1,
                                            0.5,
                                            0.5,
                                            1
), 2000)
parameter_space_smooth$dose.pyr.a = rep(c(1,
                                          1,
                                          1,
                                          1,
                                          0.5,
                                          1,
                                          1,
                                          0.5,
                                          1,
                                          0.5,
                                          0.5,
                                          0.5,
                                          1,
                                          0.5,
                                          0.5
), 2000)
parameter_space_smooth$dose.pyr.b = rep(c(1,
                                          1,
                                          0.5,
                                          1,
                                          1,
                                          0.5,
                                          1,
                                          1,
                                          0.5,
                                          0.5,
                                          1,
                                          0.5,
                                          0.5,
                                          1,
                                          0.5
), 2000)



parameter_space_smooth = dae::rep.data.frame(parameter_space_smooth, each = 5)

parameter_space_smooth$initial.pyr.PRS = rep(c(10, 100, 225, 900, 2700), each = 30000)


write.csv(parameter_space_smooth, ".\\parameter.space.seqrot.for.mixtures.csv")


