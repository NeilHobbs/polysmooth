library(devtools)
load_all()

#What is the implication of deploying pyrethroid-mixtures in sequence for the time to 10% bioassay survival
#for the "novel" insecticide????


#Create the Parameter Space Dataframe for Running the Simulations

df = data.frame(lhs::randomLHS(1000, 5)) #5000 random samples of the 6 input parameters.

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


parameter_space_smooth = dae::rep.data.frame(parameter_space_smooth, each = 12)
parameter_space_smooth$Female.Fitness.Cost = 0  #remove fitness costs for ease
parameter_space_smooth$Male.Fitness.Cost = 0 #remove fitness costs for ease

initial.pyr.PRS = rep(rep(c(20, 100, 900, 2700), each = 3), times = 1000)
dose = rep(rep(c(0.5, 0.75, 1), times = 4), times = 1000)

parameter.space = data.frame(parameter_space_smooth,
                             initial.pyr.PRS,
                             dose)

write.csv(parameter.space, ".\\parameter.space.seqrot.for.mixtures.csv")


