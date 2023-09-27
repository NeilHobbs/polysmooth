
##create micro-mosaics parameter space:::

#Create the Parameter Space Dataframe for Running the Simulations

df = data.frame(lhs::randomLHS(5000, 7)) #5000 random samples of the 6 input parameters.


parameter.space = df|>
  dplyr::rename(Heritability = X1)|>
  dplyr::rename(`Male Insecticide Exposure` = X2)|>
  dplyr::rename(`Female Insecticide Exposure` = X3)|>
  dplyr::rename(`Female Fitness Cost` = X4)|>
  dplyr::rename(`Male Fitness Cost` = X5)|>
  dplyr::rename(`Intervention Coverage` = X6)|>
  dplyr::rename(Dispersal = X7)|>
  dplyr::mutate(Heritability = qunif(Heritability, 0.05, 0.3))|>
  dplyr::mutate(`Male Insecticide Exposure` = qunif(`Male Insecticide Exposure`, 0, 1))|> #
  dplyr::mutate(`Female Insecticide Exposure` = qunif(`Female Insecticide Exposure`, 0.4, 0.9))|> #Defaults from Ian
  dplyr::mutate(`Female Fitness Cost` = qunif(`Female Fitness Cost`, 0.04, 0.58))|> #
  dplyr::mutate(`Male Fitness Cost` = qunif(`Male Fitness Cost`, 0.04, 0.58))|> #
  dplyr::mutate(`Intervention Coverage` = qunif(`Intervention Coverage`, 0.5, 0.9))|> #previously identified that if coverage is low all strategies are great...
  dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))

write.csv(parameter.space, ".//parameter.space.micromosaics.csv")









