#Create parameter space for combinations

coverage.i = c()
coverage.j = c()
coverage.ij = c()

f.encounter.i = c()
f.encounter.j = c()
f.encounter.ij = c()

m.encounter.i = c()
m.encounter.j = c()
m.encounter.ij = c()

for(i in 1:10000){

coverages = round(wakefield::probs(3), 4) #minimise floating points issue
f.encounters = round(wakefield::probs(3), 4)          #minimise floating points issue
m.encounters = round(wakefield::probs(3), 4)

coverage.i[i] = coverages[1]
coverage.j[i] = coverages[2]
coverage.ij[i] = coverages[3]

f.encounter.i[i] = f.encounters[1]
f.encounter.j[i] = f.encounters[2]
f.encounter.ij[i]= f.encounters[3]

m.encounter.i[i] = m.encounters[1]
m.encounter.j[i] = m.encounters[2]
m.encounter.ij[i]= m.encounters[3]
}

coverage.encounter = data.frame(coverage.i,
                                coverage.j,
                                coverage.ij,
                                f.encounter.i,
                                f.encounter.j,
                                f.encounter.ij,
                                m.encounter.i,
                                m.encounter.j,
                                m.encounter.ij)

df = data.frame(lhs::randomLHS(10000, 8))

parameter.space = df|>
  dplyr::rename(Heritability = X1)|>
  dplyr::rename(`Male Insecticide Exposure` = X2)|>
  dplyr::rename(`Female Insecticide Exposure` = X3)|>
  dplyr::rename(`Female Fitness Cost` = X4)|>
  dplyr::rename(`Male Fitness Cost` = X5)|>
  dplyr::rename(`Intervention Coverage` = X6)|>
  dplyr::rename(Dispersal = X7)|>
  dplyr::rename(`Between Gonotrophic Survival` = X8)|>
  dplyr::mutate(Heritability = qunif(Heritability, 0.05, 0.3))|>
  dplyr::mutate(`Male Insecticide Exposure` = qunif(`Male Insecticide Exposure`, 0, 1))|> #
  dplyr::mutate(`Female Insecticide Exposure` = qunif(`Female Insecticide Exposure`, 0.4, 0.9))|> #Defaults from Ian
  dplyr::mutate(`Female Fitness Cost` = qunif(`Female Fitness Cost`, 0.0022 , 0.0322 ))|>
  dplyr::mutate(`Male Fitness Cost` = qunif(`Male Fitness Cost`, 0.0022 , 0.0322 ))|>
  dplyr::mutate(`Intervention Coverage` = qunif(`Intervention Coverage`, 0.5, 0.9))|>
  dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))|>
  dplyr::mutate(`Between Gonotrophic Survival` =  qunif(`Between Gonotrophic Survival`, 0.66, 0.95))


parameter.space = cbind(parameter.space, coverage.encounter)

write.csv(parameter.space, "parameter.space.combinations.csv")







