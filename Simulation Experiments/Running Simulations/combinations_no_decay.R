###Coverages:
c.llin = rep(c(rep(c(0, 0, 0, 0, 0.25, 0.25, 0.25, 0.5, 0.5, 0.75), 13), 0.25, 0.5, 0.75), 1)
c.irs = rep(c(rep(c(0, 0.25, 0.5, 0.75, 0, 0.25, 0.5, 0, 0.25, 0), 13), 0.75, 0.5, 0.25), 1)
c.llin.irs = rep(c(rep(c(1, 0.75, 0.5, 0.25, 0.75, 0.5, 0.25,  0.5, 0.25, 0.25), 13), 0, 0, 0), 1)

#Encounters:
e.llin = rep(c(c(rep(0, 10),
                 rep(0.25, 10),
                 rep(0.5, 10),
                 rep(0.75, 10),
                 rep(0, 10),
                 rep(0, 10),
                 rep(0, 10),
                 rep(0.25, 10),
                 rep(0.5, 10),
                 rep(0.25, 10),
                 rep(0.25, 10),
                 rep(0.5, 10),
                 rep(0.75, 10)
), 0, 0, 0), 1)

e.irs = rep(c(c(rep(0, 10),
                rep(0, 10),
                rep(0, 10),
                rep(0, 10),
                rep(0.25, 10),
                rep(0.5, 10),
                rep(0.75, 10),
                rep(0.25, 10),
                rep(0.5, 10),
                rep(0.75, 10),
                rep(0.5, 10),
                rep(0.25, 10),
                rep(0.25, 10), 0, 0, 0)
), 1)

e.llin.irs = rep(c(c(rep(1, 10),
                     rep(0.75, 10),
                     rep(0.5, 10),
                     rep(0.25, 10),
                     rep(0.75, 10),
                     rep(0.5, 10),
                     rep(0.25, 10),
                     rep(0.5, 10),
                     rep(0, 10),
                     rep(0, 10),
                     rep(0.25, 10),
                     rep(0.25, 10),
                     rep(0, 10), 1, 1, 1)
), 1)

dual.encounter = ifelse(e.llin.irs == 1,
                        yes = "complete",
                        no = ifelse(e.llin.irs == 0.75,
                                    yes = "high",
                                    no = ifelse(e.llin.irs == 0.5,
                                                yes = "moderate",
                                                no = ifelse(e.llin.irs == 0.25,
                                                            yes = "low",
                                                            no = "none"))))

dual.coverage = ifelse(c.llin.irs == 1,
                       yes = "complete",
                       no = ifelse(c.llin.irs == 0.75,
                                   yes = "high",
                                   no = ifelse(c.llin.irs == 0.5,
                                               yes = "moderate",
                                               no = ifelse(c.llin.irs == 0.25,
                                                           yes = "low",
                                                           no = "none"))))



#
# check.encounter = c()
# check.coverage = c()
# for(i in 1:133){
#   check.coverage[i] = sum(c.llin[i], c.llin.irs[i], c.irs[i]) == 1
#   check.encounter[i] = sum(e.llin[i], e.llin.irs[i], e.irs[i]) == 1
#
# }
#
# sum(check.coverage) # =133
# sum(check.encounter) # = 133


library(devtools)
load_all()
##read in parameter space data set

parameter_space_smooth = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv")
parameter_space_smooth = parameter_space_smooth%>%
  dplyr::filter(Intervention.Coverage >= 0.5)

#remove fitness costs for simplicity
parameter_space_smooth$Male.Fitness.Cost = 0
parameter_space_smooth$Female.Fitness.Cost = 0



simulation.list = list()
  for(j in 501:750){

    combination.sim.list = list()
    llin.sim.list = list()
    #Run for each coverage + encounter combination:::
    for(i in 1:133){

      combination.sim.list[[i]] = run_simulation_advanced_combinations_simplified(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                                                                  irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                                                                                  number.of.insecticides = 2,
                                                                                  sd.scaled = FALSE, ##TRUE or FALSE
                                                                                  exposure.scaling.factor = 10,
                                                                                  female.fitness.cost = 0,
                                                                                  male.fitness.cost = 0,
                                                                                  female.exposure = parameter_space_smooth$Female.Insecticide.Exposure[j],
                                                                                  male.exposure = parameter_space_smooth$Male.Insecticide.Exposure[j],
                                                                                  heritability = parameter_space_smooth$Heritability[j],
                                                                                  dispersal.rate = parameter_space_smooth$Dispersal[j],
                                                                                  coverage = parameter_space_smooth$Intervention.Coverage[j],
                                                                                  standard.deviation = 50,
                                                                                  vector.length = 200,
                                                                                  maximum.bioassay.survival.proportion = 1,
                                                                                  michaelis.menten.slope = 1,
                                                                                  regression.coefficient = 0.48,
                                                                                  regression.intercept = 0.15,
                                                                                  maximum.generations = 200,
                                                                                  half.population.bioassay.survival.resistance = 900,
                                                                                  withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                                  return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                                  deployment.frequency = 100, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                                  maximum.resistance.value = 25000,
                                                                                  starting.refugia.resistance.score = 0,
                                                                                  starting.intervention.resistance.score = 0,
                                                                                  applied.insecticide.dose = 1,
                                                                                  recommended.insecticide.dose = 1,
                                                                                  threshold.generations = 10,
                                                                                  base.efficacy.decay.rate = 0,
                                                                                  rapid.decay.rate = 0,
                                                                                  deployment.interval.llin = 200, #only for combinations
                                                                                  deployment.interval.irs = 200, #only for combinations
                                                                                  probability.only.i.male = e.llin[i], #only for combinations
                                                                                  probability.only.j.male = e.irs[i], #only for combinations
                                                                                  probability.both.i.j.male = e.llin.irs[i], #only for combinations
                                                                                  probability.only.i.female = e.llin[i], #only for combinations
                                                                                  probability.only.j.female =  e.irs[i], #only for combinations
                                                                                  probability.both.i.j.female = e.llin.irs[i], #only for combinations
                                                                                  n.cycles = 3,
                                                                                  intervention.coverage.1 = c.llin[i],
                                                                                  intervention.coverage.2 = c.irs[i],
                                                                                  intervention.coverage.1.2 = c.llin.irs[i],
                                                                                  z.sd.intercept = 18,
                                                                                  z.sd.coefficient = 0.4,
                                                                                  mixture.strategy = "pyrethroid.plus",
                                                                                  llin.insecticides = 1,
                                                                                  irs.insecticides = 2,
                                                                                  min.cross.selection = 0,
                                                                                  max.cross.selection = 0,
                                                                                  gonotrophic.cycle.length = 3,
                                                                                  natural.daily.survival = 0.8)


      llin.sim.list[[i]] = run_simulation_advanced_combinations_simplified(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                                                           irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                                                                           number.of.insecticides = 2,
                                                                           sd.scaled = FALSE, ##TRUE or FALSE
                                                                           exposure.scaling.factor = 10,
                                                                           female.fitness.cost = 0,
                                                                           male.fitness.cost = 0,
                                                                           female.exposure = parameter_space_smooth$Female.Insecticide.Exposure[j],
                                                                           male.exposure = parameter_space_smooth$Male.Insecticide.Exposure[j],
                                                                           heritability = parameter_space_smooth$Heritability[j],
                                                                           dispersal.rate = parameter_space_smooth$Dispersal[j],
                                                                           coverage = parameter_space_smooth$Intervention.Coverage[j],
                                                                           standard.deviation = 50,
                                                                           vector.length = 200,
                                                                           maximum.bioassay.survival.proportion = 1,
                                                                           michaelis.menten.slope = 1,
                                                                           regression.coefficient = 0.48,
                                                                           regression.intercept = 0.15,
                                                                           maximum.generations = 200,
                                                                           half.population.bioassay.survival.resistance = 900,
                                                                           withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                                           return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                                           deployment.frequency = 100, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                                           maximum.resistance.value = 25000,
                                                                           starting.refugia.resistance.score = 0,
                                                                           starting.intervention.resistance.score = 0,
                                                                           applied.insecticide.dose = c(1, 0),#IRS set to zero so is LLIN Alone
                                                                           recommended.insecticide.dose = 1,
                                                                           threshold.generations = 10,
                                                                           base.efficacy.decay.rate = 0,
                                                                           rapid.decay.rate = 0,
                                                                           deployment.interval.llin = 200, #only for combinations
                                                                           deployment.interval.irs = 200, #only for combinations
                                                                           probability.only.i.male = e.llin[i], #only for combinations
                                                                           probability.only.j.male = e.irs[i], #only for combinations
                                                                           probability.both.i.j.male = e.llin.irs[i], #only for combinations
                                                                           probability.only.i.female = e.llin[i], #only for combinations
                                                                           probability.only.j.female =  e.irs[i], #only for combinations
                                                                           probability.both.i.j.female = e.llin.irs[i], #only for combinations
                                                                           n.cycles = 3,
                                                                           intervention.coverage.1 = c.llin[i],
                                                                           intervention.coverage.2 = c.irs[i],
                                                                           intervention.coverage.1.2 = c.llin.irs[i],
                                                                           z.sd.intercept = 18,
                                                                           z.sd.coefficient = 0.4,
                                                                           mixture.strategy = "pyrethroid.plus",
                                                                           llin.insecticides = 1,
                                                                           irs.insecticides = 2,
                                                                           min.cross.selection = 0,
                                                                           max.cross.selection = 0,
                                                                           gonotrophic.cycle.length = 3,
                                                                           natural.daily.survival = 0.8)

      print(c(j, i))

    }


    llin.combination = c()
    irs.combination= c()
    llin.only= c()
    irs.not.deployed= c()
    #Now get the results at the end of the simulations

    for(i in 1:133){

      llin.combination[i] =  subset(combination.sim.list[[i]], insecticide.tracked == 1&
                                      site == "intervention" &
                                      time.in.generations == 200)$bioassay.survival*100

      irs.combination[i] =  subset(combination.sim.list[[i]], insecticide.tracked == 2&
                                     site == "intervention" &
                                     time.in.generations == 200)$bioassay.survival*100



      llin.only[i] =  subset(llin.sim.list[[i]], insecticide.tracked == 1&
                               site == "intervention" &
                               time.in.generations == 200)$bioassay.survival*100

      irs.not.deployed[i] =  subset(llin.sim.list[[i]], insecticide.tracked == 2&
                                      site == "intervention" &
                                      time.in.generations == 200)$bioassay.survival*100

    }

    #put together in a single dataframe

    temp.df  = data.frame(e.llin, e.irs, e.llin.irs, c.llin, c.llin.irs, c.irs,
                          dual.encounter,
                          dual.coverage,
                          llin.combination,
                          irs.combination,
                          llin.only,
                          irs.not.deployed)



    temp.df$combinationVSllin_LLIN = temp.df$llin.combination - temp.df$llin.only
    temp.df$combinationVSllin_IRS = temp.df$irs.combination - temp.df$irs.not.deployed
    temp.df$female.exposure = parameter_space_smooth$Female.Insecticide.Exposure[j]
    temp.df$male.exposure = parameter_space_smooth$Male.Insecticide.Exposure[j]
    temp.df$heritability = parameter_space_smooth$Heritability[j]
    temp.df$dispersal.rate = parameter_space_smooth$Dispersal[j]
    temp.df$coverage = parameter_space_smooth$Intervention.Coverage[j]


    #export to list
    simulation.list[[j]] = temp.df
  }


# sims1_250 = evalutate_combinations(1, 250)
# sims1_250.df = do.call(rbind, sims1_250)
# write.csv(sims1_250.df, "combinations_1_250.csv")


# sims251_500.df = do.call(rbind, simulation.list )
# write.csv(sims251_500.df, "combinations_251_500 .csv")

# sims501_750.df = do.call(rbind, simulation.list )
# write.csv(sims501_750.df, "combinations_501_750 .csv")
#
#







