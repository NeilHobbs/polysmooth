library(devtools)
load_all()

parameter_space_smooth = read.csv("Simulation Experiments/Setting up Simulations/parameter.space.smooth.csv")
parameter_space_smooth = parameter_space_smooth%>%
  dplyr::filter(Intervention.Coverage >= 0.5)

# set1 = 1:250
# set2 = 251:500
# set3 = 501:750
# set4 = 751:1000
# set5 = 1001:1250
# set6 = 1251:1500
# set7 = 1501:1750
# set8 = 1751:2000
# set9 = 2001:2250
# set10 = 2251:2500



beta_20_function = function(parameter.space.df,
                            min_i,
                            max_i){


  base.decay.1 = rep(0.015, 12)

  base.decay.2 = rep(0.015, 12)

  #threshold gens should be equal; this is more to do with the physical structure of the net etc - so would be the same for both insecticides.
  threshold.gens = rep(15, 12)

  start.resistance.2 = c(rep(0, 3), rep(100, 3), rep(900, 3), rep(3600, 3))
  dose.1 = rep(c(1, 0.75, 0.5), 4)
  dose.2 = rep(c(1, 0.75, 0.5), 4)



  mixture.list = list()
  mixture.pyrethroid = c()
  mixture.novel = c()

  for(j in min_i:max_i){
    for(i in 1:12){

      sim.df = run_simulation_advanced_mixtures_simplified(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                                           irm.switch.strategy = "sequence", #"rotation", "sequence", "novel.sequence"
                                                           mixture.strategy = "mix.sequential.discrete",
                                                           number.of.insecticides = 2,
                                                           sd.scaled = TRUE, ##TRUE or FALSE
                                                           exposure.scaling.factor = 20,
                                                           female.fitness.cost = 0,
                                                           male.fitness.cost = 0,
                                                           female.exposure = parameter.space.df$Female.Insecticide.Exposure[j],
                                                           male.exposure = parameter.space.df$Male.Insecticide.Exposure[j],
                                                           heritability = parameter.space.df$Heritability[j],
                                                           dispersal.rate = parameter.space.df$Dispersal[j],
                                                           coverage = parameter.space.df$Intervention.Coverage[j],
                                                           standard.deviation = 50,
                                                           vector.length = 1000,
                                                           maximum.bioassay.survival.proportion = 1,
                                                           michaelis.menten.slope = 1,
                                                           regression.coefficient = 0.48,
                                                           regression.intercept = 0.15,
                                                           maximum.generations = 200,
                                                           half.population.bioassay.survival.resistance = 900,
                                                           withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                           return.threshold.value = 0.08, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                           deployment.frequency = 30, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                           maximum.resistance.value = 90000,
                                                           starting.refugia.resistance.score = c(0, start.resistance.2[i]),
                                                           starting.intervention.resistance.score = c(0, start.resistance.2[i]),
                                                           applied.insecticide.dose = c(dose.1[i], dose.2[i]),
                                                           recommended.insecticide.dose = 1,
                                                           threshold.generations = threshold.gens[i],
                                                           base.efficacy.decay.rate = c(base.decay.1[i], base.decay.2[i]),
                                                           rapid.decay.rate = 0.08,
                                                           deployment.interval.llin = NA, #only for combinations
                                                           deployment.interval.irs = NA, #only for combinations
                                                           probability.only.i.male = NA, #only for combinations
                                                           probability.only.j.male = NA, #only for combinations
                                                           probability.both.i.j.male = NA, #only for combinations
                                                           probability.only.i.female = NA, #only for combinations
                                                           probability.only.j.female = NA, #only for combinations
                                                           probability.both.i.j.female = NA, #only for combinations
                                                           n.cycles = 1,
                                                           intervention.coverage.1 = NA,
                                                           intervention.coverage.2 = NA,
                                                           intervention.coverage.1.2 = NA,
                                                           z.sd.coefficient = 0.4,
                                                           z.sd.intercept = 18,
                                                           llin.insecticides = NA,
                                                           irs.insecticides = NA,
                                                           min.cross.selection = 0,
                                                           max.cross.selection = 0)

      # outcomes:
      #1. Change resistance "novel"

      mixture.novel[i] = sim.df[[1]]["intervention", 1, 200]

      #2. Change resistance "pyrethroid"

      mixture.pyrethroid[i] = sim.df[[1]]["intervention", 2, 200]

      print(c(i, j))

    }


    ##Now to match up the simulations:::
    mixture.df = data.frame(mixture.novel,
                            mixture.pyrethroid,
                            base.decay.1,
                            base.decay.2,
                            threshold.gens,
                            dose.1,
                            dose.2,
                            start.resistance.2)





    heritability = rep(parameter.space.df$Heritability[j], 12)
    male.exposure = rep(parameter.space.df$Male.Insecticide.Exposure[j], 12)
    female.exposure = rep(parameter.space.df$Female.Insecticide.Exposure[j], 12)
    intervention.coverage = rep(parameter.space.df$Intervention.Coverage[j], 12)
    dispersal = rep(parameter.space.df$Dispersal[j], 12)

    mixture.df = data.frame(mixture.df,
                            heritability,
                            male.exposure,
                            female.exposure,
                            intervention.coverage,
                            dispersal)

    mixture.list[[j]] = mixture.df

    print(c(i, j))

  }
  return(mixture.list)

}

set1 = beta_20_function(parameter.space.df = parameter_space_smooth,
                                               min_i = 1,
                                               max_i = 2500)


set.1 = do.call(rbind, set1)


write.csv(set.1, "beta20_dosing_resistance_decay.csv")


