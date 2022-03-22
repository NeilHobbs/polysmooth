convert_output_to_dataframe_singles = function(simulation.results,
                                               maximum.generations,
                                               number.of.insecticides,
                                               maximum.bioassay.survival.proportion,
                                               michaelis.menten.slope,
                                               half.population.bioassay.survival.resistance){

  data.list = list()

  sim.duration = length(simulation.results[[2]])

  for(insecticide in 1:number.of.insecticides){

    if(sim.duration >= maximum.generations){
      insecticide.tracked = as.character(rep(insecticide, times = (2 * maximum.generations))) # 2* as refugia and intervention

      generation.sequence = seq(1, maximum.generations, by = 1)
      time.in.generations = rep(generation.sequence, times = 2) # 2* as refugia and intervention

      resistance.score.refugia = simulation.results[[1]]["refugia", insecticide, ]
      resistance.score.refugia = head(resistance.score.refugia, n=maximum.generations)
      resistance.score.intervention = simulation.results[[1]]["intervention", insecticide, ]
      resistance.score.intervention = head(resistance.score.intervention, n=maximum.generations)
      resistance.score = c(resistance.score.refugia, resistance.score.intervention)

      site.refugia = rep("refugia", times = maximum.generations)
      site.intervention = rep("intervention", times = maximum.generations)
      site = c(site.refugia, site.intervention)

      deployed = simulation.results[[2]]
      deployed_temp = head(deployed, n = maximum.generations)
      insecticide.deployed = as.character(rep(deployed_temp, times = 2)) #2 times as refugia and intervention
      bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                        michaelis.menten.slope = michaelis.menten.slope,
                                                                        trait.mean = resistance.score,
                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance)

      efficacy = simulation.results[[3]]
      efficacy.temp = head(efficacy, n = maximum.generations)
      insecticide.efficacy = rep(efficacy.temp, times = 2)


      data.list[[insecticide]]= data.frame(insecticide.tracked,
                                           resistance.score,
                                           bioassay.survival,
                                           site,
                                           time.in.generations,
                                           insecticide.deployed,
                                           insecticide.efficacy)

    } else{          #Does sim.duration-1 as the final deployed.insecticide is NA which is when the simulation stops
      insecticide.tracked = as.character(rep(insecticide, times = (2 * (sim.duration-1)))) # 2* as refugia and intervention
      time.in.generations = rep(seq(1, (sim.duration-1), by = 1), times = 2) # 2* as refugia and intervention

      resistance.score = c(head(simulation.results[[1]]["refugia", insecticide, ], n=(sim.duration-1)), #first is refugia
                           head(simulation.results[[1]]["intervention", insecticide, ], n=(sim.duration-1))) #second is intervention

      site = c(rep("refugia", times =  (sim.duration-1)), #first is refugia
               rep("intervention", times = (sim.duration-1))) #second is intervention

      insecticide.deployed = as.character(rep(head(simulation.results[[2]], n=(sim.duration-1)), times = 2)) #2 times as refugia and intervention

      bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                        michaelis.menten.slope = michaelis.menten.slope,
                                                                        trait.mean = resistance.score,
                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance)

      efficacy = simulation.results[[3]]
      efficacy.temp = head(efficacy, n = sim.duration-1)
      insecticide.efficacy = rep(efficacy.temp, times = 2)

      data.list[[insecticide]]= data.frame(insecticide.tracked,
                                           resistance.score,
                                           bioassay.survival,
                                           site,
                                           time.in.generations,
                                           insecticide.deployed,
                                           insecticide.efficacy)
    }
  }

  final.df = do.call(rbind, data.list)
  return(final.df)
}
