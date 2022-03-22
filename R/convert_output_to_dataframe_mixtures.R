#'@title Get a dataframe of a mixture simulation
#'
#'@description Helper function to aid in the conversion of the simulation to a dataframe for use in plotting/analysis
#'
#'@param simulation.array = The simulation
#'@param maximum.generation = The maximum number of generations the simulation was set to run for
#'@param number.of.insecticides = The number of insecticides included in the simulation

convert_output_to_dataframe_mixtures = function(simulation.results,
                                                maximum.generations,
                                                number.of.insecticides,
                                                maximum.bioassay.survival.proportion,
                                                michaelis.menten.slope,
                                                half.population.bioassay.survival.resistance){

  data.list = list()


  sim.duration = nrow(simulation.results[[2]])

  for(insecticide in 1:number.of.insecticides){

    if(sim.duration >= maximum.generations){
      insecticide.tracked = as.character(rep(insecticide, times = (2 * maximum.generations))) # 2* as refugia and treatment

      generation.sequence = seq(1, maximum.generations, by = 1)
      time.in.generations = rep(generation.sequence, times = 2) # 2* as refugia and treatment

      resistance.intensity.refugia = simulation.results[[1]]["refugia", insecticide, ]
      resistance.intensity.refugia = head(resistance.intensity.refugia, n=maximum.generations)
      resistance.intensity.treatment = simulation.results[[1]]["intervention", insecticide, ]
      resistance.intensity.treatment = head(resistance.intensity.treatment, n=maximum.generations)
      resistance.score = c(resistance.intensity.refugia, resistance.intensity.treatment)

      site.refugia = rep("refugia", times = maximum.generations)
      site.treatment = rep("intervention", times = maximum.generations)
      site = c(site.refugia, site.treatment)

      deployed = simulation.results[[2]]
      mixture.id = head(deployed$mixture.id, n = maximum.generations)
      mixture.part.1 = head(deployed$mixture.part.1, n = maximum.generations)
      mixture.part.2 = head(deployed$mixture.part.2, n = maximum.generations)

      efficacy.1 = head(deployed$insecticide.efficacy.vector.part.1, n=maximum.generations)
      efficacy.2 = head(deployed$insecticide.efficacy.vector.part.2, n=maximum.generations)

      deployed.mixture.id = as.character(rep(mixture.id, times = 2)) #2 times as refugia and treatment
      deployed.mixture.part.1 = as.character(rep(mixture.part.1, times = 2)) #2 times as refugia and treatment
      deployed.mixture.part.2 = as.character(rep(mixture.part.2, times = 2)) #2 times as refugia and treatment
      insecticide.efficacy.part.1 = rep(efficacy.1, times = 2)
      insecticide.efficacy.part.2 = rep(efficacy.2, times = 2)

      bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                        michaelis.menten.slope = michaelis.menten.slope,
                                                                        trait.mean = resistance.score,
                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance)


      data.list[[insecticide]]= data.frame(insecticide.tracked,
                                           resistance.score,
                                           bioassay.survival,
                                           site,
                                           time.in.generations,
                                           deployed.mixture.id,
                                           deployed.mixture.part.1,
                                           deployed.mixture.part.2,
                                           insecticide.efficacy.part.1,
                                           insecticide.efficacy.part.2)
    } else{          #Does sim.duration-1 as the final deployed.insecticide is NA which is when the simulation stops
      insecticide.tracked = as.character(rep(insecticide, times = (2 * (sim.duration-1)))) # 2* as refugia and treatment
      time.in.generations = rep(seq(1, (sim.duration-1), by = 1), times = 2) # 2* as refugia and treatment

      resistance.score = c(head(simulation.results[[1]]["refugia", insecticide, ], n=(sim.duration-1)), #first is refugia
                               head(simulation.results[[1]]["intervention", insecticide, ], n=(sim.duration-1))) #second is treatment

      site = c(rep("refugia", times =  (sim.duration-1)), #first is refugia
               rep("intervention", times = (sim.duration-1))) #second is treatment

      deployed = simulation.results[[2]]
      mixture.id = head(deployed$mixture.id, n = (sim.duration - 1))
      mixture.part.1 = head(deployed$mixture.part.1, n = (sim.duration - 1))
      mixture.part.2 = head(deployed$mixture.part.2, n = (sim.duration - 1))

      deployed.mixture.id = as.character(rep(mixture.id, times = 2)) #2 times as refugia and treatment
      deployed.mixture.part.1 = as.character(rep(mixture.part.1, times = 2)) #2 times as refugia and treatment
      deployed.mixture.part.2 = as.character(rep(mixture.part.2, times = 2)) #2 times as refugia and treatment

      efficacy.1 = head(deployed$insecticide.efficacy.vector.part.1, n=(sim.duration - 1))
      efficacy.2 = head(deployed$insecticide.efficacy.vector.part.2, n=(sim.duration - 1))
      insecticide.efficacy.part.1 = rep(efficacy.1, times = 2)
      insecticide.efficacy.part.2 = rep(efficacy.2, times = 2)

      bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                        michaelis.menten.slope = michaelis.menten.slope,
                                                                        trait.mean = resistance.score,
                                                                        half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance)




      data.list[[insecticide]]= data.frame(insecticide.tracked,
                                           resistance.score,
                                           bioassay.survival,
                                           site,
                                           time.in.generations,
                                           deployed.mixture.id,
                                           deployed.mixture.part.1,
                                           deployed.mixture.part.2,
                                           insecticide.efficacy.part.1,
                                           insecticide.efficacy.part.2)
    }
  }

  final.df = do.call(rbind, data.list)
  return(final.df)
}
