library(devtools)
load_all()
#Impact of different coverage and conditional encounter values::;

parameter.space = data.table::fread(".//parameter.space.combinations.multi.irs.csv")


#First combinations with 1 IRS:
combination.1.irs= list() #creates empty list to hold simulation data
combination.2.irs = list()
combination.3.irs = list()
combination.4.irs = list()
mixtures.full = list()

mixtures.half.75 = list()
mixtures.half.50 = list()

# mixtures.half = list()
# rotations = list()

for(i in 13128:30000){

  # #run the simulations
  # combination.1.irs[[i]] = subset(run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
  #                                                         irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
  #                                                         number.of.insecticides = 2,
  #                                                         sd.scaled = FALSE, ##TRUE or FALSE
  #                                                         exposure.scaling.factor = 10,
  #                                                         female.fitness.cost = 0,
  #                                                         male.fitness.cost = 0,
  #                                                         female.exposure = parameter.space$Female.Insecticide.Exposure[i],
  #                                                         male.exposure = parameter.space$Male.Insecticide.Exposure[i],
  #                                                         heritability = parameter.space$Heritability[i],
  #                                                         dispersal.rate = parameter.space$Dispersal[i],
  #                                                         coverage = parameter.space$Intervention.Coverage[i],
  #                                                         standard.deviation = 50,
  #                                                         vector.length = 250,
  #                                                         maximum.bioassay.survival.proportion = 1,
  #                                                         michaelis.menten.slope = 1,
  #                                                         regression.coefficient = 0.48,
  #                                                         regression.intercept = 0.15,
  #                                                         maximum.generations = 200, #10 years
  #                                                         half.population.bioassay.survival.resistance = 900,
  #                                                         withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
  #                                                         return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
  #                                                         deployment.frequency = 50, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
  #                                                         maximum.resistance.value = 25000,
  #                                                         starting.refugia.resistance.score = 0,
  #                                                         starting.intervention.resistance.score = 0,
  #                                                         applied.insecticide.dose = 1,
  #                                                         recommended.insecticide.dose = 1,
  #                                                         threshold.generations = 10,
  #                                                         base.efficacy.decay.rate = 0,
  #                                                         rapid.decay.rate = 0,
  #                                                         deployment.interval.llin = 200, #only for combinations
  #                                                         deployment.interval.irs = 200, #only for combinations
  #                                                         probability.only.i.male = parameter.space$encounter.llin[i],
  #                                                         probability.only.j.male = parameter.space$encounter.irs[i],
  #                                                         probability.both.i.j.male = parameter.space$encounter.both[i],
  #                                                         probability.only.i.female = parameter.space$encounter.llin[i],
  #                                                         probability.only.j.female = parameter.space$encounter.irs[i],
  #                                                         probability.both.i.j.female = parameter.space$encounter.both[i],
  #                                                         n.cycles = 3,
  #                                                         intervention.coverage.1 = parameter.space$coverages.llin[i],
  #                                                         intervention.coverage.2 = parameter.space$coverages.irs[i],
  #                                                         intervention.coverage.1.2 = parameter.space$coverages.both[i],
  #                                                         z.sd.intercept = 18,
  #                                                         z.sd.coefficient = 0.4,
  #                                                         mixture.strategy = "pyrethroid.plus",
  #                                                         llin.insecticides = 1,
  #                                                         irs.insecticides = 2,
  #                                                         min.cross.selection = 0,
  #                                                         max.cross.selection = 0,
  #                                                         gonotrophic.cycle.length = 3,
  #                                                         natural.daily.survival = 0.8), site == "intervention" & time.in.generations == 200)
  #

  # print("Combi 1")
#   #run the simulations
#   combination.2.irs[[i]] = subset(run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
#                                                           irm.switch.strategy = "rotate.irs", #"rotation", "sequence", "insecticide.1"
#                                                           number.of.insecticides = 3,
#                                                           sd.scaled = FALSE, ##TRUE or FALSE
#                                                           exposure.scaling.factor = 10,
#                                                           female.fitness.cost = 0,
#                                                           male.fitness.cost = 0,
#                                                           female.exposure = parameter.space$Female.Insecticide.Exposure[i],
#                                                           male.exposure = parameter.space$Male.Insecticide.Exposure[i],
#                                                           heritability = parameter.space$Heritability[i],
#                                                           dispersal.rate = parameter.space$Dispersal[i],
#                                                           coverage = parameter.space$Intervention.Coverage[i],
#                                                           standard.deviation = 50,
#                                                           vector.length = 250,
#                                                           maximum.bioassay.survival.proportion = 1,
#                                                           michaelis.menten.slope = 1,
#                                                           regression.coefficient = 0.48,
#                                                           regression.intercept = 0.15,
#                                                           maximum.generations = 200, #10 years
#                                                           half.population.bioassay.survival.resistance = 900,
#                                                           withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                           return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                           deployment.frequency = 50, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                           maximum.resistance.value = 25000,
#                                                           starting.refugia.resistance.score = 0,
#                                                           starting.intervention.resistance.score = 0,
#                                                           applied.insecticide.dose = 1,
#                                                           recommended.insecticide.dose = 1,
#                                                           threshold.generations = 10,
#                                                           base.efficacy.decay.rate = 0,
#                                                           rapid.decay.rate = 0,
#                                                           deployment.interval.llin = 200, #only for combinations
#                                                           deployment.interval.irs = 10, #only for combinations
#                                                           probability.only.i.male = parameter.space$encounter.llin[i],
#                                                           probability.only.j.male = parameter.space$encounter.irs[i],
#                                                           probability.both.i.j.male = parameter.space$encounter.both[i],
#                                                           probability.only.i.female = parameter.space$encounter.llin[i],
#                                                           probability.only.j.female = parameter.space$encounter.irs[i],
#                                                           probability.both.i.j.female = parameter.space$encounter.both[i],
#                                                           n.cycles = 3,
#                                                           intervention.coverage.1 = parameter.space$coverages.llin[i],
#                                                           intervention.coverage.2 = parameter.space$coverages.irs[i],
#                                                           intervention.coverage.1.2 = parameter.space$coverages.both[i],
#                                                           z.sd.intercept = 18,
#                                                           z.sd.coefficient = 0.4,
#                                                           mixture.strategy = "pyrethroid.plus",
#                                                           llin.insecticides = 1,
#                                                           irs.insecticides = c(2, 3),
#                                                           min.cross.selection = 0,
#                                                           max.cross.selection = 0,
#                                                           gonotrophic.cycle.length = 3,
#                                                           natural.daily.survival = 0.8), site == "intervention" & time.in.generations == 200)
#
# # print("Combi 2")
#   #run the simulations
#   combination.3.irs[[i]] = subset(run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
#                                                           irm.switch.strategy = "rotate.irs", #"rotation", "sequence", "insecticide.1"
#                                                           number.of.insecticides = 4,
#                                                           sd.scaled = FALSE, ##TRUE or FALSE
#                                                           exposure.scaling.factor = 10,
#                                                           female.fitness.cost = 0,
#                                                           male.fitness.cost = 0,
#                                                           female.exposure = parameter.space$Female.Insecticide.Exposure[i] - (parameter.space$Female.Insecticide.Exposure[i] * parameter.space$coverages.irs[i]),
#                                                           male.exposure = parameter.space$Male.Insecticide.Exposure[i],
#                                                           heritability = parameter.space$Heritability[i],
#                                                           dispersal.rate = parameter.space$Dispersal[i],
#                                                           coverage = parameter.space$Intervention.Coverage[i],
#                                                           standard.deviation = 50,
#                                                           vector.length = 250,
#                                                           maximum.bioassay.survival.proportion = 1,
#                                                           michaelis.menten.slope = 1,
#                                                           regression.coefficient = 0.48,
#                                                           regression.intercept = 0.15,
#                                                           maximum.generations = 200, #10 years
#                                                           half.population.bioassay.survival.resistance = 900,
#                                                           withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                           return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                           deployment.frequency = 50, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                           maximum.resistance.value = 25000,
#                                                           starting.refugia.resistance.score = 0,
#                                                           starting.intervention.resistance.score = 0,
#                                                           applied.insecticide.dose = 1,
#                                                           recommended.insecticide.dose = 1,
#                                                           threshold.generations = 10,
#                                                           base.efficacy.decay.rate = 0,
#                                                           rapid.decay.rate = 0,
#                                                           deployment.interval.llin = 30, #only for combinations
#                                                           deployment.interval.irs = 10, #only for combinations
#                                                           probability.only.i.male = parameter.space$encounter.llin[i],
#                                                           probability.only.j.male = parameter.space$encounter.irs[i],
#                                                           probability.both.i.j.male = parameter.space$encounter.both[i],
#                                                           probability.only.i.female = parameter.space$encounter.llin[i],
#                                                           probability.only.j.female = parameter.space$encounter.irs[i],
#                                                           probability.both.i.j.female = parameter.space$encounter.both[i],
#                                                           n.cycles = 3,
#                                                           intervention.coverage.1 = parameter.space$coverages.llin[i],
#                                                           intervention.coverage.2 = parameter.space$coverages.irs[i],
#                                                           intervention.coverage.1.2 = parameter.space$coverages.both[i],
#                                                           z.sd.intercept = 18,
#                                                           z.sd.coefficient = 0.4,
#                                                           mixture.strategy = "pyrethroid.plus",
#                                                           llin.insecticides = 1,
#                                                           irs.insecticides = c(2, 3, 4),
#                                                           min.cross.selection = 0,
#                                                           max.cross.selection = 0,
#                                                           gonotrophic.cycle.length = 3,
#                                                           natural.daily.survival = 0.8), site == "intervention" & time.in.generations == 200)
#
# # print("Combi 3")


    # #run the simulations
    # combination.4.irs[[i]] = subset(run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
    #                                                         irm.switch.strategy = "rotate.irs", #"rotation", "sequence", "insecticide.1"
    #                                                         number.of.insecticides = 5,
    #                                                         sd.scaled = FALSE, ##TRUE or FALSE
    #                                                         exposure.scaling.factor = 10,
    #                                                         female.fitness.cost = 0,
    #                                                         male.fitness.cost = 0,
    #                                                         female.exposure = parameter.space$Female.Insecticide.Exposure[i] - (parameter.space$Female.Insecticide.Exposure[i] * parameter.space$coverages.irs[i]),
    #                                                         male.exposure = parameter.space$Male.Insecticide.Exposure[i],
    #                                                         heritability = parameter.space$Heritability[i],
    #                                                         dispersal.rate = parameter.space$Dispersal[i],
    #                                                         coverage = parameter.space$Intervention.Coverage[i],
    #                                                         standard.deviation = 50,
    #                                                         vector.length = 250,
    #                                                         maximum.bioassay.survival.proportion = 1,
    #                                                         michaelis.menten.slope = 1,
    #                                                         regression.coefficient = 0.48,
    #                                                         regression.intercept = 0.15,
    #                                                         maximum.generations = 200, #10 years
    #                                                         half.population.bioassay.survival.resistance = 900,
    #                                                         withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
    #                                                         return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
    #                                                         deployment.frequency = 50, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
    #                                                         maximum.resistance.value = 25000,
    #                                                         starting.refugia.resistance.score = 0,
    #                                                         starting.intervention.resistance.score = 0,
    #                                                         applied.insecticide.dose = 1,
    #                                                         recommended.insecticide.dose = 1,
    #                                                         threshold.generations = 10,
    #                                                         base.efficacy.decay.rate = 0,
    #                                                         rapid.decay.rate = 0,
    #                                                         deployment.interval.llin = 30, #only for combinations
    #                                                         deployment.interval.irs = 10, #only for combinations
    #                                                         probability.only.i.male = parameter.space$encounter.llin[i],
    #                                                         probability.only.j.male = parameter.space$encounter.irs[i],
    #                                                         probability.both.i.j.male = parameter.space$encounter.both[i],
    #                                                         probability.only.i.female = parameter.space$encounter.llin[i],
    #                                                         probability.only.j.female = parameter.space$encounter.irs[i],
    #                                                         probability.both.i.j.female = parameter.space$encounter.both[i],
    #                                                         n.cycles = 3,
    #                                                         intervention.coverage.1 = parameter.space$coverages.llin[i],
    #                                                         intervention.coverage.2 = parameter.space$coverages.irs[i],
    #                                                         intervention.coverage.1.2 = parameter.space$coverages.both[i],
    #                                                         z.sd.intercept = 18,
    #                                                         z.sd.coefficient = 0.4,
    #                                                         mixture.strategy = "pyrethroid.plus",
    #                                                         llin.insecticides = 1,
    #                                                         irs.insecticides = c(2, 3, 4, 5),
    #                                                         min.cross.selection = 0,
    #                                                         max.cross.selection = 0,
    #                                                         gonotrophic.cycle.length = 3,
    #                                                         natural.daily.survival = 0.8), site == "intervention" & time.in.generations == 200)

  # print("Combi 3")


  # #run the simulations
  # mixtures.full[[i]] = subset(run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
  #                                                     irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
  #                                                     number.of.insecticides = 2,
  #                                                     sd.scaled = FALSE, ##TRUE or FALSE
  #                                                     exposure.scaling.factor = 10,
  #                                                     female.fitness.cost = 0,
  #                                                     male.fitness.cost = 0,
  #                                                     female.exposure = parameter.space$Female.Insecticide.Exposure[i] - (parameter.space$Female.Insecticide.Exposure[i] * parameter.space$coverages.irs[i]),
  #                                                     male.exposure = parameter.space$Male.Insecticide.Exposure[i],
  #                                                     heritability = parameter.space$Heritability[i],
  #                                                     dispersal.rate = parameter.space$Dispersal[i],
  #                                                     coverage = parameter.space$Intervention.Coverage[i],
  #                                                     standard.deviation = 50,
  #                                                     vector.length = 250,
  #                                                     maximum.bioassay.survival.proportion = 1,
  #                                                     michaelis.menten.slope = 1,
  #                                                     regression.coefficient = 0.48,
  #                                                     regression.intercept = 0.15,
  #                                                     maximum.generations = 200, #10 years
  #                                                     half.population.bioassay.survival.resistance = 900,
  #                                                     withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
  #                                                     return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
  #                                                     deployment.frequency = 200, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
  #                                                     maximum.resistance.value = 25000,
  #                                                     starting.refugia.resistance.score = 0,
  #                                                     starting.intervention.resistance.score = 0,
  #                                                     applied.insecticide.dose = 1,
  #                                                     recommended.insecticide.dose = 1,
  #                                                     threshold.generations = 10,
  #                                                     base.efficacy.decay.rate = 0,
  #                                                     rapid.decay.rate = 0,
  #                                                     deployment.interval.llin = 200, #only for combinations
  #                                                     deployment.interval.irs = 200, #only for combinations
  #                                                     probability.only.i.male = 0,
  #                                                     probability.only.j.male = 0,
  #                                                     probability.both.i.j.male = 1,
  #                                                     probability.only.i.female = 0,
  #                                                     probability.only.j.female = 0,
  #                                                     probability.both.i.j.female = 1,
  #                                                     n.cycles = 3,
  #                                                     intervention.coverage.1 = 0,
  #                                                     intervention.coverage.2 = 0,
  #                                                     intervention.coverage.1.2 = 1,
  #                                                     z.sd.intercept = 18,
  #                                                     z.sd.coefficient = 0.4,
  #                                                     mixture.strategy = "pyrethroid.plus",
  #                                                     llin.insecticides = 1,
  #                                                     irs.insecticides = 2,
  #                                                     min.cross.selection = 0,
  #                                                     max.cross.selection = 0,
  #                                                     gonotrophic.cycle.length = 3,
  #                                                     natural.daily.survival = 0.8), site == "intervention" & time.in.generations == 200)
  #
  # print("Full mix")

#run the simulations
  mixtures.half.75[[i]] = subset(run_simulation_advanced(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                                      irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                                      number.of.insecticides = 2,
                                                      sd.scaled = FALSE, ##TRUE or FALSE
                                                      exposure.scaling.factor = 10,
                                                      female.fitness.cost = 0,
                                                      male.fitness.cost = 0,
                                                      female.exposure = parameter.space$Female.Insecticide.Exposure[i] - parameter.space$coverages.irs[i],
                                                      male.exposure = parameter.space$Male.Insecticide.Exposure[i],
                                                      heritability = parameter.space$Heritability[i],
                                                      dispersal.rate = parameter.space$Dispersal[i],
                                                      coverage = parameter.space$Intervention.Coverage[i],
                                                      standard.deviation = 50,
                                                      vector.length = 250,
                                                      maximum.bioassay.survival.proportion = 1,
                                                      michaelis.menten.slope = 1,
                                                      regression.coefficient = 0.48,
                                                      regression.intercept = 0.15,
                                                      maximum.generations = 200, #10 years
                                                      half.population.bioassay.survival.resistance = 900,
                                                      withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                      return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                      deployment.frequency = 200, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                      maximum.resistance.value = 25000,
                                                      starting.refugia.resistance.score = 0,
                                                      starting.intervention.resistance.score = 0,
                                                      applied.insecticide.dose = 0.75,
                                                      recommended.insecticide.dose = 1,
                                                      threshold.generations = 10,
                                                      base.efficacy.decay.rate = 0,
                                                      rapid.decay.rate = 0,
                                                      deployment.interval.llin = 30, #only for combinations
                                                      deployment.interval.irs = 10, #only for combinations
                                                      probability.only.i.male = 0,
                                                      probability.only.j.male = 0,
                                                      probability.both.i.j.male = 1,
                                                      probability.only.i.female = 0,
                                                      probability.only.j.female = 0,
                                                      probability.both.i.j.female = 1,
                                                      n.cycles = 3,
                                                      intervention.coverage.1 = 0,
                                                      intervention.coverage.2 = 0,
                                                      intervention.coverage.1.2 = 1,
                                                      z.sd.intercept = 18,
                                                      z.sd.coefficient = 0.4,
                                                      mixture.strategy = "pyrethroid.plus",
                                                      llin.insecticides = 1,
                                                      irs.insecticides = 2,
                                                      min.cross.selection = 0,
                                                      max.cross.selection = 0,
                                                      gonotrophic.cycle.length = 3,
                                                      natural.daily.survival = 0.8), site == "intervention" & time.in.generations == 200)


  mixtures.half.50[[i]] = subset(run_simulation_advanced(irm.deployment.strategy = "mixtures", #singles, mixtures, micromosaics, combinations
                                                         irm.switch.strategy = "sequence", #"rotation", "sequence", "insecticide.1"
                                                         number.of.insecticides = 2,
                                                         sd.scaled = FALSE, ##TRUE or FALSE
                                                         exposure.scaling.factor = 10,
                                                         female.fitness.cost = 0,
                                                         male.fitness.cost = 0,
                                                         female.exposure = parameter.space$Female.Insecticide.Exposure[i] - parameter.space$coverages.irs[i],
                                                         male.exposure = parameter.space$Male.Insecticide.Exposure[i],
                                                         heritability = parameter.space$Heritability[i],
                                                         dispersal.rate = parameter.space$Dispersal[i],
                                                         coverage = parameter.space$Intervention.Coverage[i],
                                                         standard.deviation = 50,
                                                         vector.length = 250,
                                                         maximum.bioassay.survival.proportion = 1,
                                                         michaelis.menten.slope = 1,
                                                         regression.coefficient = 0.48,
                                                         regression.intercept = 0.15,
                                                         maximum.generations = 200, #10 years
                                                         half.population.bioassay.survival.resistance = 900,
                                                         withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                         return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                         deployment.frequency = 200, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                         maximum.resistance.value = 25000,
                                                         starting.refugia.resistance.score = 0,
                                                         starting.intervention.resistance.score = 0,
                                                         applied.insecticide.dose = 0.5,
                                                         recommended.insecticide.dose = 1,
                                                         threshold.generations = 10,
                                                         base.efficacy.decay.rate = 0,
                                                         rapid.decay.rate = 0,
                                                         deployment.interval.llin = 30, #only for combinations
                                                         deployment.interval.irs = 10, #only for combinations
                                                         probability.only.i.male = 0,
                                                         probability.only.j.male = 0,
                                                         probability.both.i.j.male = 1,
                                                         probability.only.i.female = 0,
                                                         probability.only.j.female = 0,
                                                         probability.both.i.j.female = 1,
                                                         n.cycles = 3,
                                                         intervention.coverage.1 = 0,
                                                         intervention.coverage.2 = 0,
                                                         intervention.coverage.1.2 = 1,
                                                         z.sd.intercept = 18,
                                                         z.sd.coefficient = 0.4,
                                                         mixture.strategy = "pyrethroid.plus",
                                                         llin.insecticides = 1,
                                                         irs.insecticides = 2,
                                                         min.cross.selection = 0,
                                                         max.cross.selection = 0,
                                                         gonotrophic.cycle.length = 3,
                                                         natural.daily.survival = 0.8), site == "intervention" & time.in.generations == 200)

#   rotations[[i]] = subset(run_simulation_advanced(irm.deployment.strategy = "singles", #singles, mixtures, micromosaics, combinations
#                                                   irm.switch.strategy = "rotation", #"rotation", "sequence", "insecticide.1"
#                                                   number.of.insecticides = 2,
#                                                   sd.scaled = FALSE, ##TRUE or FALSE
#                                                   exposure.scaling.factor = 10,
#                                                   female.fitness.cost = 0,
#                                                   male.fitness.cost = 0,
#                                                   female.exposure = f.exposure,
#                                                   male.exposure = m.exposure,
#                                                   heritability = heritability.vals,
#                                                   dispersal.rate = dispersal.vals,
#                                                   coverage = coverage.vals,
#                                                   standard.deviation = 50,
#                                                   vector.length = 250,
#                                                   maximum.bioassay.survival.proportion = 1,
#                                                   michaelis.menten.slope = 1,
#                                                   regression.coefficient = 0.48,
#                                                   regression.intercept = 0.15,
#                                                   maximum.generations = 200, #10 years
#                                                   half.population.bioassay.survival.resistance = 900,
#                                                   withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
#                                                   return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
#                                                   deployment.frequency = 10, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
#                                                   maximum.resistance.value = 25000,
#                                                   starting.refugia.resistance.score = 0,
#                                                   starting.intervention.resistance.score = 0,
#                                                   applied.insecticide.dose = 1,
#                                                   recommended.insecticide.dose = 1,
#                                                   threshold.generations = 10,
#                                                   base.efficacy.decay.rate = 0,
#                                                   rapid.decay.rate = 0,
#                                                   deployment.interval.llin = 30, #only for combinations
#                                                   deployment.interval.irs = 10, #only for combinations
#                                                   probability.only.i.male = e.LLIN[i], #only for combinations
#                                                   probability.only.j.male = e.IRS[i], #only for combinations
#                                                   probability.both.i.j.male = e.LLIN.IRS[i], #only for combinations
#                                                   probability.only.i.female = e.LLIN[i], #only for combinations
#                                                   probability.only.j.female = e.IRS[i], #only for combinations
#                                                   probability.both.i.j.female = e.LLIN.IRS[i], #only for combinations
#                                                   n.cycles = 3,
#                                                   intervention.coverage.1 = c.LLIN[i],
#                                                   intervention.coverage.2 = c.IRS[i],
#                                                   intervention.coverage.1.2 = c.LLIN.IRS[i],
#                                                   z.sd.intercept = 18,
#                                                   z.sd.coefficient = 0.4,
#                                                   mixture.strategy = "pyrethroid.plus",
#                                                   llin.insecticides = 1,
#                                                   irs.insecticides = 2,
#                                                   min.cross.selection = 0,
#                                                   max.cross.selection = 0,
#                                                   gonotrophic.cycle.length = 3,
#                                                   natural.daily.survival = 0.8), site == "intervention" & time.in.generations == 200)
#

  print(i)#print the progress to make sure things are working/running. Just nice to know how things are doing.
}

combination.1 = do.call(rbind, combination.1.irs)
combination.2 = do.call(rbind, combination.2.irs)
combination.3 = do.call(rbind, combination.3.irs)
combination.4 = do.call(rbind, combination.4.irs)

mix.full = do.call(rbind, mixtures.full)
rot = do.call(rbind, rotations)

combination.1$strategy= "IRS 1"
combination.2$strategy= "IRS 2"
combination.3$strategy= "IRS 3"
combination.4$strategy = "IRS 4"
mix.half$strategy = "half mix"
mix.full$strategy = "full mix"
rot$strategy = "rotations"

combination.1 = combination.1|>
  dplyr::select("insecticide.tracked", "bioassay.survival")

combination.2 = combination.2|>
  dplyr::select("insecticide.tracked", "bioassay.survival")

mix.half = mix.half|>
  dplyr::select("insecticide.tracked", "bioassay.survival")

mix.full = mix.full|>
  dplyr::select("insecticide.tracked", "bioassay.survival")

rot = rot|>
  dplyr::select("insecticide.tracked", "bioassay.survival")


combination.1.1 = c(subset(combination.1, insecticide.tracked == 1)$bioassay.survival)
combination.1.2 = c(subset(combination.1, insecticide.tracked == 2)$bioassay.survival)

combination.2.1 = c(subset(combination.2, insecticide.tracked == 1)$bioassay.survival)
combination.2.2 = c(subset(combination.2, insecticide.tracked == 2)$bioassay.survival)
combination.2.3 = c(subset(combination.2, insecticide.tracked == 3)$bioassay.survival)

mix.full.1 = c(subset(mix.full, insecticide.tracked == 1)$bioassay.survival)
mix.full.2 = c(subset(mix.full, insecticide.tracked == 2)$bioassay.survival)


the.df = data.frame(combination.1.1,
                    combination.1.2,
                    combination.2.1,
                    combination.2.2,
                    combination.2.3,
                    mix.full.1,
                    mix.full.2,
                    parameter.space)


the.df_1 = data.frame(combination.1.1,
                    combination.1.2,
                    parameter.space)



write.csv(the.df_1, "how.many.irs.is.mixture_1.csv")

combination.3 = combination.3|>
  dplyr::select("insecticide.tracked", "bioassay.survival")

combination.3.1 = c(subset(combination.3, insecticide.tracked == 1)$bioassay.survival)
combination.3.2 = c(subset(combination.3, insecticide.tracked == 2)$bioassay.survival)
combination.3.3 = c(subset(combination.3, insecticide.tracked == 3)$bioassay.survival)
combination.3.4 = c(subset(combination.3, insecticide.tracked == 4)$bioassay.survival)


the.df.1 = data.frame(combination.3.1,
                      combination.3.2,
                      combination.3.3,
                      combination.3.4,
                    parameter.space)

write.csv(the.df.1, "how.many.irs.is.mixture_3.csv")





##########
combination.4 = combination.4|>
  dplyr::select("insecticide.tracked", "bioassay.survival")

combination.4.1 = c(subset(combination.4, insecticide.tracked == 1)$bioassay.survival)
combination.4.2 = c(subset(combination.4, insecticide.tracked == 2)$bioassay.survival)
combination.4.3 = c(subset(combination.4, insecticide.tracked == 3)$bioassay.survival)
combination.4.4 = c(subset(combination.4, insecticide.tracked == 4)$bioassay.survival)
combination.4.5 = c(subset(combination.4, insecticide.tracked == 5)$bioassay.survival)


the.df.1 = data.frame(combination.4.1,
                      combination.4.2,
                      combination.4.3,
                      combination.4.4,
                      combination.4.5,
                      parameter.space)

write.csv(the.df.1, "how.many.irs.is.mixture_4.csv")


#Half dose mixtures
half.mix.75 = do.call(rbind, mixtures.half.75)
half.mix.50 = do.call(rbind, mixtures.half.50)
half.mix.75$strategy = "half mix 50%"
half.mix.50$strategy = "half mix 75%"


half.mix.75 = half.mix.75|>
  dplyr::select("insecticide.tracked", "bioassay.survival")

half.mix.75.1 = c(subset(half.mix.75, insecticide.tracked == 1)$bioassay.survival)
half.mix.75.2 = c(subset(half.mix.75, insecticide.tracked == 2)$bioassay.survival)


half.mix.50 = half.mix.50|>
  dplyr::select("insecticide.tracked", "bioassay.survival")

half.mix.50.1 = c(subset(half.mix.50, insecticide.tracked == 1)$bioassay.survival)
half.mix.50.2  = c(subset(half.mix.50, insecticide.tracked == 2)$bioassay.survival)

half.mix.df = data.frame(half.mix.50.1, half.mix.50.2,
                          half.mix.75.1, half.mix.75.2,
                          parameter.space)



write.csv(half.mix.df, "how.many.irs.is.mixture_half_dose_mixtures.csv")
