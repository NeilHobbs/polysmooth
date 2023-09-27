
#Impact of different coverage and conditional encounter values::;

##All the possible coverages -- at 0.25 resolution
c.LLIN = rep(rep(rep(c(1, 0.75, 0.75, 0.5, 0.5, 0.5, 0.25, 0.25, 0.25, 0.25, 0, 0, 0, 0, 0), 15), 4), 3)
c.IRS = rep(rep(rep(c(0, 0.25, 0, 0.5, 0, 0.25, 0.75, 0.5, 0.25, 0, 1, 0, 0.25, 0.5, 0.75), 15), 4), 3)
c.LLIN.IRS = rep(rep(rep(c(0, 0, 0.25, 0, 0.5, 0.25, 0, 0.25, 0.5, 0.75, 0, 1, 0.75, 0.5, 0.25), 15), 4), 3)


#all the possible conditional encounters -- at 0.25 resolution
e.LLIN = rep(rep(rep(c(1, 0.75, 0.75, 0.5, 0.5, 0.5, 0.25, 0.25, 0.25, 0.25, 0, 0, 0, 0, 0), each = 15), 4), 3)
e.IRS = rep(rep(rep(c(0, 0.25, 0, 0.5, 0, 0.25, 0.75, 0.5, 0.25, 0, 1, 0, 0.25, 0.5, 0.75), each = 15), 4), 3)
e.LLIN.IRS = rep(rep(rep(c(0, 0, 0.25, 0, 0.5, 0.25, 0, 0.25, 0.5, 0.75, 0, 1, 0.75, 0.5, 0.25), each =15), 4), 3)

pyr.prs = rep(rep(c(4.5, 100, 225, 900), each = 225), 3)

irs.base.decay = rep(c(0.005, 0.015, 0.025), 900)

f.exposure = 0.7
m.exposure = 0.7
coverage.vals = 0.7
dispersal.vals = 0.3
heritability.vals = 0.3


combination.sim.list = list() #creates empty list to hold simulation data
for(i in 1:2700){

  #run the simulations
  combination.sim.list[[i]] = run_simulation_advanced(irm.deployment.strategy = "combinations", #singles, mixtures, micromosaics, combinations
                                                      irm.switch.strategy = "sequence.irs", #"rotation", "sequence", "insecticide.1"
                                                      number.of.insecticides = 2,
                                                      sd.scaled = TRUE, ##TRUE or FALSE
                                                      exposure.scaling.factor = 10,
                                                      female.fitness.cost = 0,
                                                      male.fitness.cost = 0,
                                                      female.exposure = f.exposure,
                                                      male.exposure = m.exposure,
                                                      heritability = heritability.vals,
                                                      dispersal.rate = dispersal.vals,
                                                      coverage = coverage.vals,
                                                      standard.deviation = 50,
                                                      vector.length = 250,
                                                      maximum.bioassay.survival.proportion = 1,
                                                      michaelis.menten.slope = 1,
                                                      regression.coefficient = 0.48,
                                                      regression.intercept = 0.15,
                                                      maximum.generations = 100, #10 years
                                                      half.population.bioassay.survival.resistance = 900,
                                                      withdrawal.threshold.value = 1, #this is the survival proportion in a bioassay that would withdraw the insecticide from the arsenal
                                                      return.threshold.value = 0.05, #this is the survival proportion in a bioassay that would return insecticide to arsenal
                                                      deployment.frequency = 50, #Number of mosquito generations between choosing insecticides (note, 1 year is 10 generations)
                                                      maximum.resistance.value = 25000,
                                                      starting.refugia.resistance.score = c(pyr.prs[i],0),
                                                      starting.intervention.resistance.score = c(pyr.prs[i],0),
                                                      applied.insecticide.dose = 1,
                                                      recommended.insecticide.dose = 1,
                                                      threshold.generations = c(30, 5),
                                                      base.efficacy.decay.rate = c(0, irs.base.decay[i]),
                                                      rapid.decay.rate = c(0, 0.08),
                                                      deployment.interval.llin = 30, #only for combinations
                                                      deployment.interval.irs = 10, #only for combinations
                                                      probability.only.i.male = e.LLIN[i], #only for combinations
                                                      probability.only.j.male = e.IRS[i], #only for combinations
                                                      probability.both.i.j.male = e.LLIN.IRS[i], #only for combinations
                                                      probability.only.i.female = e.LLIN[i], #only for combinations
                                                      probability.only.j.female = e.IRS[i], #only for combinations
                                                      probability.both.i.j.female = e.LLIN.IRS[i], #only for combinations
                                                      n.cycles = 3,
                                                      intervention.coverage.1 = c.LLIN[i],
                                                      intervention.coverage.2 = c.IRS[i],
                                                      intervention.coverage.1.2 = c.LLIN.IRS[i],
                                                      z.sd.intercept = 18,
                                                      z.sd.coefficient = 0.4,
                                                      mixture.strategy = "pyrethroid.plus",
                                                      llin.insecticides = 1,
                                                      irs.insecticides = 2,
                                                      min.cross.selection = 0,
                                                      max.cross.selection = 0,
                                                      gonotrophic.cycle.length = 3,
                                                      natural.daily.survival = 0.8)

  print(i)#print the progress to make sure things are working/running. Just nice to know how things are doing.
}

end.llin = c()
end.irs = c()
for(i in 1:2700){
  end.llin[i] = subset(combination.sim.list[[i]], insecticide.tracked == 1 &
                         site == "intervention" & time.in.generations == 100)$bioassay.survival

  end.irs[i] = subset(combination.sim.list[[i]], insecticide.tracked == 2 &
                         site == "intervention" & time.in.generations == 100)$bioassay.survival

}


df = data.frame(encounter.LLIN = e.LLIN,
                encounter.IRS = e.IRS,
                encouter.LLIN.IRS = e.LLIN.IRS,
                coverage.LLIN = c.LLIN,
                coverage.IRS = c.IRS,
                coverage.LLIN.IRS =c.LLIN.IRS,
                female.exposure = rep(f.exposure,  2700),
                male.exposure = rep(m.exposure, 2700),
                coverage = rep(coverage.vals,  2700),
                dispersal = rep(dispersal.vals,  2700),
                heritability = rep(heritability.vals, 2700),
                end.irs,
                end.llin,
                pyr.prs,
                irs.base.decay)

ggplot(df, aes(x=coverage.LLIN.IRS,
               y = end.llin))+
  geom_point(colour = "black")+
  xlab("Coverage LLIN and IRS")+
  ylab("End LLIN Bioassay")+
facet_grid(. ~ pyr.prs)+
  ggtitle("Change in Bioassay Survival: LLIN")+
  theme_bw()

ggplot(subset(df,
              coverage.LLIN != 0,
              coverage.IRS != 0), aes(x=end.llin,
               fill = as.factor(encouter.LLIN.IRS)))+
  facet_grid(coverage.LLIN ~ coverage.IRS)+
  geom_histogram()+
  theme_bw()
