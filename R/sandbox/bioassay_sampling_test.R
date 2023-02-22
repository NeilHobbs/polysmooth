library(devtools)
load_all()



bioassay_sampling = function(mean.resistance,
                             standard.deviation,
                             number.of.bioassays){

  prs.values = create_normal_distribution(vector.length = 1000,
                                          trait.mean = mean.resistance,
                                          standard.deviation = standard.deviation)

  freq.individuals = calculate_density_of_trait_values(vector.length = 1000,
                                                       trait.mean = mean.resistance,
                                                       standard.deviation = standard.deviation)

  survival.probabilities = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                         half.population.bioassay.survival.resistance = 900,
                                                                         trait.mean = prs.values,
                                                                         michaelis.menten.slope = 1)


  #collect 100 mosquitoes assuming perfect random collections: and divide into 4 bioassays
  bioassay.list = list()
  for(bioassay in 1:number.of.bioassays){
    bioassay.list[[bioassay]] = mean(ifelse(runif(25, 0, 1) <= sample(survival.probabilities, 25, prob = freq.individuals, replace = TRUE) ,
                                       yes = 1,
                                       no = 0))}


  sample.mean = mean(unlist(bioassay.list))

  return(sample.mean)

}


temp.list = list()

n.bioassays = c(1, 4, 8, 20, 50)

for(j in 1:length(n.bioassays)){
measured.values = c()
actual.values = c()
generation = seq(1, 30, 1)
for(i in 1:30){

  actual.values[i] = convert_resistance_score_to_bioassay_survival(trait.mean = 0 + (i*3))

  measured.values[i] = bioassay_sampling(mean.resistance = 0 + (i*3),
                                         standard.deviation = 20,
                                         number.of.bioassays = n.bioassays[j])

}
number.of.bioassays = rep(n.bioassays[j], 30)
temp.df = data.frame(measured.values, generation,
              actual.values, number.of.bioassays)

temp.list[[j]] = temp.df
}

temp.df = do.call(rbind, temp.list)

ggplot(temp.df, aes(x=generation, y = measured.values,
                    group = as.factor(number.of.bioassays)))+
  geom_line(colour = "blue")+
  geom_smooth(method = "gam")+
  geom_line(aes(x=generation, y = actual.values),
            colour = "grey")+
  geom_hline(yintercept = 0.02, linetype = "dashed",
             colour = "orange")+
  geom_hline(yintercept = 0.1, linetype = "dashed",
             colour = "red")+
  xlab("Time in Mosquito Generations")+
  ylab("Bioassay Survival Proportion")+
  facet_wrap(~number.of.bioassays)+
  theme_bw()

#issue is sampling might be limited; such that the individuals
#used in the bioassay are not necessarily representative of the sample





