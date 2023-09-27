length.calibration = rep(c(2, 10, 100, 200, 500, 1000, 2000, 5000, 7000, 10000, 20000), 4)
calibration.value = c()
trait.mean.val = c(rep(0, 11), rep(100, 11), rep(900, 11), rep(3600, 11))


for(i in 1:44){
calibration.value[i] = multiple_gonotrophic_cycles_singles_dispersal_sd_scaled(intervention.trait.mean.i = trait.mean.val[i],
                                                        refugia.trait.mean.i = trait.mean.val[i],
                                                        vector.length = length.calibration[i],
                                                        female.exposure = 0.7,
                                                        exposure.scaling.factor = 10,
                                                        coverage = 0.7,
                                                        dispersal.rate = 0.2,
                                                        male.differential.intervention.i = 0,
                                                        male.differential.refugia.i = 0,
                                                        female.fitness.cost.i = 0,
                                                        heritability.i = 0.3,
                                                        n.cycles = 1,
                                                        half.population.bioassay.survival.resistance = 900,
                                                        michaelis.menten.slope = 1,
                                                        maximum.bioassay.survival.proportion = 1,
                                                        regression.coefficient = 0.48,
                                                        regression.intercept = 0.15,
                                                        current.insecticide.efficacy.i = 1,
                                                        z.sd.intercept = 18,
                                                        z.sd.coefficient = 0.4)[[1]]
}


calibration.df = data.frame(calibration.value, length.calibration, trait.mean.val)

calibration.df$error.difference = c((calibration.df[11, 1] - calibration.df[1:11, 1]) / calibration.df[11, 1] * 100,
                                    (calibration.df[22, 1] - calibration.df[12:22, 1]) / calibration.df[22, 1] * 100,
                                    (calibration.df[33, 1] - calibration.df[23:33, 1]) / calibration.df[33, 1] * 100,
                                    (calibration.df[44, 1] - calibration.df[34:44, 1]) / calibration.df[44, 1] * 100)



# calibration.df$error.difference = (calibration.df[11, 1] - calibration.df$calibration.value)/calibration.df[11, 1] * 100

#value of 1000 looks to be optimal in terms of resolution and computational resources.
ggplot(calibration.df, aes(x=length.calibration, y=error.difference,
                           colour = as.character(trait.mean.val)))+
  geom_point(size =3)+
  geom_line()+
  xlab("Length of the Tracked Vector")+
  ylab("Percentage Error of Calculated Estimate")+
  xlim(0, 5000)+
  theme_bw()+
  facet_wrap(trait.mean.val ~ .)

#value of 250 looks to be optimal in terms of resolution and computational resources.






