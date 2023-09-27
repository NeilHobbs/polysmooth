#Agreement between polysmooth and polytruncate when there is cross resistance and dosing::::

#Read in polysmooth results:
mixture.smooth.df = read.csv(".//part.3.mixture.cross.resistance.polysmooth.csv")
rotation.smooth.df = read.csv(".//part.3.rotation.cross.resistance.polysmooth.csv")

rotation.smooth.df = rbind(rotation.smooth.df, rotation.smooth.df, rotation.smooth.df)

mixture.smooth.df$rotation.insecticide.i = rotation.smooth.df$insecticide.i
mixture.smooth.df$rotation.insecticide.j = rotation.smooth.df$insecticide.j


#Read in polytruncate results:
mixture.truncate.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/part.3.mixture.cross.resistance.csv")
rotation.truncate.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/part.3.rotation.cross.resistance.csv")

rotation.truncate.df = rbind(rotation.truncate.df, rotation.truncate.df, rotation.truncate.df)

mixture.truncate.df$rotation.insecticide.i = rotation.truncate.df$insecticide.i
mixture.truncate.df$rotation.insecticide.j = rotation.truncate.df$insecticide.j

rm(rotation.smooth.df, rotation.truncate.df)

########
library(devtools)
load_all()

#convert resistance scores to bioassays
mixture.smooth.df$mixture.i.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     michaelis.menten.slope = 1,
                                                                                     trait.mean = mixture.smooth.df$insecticide.i)


mixture.smooth.df$mixture.j.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     michaelis.menten.slope = 1,
                                                                                     trait.mean = mixture.smooth.df$insecticide.j)


mixture.smooth.df$rotation.i.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     michaelis.menten.slope = 1,
                                                                                     trait.mean = mixture.smooth.df$rotation.insecticide.i)


mixture.smooth.df$rotation.j.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     michaelis.menten.slope = 1,
                                                                                     trait.mean = mixture.smooth.df$rotation.insecticide.j)



mixture.truncate.df$mixture.i.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     michaelis.menten.slope = 1,
                                                                                     trait.mean = mixture.truncate.df$insecticide.i)


mixture.truncate.df$mixture.j.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                     half.population.bioassay.survival.resistance = 900,
                                                                                     michaelis.menten.slope = 1,
                                                                                     trait.mean = mixture.truncate.df$insecticide.j)


mixture.truncate.df$rotation.i.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                      half.population.bioassay.survival.resistance = 900,
                                                                                      michaelis.menten.slope = 1,
                                                                                      trait.mean = mixture.truncate.df$rotation.insecticide.i)


mixture.truncate.df$rotation.j.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                      half.population.bioassay.survival.resistance = 900,
                                                                                      michaelis.menten.slope = 1,
                                                                                      trait.mean = mixture.truncate.df$rotation.insecticide.j)



###totals###
mixture.smooth.df$mixture.total = mixture.smooth.df$mixture.i.bioassay + mixture.smooth.df$mixture.j.bioassay
mixture.smooth.df$rotation.total = mixture.smooth.df$rotation.i.bioassay + mixture.smooth.df$rotation.j.bioassay
mixture.truncate.df$mixture.total = mixture.truncate.df$mixture.i.bioassay + mixture.truncate.df$mixture.j.bioassay
mixture.truncate.df$rotation.total = mixture.truncate.df$rotation.i.bioassay + mixture.truncate.df$rotation.j.bioassay

mixture.smooth.df$outcome = ifelse(mixture.smooth.df$mixture.total < mixture.smooth.df$rotation.total,
                                   yes = "Mixture Better",
                                   no = "Rotation Better")

mixture.truncate.df$outcome = ifelse(mixture.truncate.df$mixture.total < mixture.truncate.df$rotation.total,
                                   yes = "Mixture Better",
                                   no = "Rotation Better")

mixture.smooth.df$compare.outcome = mixture.truncate.df$outcome == mixture.smooth.df$outcome

temp.df = data.frame(table(mixture.smooth.df$compare.outcome, mixture.smooth.df$cross.resistance,
      mixture.smooth.df$dose))

temp.df$perc = temp.df$Freq / 2500 * 100


ggplot(temp.df, aes(x=Var1,
                    y=perc,
                    fill = as.factor(Var2)))+
  geom_bar(position = "dodge", stat = "identity")+
  facet_grid(Var3~.)+
  theme_bw()


ggplot(mixture.smooth.df, aes(x=mixture.total - rotation.total,
                              fill = as.factor(dose)))+
  geom_histogram()+
  facet_grid(cross.resistance ~ .)

ggplot(mixture.truncate.df, aes(x=mixture.total - rotation.total,
                              fill = as.factor(dose)))+
  geom_histogram()+
  facet_grid(cross.resistance ~ .)












