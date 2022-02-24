
insecticide.efficacy = seq(1, 0, by  = -0.05)

z.0 = c()
z.50 = c()
z.100 = c()
z.900 = c()
for(i in 1:length(insecticide.efficacy)){

z.0[i] = convert_bioassay_survival_to_field_survival(bioassay.survival = 0,
                                            regression.coefficient = 0.48,
                                            regression.intercept = 0.15,
                                            current.insecticide.efficacy = insecticide.efficacy[i])

z.50[i] = convert_bioassay_survival_to_field_survival(bioassay.survival = 0.05,
                                                     regression.coefficient = 0.48,
                                                     regression.intercept = 0.15,
                                                     current.insecticide.efficacy = insecticide.efficacy[i])

z.100[i] = convert_bioassay_survival_to_field_survival(bioassay.survival = 0.1,
                                                     regression.coefficient = 0.48,
                                                     regression.intercept = 0.15,
                                                     current.insecticide.efficacy = insecticide.efficacy[i])

z.900[i] = convert_bioassay_survival_to_field_survival(bioassay.survival = 0.5,
                                                     regression.coefficient = 0.48,
                                                     regression.intercept = 0.15,
                                                     current.insecticide.efficacy = insecticide.efficacy[i])
}


temp.df = data.frame(insecticide.efficacy,
                     z.0, z.50, z.100, z.900)


ggplot(temp.df, aes(x=insecticide.efficacy,
                    y=z.0))+
  geom_line(colour = "blue", size = 2)+
  geom_line(aes(x=insecticide.efficacy,
                y = z.50),
            colour = "red", size = 2)+
  geom_line(aes(x=insecticide.efficacy,
               y=z.100),
            colour = "green", size = 2)+
  geom_line(aes(x=insecticide.efficacy,
                y=z.900),
            colour = "orange", size = 2)+
  xlab("Insecticide Efficacy") +
  ylab("Calculated Field Survival")+
  theme_classic()





