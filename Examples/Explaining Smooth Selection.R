###

z.values = create_normal_distribution(vector.length = 10000,
                                                 trait.mean = 0,
                                                 standard.deviation = 20)

relative.frequency = calculate_density_of_trait_values(vector.length = 10000,
                                                       trait.mean = 0,
                                                       standard.deviation = 20)*1000

df = data.frame(z.values,
                relative.frequency)


#Graph 1: All the mosquitoes
ggplot(df, aes(x=z.values,
               y=relative.frequency))+
  geom_line(size = 3)+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))


#This is then separated into those that go on to encounter the insecticide; and those that
  #do not encounter the insecticide.
ggplot(df, aes(x=z.values,
               y=relative.frequency*0.7))+
  geom_line(size = 3, colour = "red",
            alpha = 0.5)+
  geom_line(aes(x=z.values,
                y=relative.frequency*0.3), colour = "green",
            size = 3, alpha = 0.5)+
  geom_vline(xintercept = 0,
             linetype = "dashed")+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))

##Of those that encounter the insecticide, a proportion will survive and a proportion will die.
  #This will depend on their polygenic resistance score and the "amount" of insecticide they encounter.



ggplot(df, aes(x=z.values,
               y=relative.frequency*0.7))+
  geom_segment(aes(x = z.values, xend = z.values, y = 0, yend = relative.frequency*0.7, colour = z.values))+
  scale_colour_distiller(palette = "YlOrRd")+
  geom_line(colour = "red", size =3,
            alpha = 0.5)+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))+
  theme(legend.position = "none")



surviving.frequency = calculate_density_after_selection(insecticide.exposure  = 0.7,
                                  vector.length = 10000,
                                  trait.mean = 0,
                                  standard.deviation = 20,
                                  maximum.bioassay.survival.proportion = 1,
                                  michaelis.menten.slope = 1,
                                  half.population.bioassay.survival.resistance = 900,
                                  regression.coefficient = 0.48,
                                  regression.intercept = 0.15,
                                  current.insecticide.efficacy = 0.7)*1000



#What would be the relative frequency after exposure
ggplot(df, aes(x=z.values,
               y=surviving.frequency))+
  geom_line(size = 3, colour = "orange",
            alpha = 0.5)+
  geom_vline(xintercept = mean(surviving.frequency*z.values),
                               colour = "orange")+
  geom_vline(xintercept = 0,
             colour = "black",
             linetype = "dashed")+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))


#What are the surviving mosquitoes
ggplot(df, aes(x=z.values,
               y=surviving.frequency))+
  geom_line(size = 3, colour = "orange",
            alpha = 0.5)+
  geom_line(aes(x=z.values,
                y=relative.frequency*0.3), colour = "green",
            size = 3, alpha = 0.5)+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))

#The final population is therefore:
final.population = surviving.frequency+(relative.frequency*0.3)
mean(final.population * z.values)
mean(relative.frequency * z.values)

ggplot(df, aes(x=z.values,
               y=final.population))+
  geom_line(size = 3, colour = "blue",
            alpha = 0.5)+
  geom_vline(xintercept = mean(final.population * z.values),
             colour = "blue")+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))

#Calculate the selection differential:
ggplot(df, aes(x=z.values,
               y=final.population))+
  geom_line(size = 3, colour = "blue",
            alpha = 0.5)+
  geom_vline(xintercept = mean(final.population*z.values),
             colour = "blue")+
  geom_vline(xintercept = 0)+
  ylim(0, 20)+
  xlab("Polygenic Resistance Score")+
  ylab("Relative Frequency in the Population")+
  theme_classic()+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))




