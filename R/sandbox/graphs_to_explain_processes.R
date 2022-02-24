##Visualising Process
library(ggforce)
library(patchwork)

prs.values = seq(0, 10000, by = 1)
bioassay.survival = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                  trait.mean = prs.values,
                                                                  michaelis.menten.slope = 1,
                                                                  half.population.bioassay.survival.resistance = 900)
field.survival = convert_bioassay_survival_to_field_survival(bioassay.survival = bioassay.survival,
                                                             regression.coefficient = 0.48,
                                                             regression.intercept = 0.15,
                                                             current.insecticide.efficacy = 1)

df = data.frame(prs.values, bioassay.survival, field.survival)

#Figure 1:: PRS to Bioassay Survival
ggplot(df, aes(x=prs.values, y=bioassay.survival*100)) +
  geom_point() +
  geom_segment(x=100, xend = 0, y = 10, yend=10,
               linetype = "dashed", colour = "orange")+
  geom_segment(x=100, xend=100, y=0, yend=10,
               linetype = "dashed", colour = "orange")+
  geom_segment(x=900, xend=900, y=0, yend=50,
               linetype = "dashed", colour = "green")+
  geom_segment(x=900, xend = 0, y = 50, yend=50,
               linetype = "dashed", colour = "green")+
  xlab("Polygenic Resistance Score")+
  ylab("Bioassay Survival (%)")+
  facet_zoom(xlim = c(0, 900),
             ylim=c(0, 50),
             horizontal = FALSE)+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 15),
        axis.title.x =  element_text(size = 20))

#Figure 2:: Bioassay Survival to Field Survival
fig.2a = ggplot(df, aes(y=field.survival*100, x=bioassay.survival*100))+
  geom_point(colour = "#1c9099")+
  xlab("Bioassay Survival (%)")+
  ylab("Field Survival (%)")+
  ylim(0, 60)+
  ggtitle("A")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 15),
        axis.title.x =  element_text(size = 15))

fig.2b = ggplot(df, aes(y=field.survival*100, x=prs.values))+
  geom_point(colour = "#66c2a5")+
  xlab("Polygenic Resistance Score")+
  ylab("Field Survival (%)")+
  ylim(0, 60)+
  ggtitle("B")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 15),
        axis.title.x =  element_text(size = 15))

fig.2a + fig.2b


#Figure 3:: Example of Insecticide Efficacy and Decay
gens.vector = seq(0, 30)
eff.vector = calculate_current_insecticide_efficacy(generations.since.deployment = gens.vector,
                                       threshold.generations = 20,
                                       base.efficacy.decay.rate = 0.015,
                                       rapid.decay.rate = 0.08,
                                       initial.insecticide.efficacy = 1)
df.2 = data.frame(gens.vector, eff.vector)

ggplot(df.2, aes(x=gens.vector, y=eff.vector))+
  geom_rect(xmin=0,
            xmax=20,
            ymin=0,
            ymax=1, alpha=0.1,
            fill = "#9ecae1") +
  geom_rect(xmin=20,
            xmax=30,
            ymin=0,
            ymax=1, alpha=0.1,
            fill = "#fc9272") +
  geom_vline(xintercept = 20,
             colour = "#ffd92f",
             size =2,
             linetype="dashed")+
  geom_line(size = 3)+
  xlab("Mosquito Generations Since Deployment")+
  ylab("Insecticide Efficacy")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 15),
        axis.title.x =  element_text(size = 15))






##Figure 4:: Insecticide Efficacy, PRS and Field Survival
eff.values = seq(0, 2, 0.01)

survival.0 = convert_bioassay_survival_to_field_survival(bioassay.survival = 0,
                                                         regression.coefficient = 0.48,
                                                         regression.intercept = 0.15,
                                                         current.insecticide.efficacy = eff.values)

survival.0 = convert_bioassay_survival_to_field_survival(bioassay.survival = 0.05,
                                                         regression.coefficient = 0.48,
                                                         regression.intercept = 0.15,
                                                         current.insecticide.efficacy = eff.values)


survival.100 = convert_bioassay_survival_to_field_survival(bioassay.survival = 0.1,
                                                         regression.coefficient = 0.48,
                                                         regression.intercept = 0.15,
                                                         current.insecticide.efficacy = eff.values)

survival.900 = convert_bioassay_survival_to_field_survival(bioassay.survival = 0.5,
                                                         regression.coefficient = 0.48,
                                                         regression.intercept = 0.15,
                                                         current.insecticide.efficacy = eff.values)

survival.3600 = convert_bioassay_survival_to_field_survival(bioassay.survival = 0.8,
                                                         regression.coefficient = 0.48,
                                                         regression.intercept = 0.15,
                                                         current.insecticide.efficacy = eff.values)

survival.8100 = convert_bioassay_survival_to_field_survival(bioassay.survival = 0.9,
                                                            regression.coefficient = 0.48,
                                                            regression.intercept = 0.15,
                                                            current.insecticide.efficacy = eff.values)


df.3 = data.frame(eff.values, survival.0, survival.100, survival.900, survival.3600, survival.8100)


ggplot(df.3, aes(x=eff.values, y=survival.0*100))+
  geom_rect(xmin=1,
            xmax=2,
            ymin=0,
            ymax=100, alpha=0.1,
            fill = "#fbb4ae") +
  geom_line(size =3,
            colour = "#1f78b4")+ #z=0=blue
  geom_line(aes(x=eff.values, y=survival.100*100),
            size= 3,
            colour = "#33a02c")+ #z=100=green
  geom_line(aes(x=eff.values, y=survival.900*100),
            size =3,
            colour = "#e31a1c")+ #z=900=red
  geom_line(aes(x=eff.values, y=survival.3600*100),
            size = 3,
            colour = "#ff7f00")+ #z=3600 = orange
  geom_line(aes(x=eff.values, y=survival.8100*100),
            size = 3,
            colour = "#984ea3")+ #z=8100 = purpl
  geom_vline(xintercept = 1,
             linetype = "dashed",
             colour = "black",
             size = 1.5)+
  geom_vline(xintercept = 0.5,
             linetype = "dashed",
             colour = "grey",
             size = 1.5)+
  xlab("Insecticide Efficacy")+
  ylab("Field Survival (%)")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 15),
        axis.title.x =  element_text(size = 15))



##Figure 5:: Truncation Selection

norm.dist = create_normal_distribution(vector.length = 100000,
                           trait.mean = 100,
                           standard.deviation = 50)
rel.freq = calculate_density_of_trait_values(vector.length = 100000,
                                             trait.mean = 100,
                                             standard.deviation = 50)

df.4 = data.frame(norm.dist, rel.freq)
df.4.a = df.4%>%
  dplyr::filter(norm.dist >= 160)


ggplot(df.4, aes(x=norm.dist, y=rel.freq))+
  geom_area(fill = "#b2182b")+
  geom_area(data=df.4.a, aes(x=norm.dist,
                                y=rel.freq),
            fill = "#2166ac")+
  geom_vline(xintercept = 160,
             linetype = "dashed",
             size = 2)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())+
  theme(axis.title.y =  element_text(size = 15),
        axis.title.x =  element_text(size = 15))


#Figure 6:: Smooth Selection
ggplot(df.4, aes(x=norm.dist, y=rel.freq))+
  geom_segment(aes(xend=norm.dist, yend=0, colour = norm.dist),
               size = 0) +
  geom_line()+
  scale_colour_gradient(low = "white",
                        high = "#2166ac")+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        axis.title.y =  element_text(size = 15),
        axis.title.x =  element_text(size = 15))


plot_Dzi_explanation = function(){
  norm.dist = create_normal_distribution(vector.length = 100000,
                                         trait.mean = 100,
                                         standard.deviation = 50)

  rel.freq = calculate_density_of_trait_values(vector.length = 100000,
                                               trait.mean = 100,
                                               standard.deviation = 50)

  temp.df = data.frame(norm.dist, rel.freq)
  temp.df.1 = temp.df%>%
    dplyr::filter(norm.dist <= 85 &
                    norm.dist >= 83)


  final.plot = ggplot(temp.df, aes(x=norm.dist))+
    geom_histogram(binwidth = 2,
                   colour = "#0570b0",
                   fill ="#023858",
                   size = 0.5,
                   alpha = 0.2)+
    geom_histogram(data=temp.df.1, aes(x=norm.dist),
                   binwidth = 2,
                   colour = "red",
                   fill ="#ffff33",
                   size = 0.5)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency")+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           axis.title.y =  element_text(size = 15),
           axis.title.x =  element_text(size = 15),
           legend.position = "none")

  return(final.plot)
}

plot_Dzi_explanation()

##Figure 7:: Flow Diagram of Truncation Selection::
norm.dist = create_normal_distribution(vector.length = 1000,
                                       trait.mean = 100,
                                       standard.deviation = 50)
rel.freq = calculate_density_of_trait_values(vector.length = 1000,
                                             trait.mean = 100,
                                             standard.deviation = 50)

df.5 = data.frame(norm.dist, rel.freq)
#Population emerges with a mean PRS z::

fig.7.a = ggplot(df.5, aes(x=norm.dist, y=rel.freq))+
  geom_area(fill = "#b3b3b3")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
  ggtitle("1)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")

#Some will go on to avoid the insecticide, there mean z will not change
fig.7.b = ggplot(df.5, aes(x=norm.dist, y=rel.freq*0.3))+
  geom_area(fill = "#b2df8a")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
  ggtitle("2)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")



#Other propotion will encounter the insecticide::
fig.7.c = ggplot(df.5, aes(x=norm.dist, y=rel.freq*0.7))+
  geom_area(fill = "#b3b3b3")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
  ggtitle("3)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")

#Some will die and some will survive::
df.5.a = df.5%>%
  dplyr::filter(norm.dist >= 160)


fig.7.d = ggplot(df.5, aes(x=norm.dist, y=rel.freq*0.7))+
  geom_area(fill = "#b2182b")+
  geom_area(data=df.5.a, aes(x=norm.dist,
                             y=rel.freq*0.7),
            fill = "#2166ac")+
  geom_vline(xintercept = 160,
             linetype = "dashed",
             size = 2)+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("4)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

fig.7.e = ggplot(df.5, aes(x=norm.dist, y=rel.freq*0.7))+
  geom_area(fill="white", alpha = 0)+
  geom_area(data=df.5.a, aes(x=norm.dist,
                             y=rel.freq*0.7),
            fill = "#2166ac")+
  geom_line(data=df.5.a, aes(x=norm.dist,
                             y=rel.freq*0.7))+
  geom_vline(xintercept = 180,
             linetype = "dashed",
             colour = "purple")+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("5)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

#combine together::

unselected = rel.freq*0.3
selected = rel.freq*0.7

df.5.b = data.frame(unselected, selected, norm.dist)
df.5.b$selected = ifelse(df.5.b$norm.dist >= 160,
                         yes = df.5.b$selected,
                         no = 0)


df.5.b$total = df.5.b$unselected + df.5.b$selected

fig.7.f = ggplot(df.5.b, aes(x=norm.dist, y=total))+
  geom_area(fill = "#1b9e77")+
  geom_line()+
  geom_vline(xintercept = 140,
             linetype = "dashed",
             colour = "orange")+
  geom_vline(xintercept = 100,
             linetype = "dashed")+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("6)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

#Combine into single plot:::
plot.layout = "
A#B#F
#CDE#"


fig.7.a + fig.7.b + fig.7.c + fig.7.d + fig.7.e + fig.7.f + plot_layout(design = plot.layout)


##Figure 8:: Flow Diagram of Smooth Selection::

norm.dist = create_normal_distribution(vector.length = 1000,
                                       trait.mean = 100,
                                       standard.deviation = 50)

rel.freq = calculate_density_of_trait_values(vector.length = 1000,
                                             trait.mean = 100,
                                             standard.deviation = 50)

df.6 = data.frame(norm.dist, rel.freq)
#Population emerges with a mean PRS z::

fig.8.a = ggplot(df.6, aes(x=norm.dist, y=rel.freq))+
  geom_area(fill = "#b3b3b3")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
  ggtitle("1)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")

#Some will go on to avoid the insecticide, there mean z will not change
fig.8.b = ggplot(df.5, aes(x=norm.dist, y=rel.freq*0.3))+
  geom_area(fill = "#b2df8a")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
  ggtitle("2)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")


#Other propotion will encounter the insecticide::
fig.8.c = ggplot(df.6, aes(x=norm.dist, y=rel.freq*0.7))+
  geom_area(fill = "#b3b3b3")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
  ggtitle("3)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")

#These indivuals will surive depending upon their PRS:
norm.dist = create_normal_distribution(vector.length = 100000,
                                       trait.mean = 100,
                                       standard.deviation = 50)

rel.freq = calculate_density_of_trait_values(vector.length = 100000,
                                             trait.mean = 100,
                                             standard.deviation = 50)

df.6.a = data.frame(norm.dist, rel.freq)

fig.8.d = ggplot(df.6.a, aes(x=norm.dist, y=rel.freq*0.7))+
  geom_segment(aes(xend=norm.dist, yend=0, colour = norm.dist),
               size = 0) +
  geom_line()+
  scale_colour_gradient(low = "white",
                        high = "#2166ac")+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ylim(0, 0.008)+
  ggtitle("4)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")

df.6.a$survival.vals = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = norm.dist,
                                                                                                                              michaelis.menten.slope = 1,
                                                                                                                              maximum.bioassay.survival.proportion = 1,
                                                                                                                              half.population.bioassay.survival.resistance = 900),
                                                            regression.coefficient = 0.48,
                                                            regression.intercept = 0.15,
                                                            current.insecticide.efficacy  = 1)

fig.8.e = ggplot(df.6, aes(x=norm.dist, y=rel.freq*0.7*survival.vals))+
  geom_area(fill = "#2166ac") +
  geom_line()+
  geom_vline(xintercept = 130,
             linetype = "dashed",
             colour = "purple")+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ylim(0, 0.008)+
  ggtitle("5)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")


df.6$survival.vals = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = df.6$norm.dist,
                                                                                                                              michaelis.menten.slope = 1,
                                                                                                                              maximum.bioassay.survival.proportion = 1,
                                                                                                                             half.population.bioassay.survival.resistance = 900),
                                                            regression.coefficient = 0.48,
                                                            regression.intercept = 0.15,
                                                            current.insecticide.efficacy  = 1)


df.6$unselected = df.6$rel.freq*0.3
df.6$selected = df.6$rel.freq*0.7*df.6$survival.vals


df.6$total = df.6$unselected + df.6$selected




fig.8.f = ggplot(df.6, aes(x=norm.dist, y=total))+
  geom_area(fill = "#1b9e77")+
  geom_line()+
  geom_vline(xintercept = 110,
             linetype = "dashed",
             colour = "orange")+
  geom_vline(xintercept = 100,
             linetype = "dashed")+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("6)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

#Combine into single plot:::
plot.layout = "
A#B#F
#CDE#"


fig.8.a + fig.8.b + fig.8.c + fig.8.d + fig.8.e + fig.8.f + plot_layout(design = plot.layout)

##Figure 9:: Disersal Diagram

norm.dist.int = create_normal_distribution(vector.length = 1000,
                                       trait.mean = 100,
                                       standard.deviation = 50)

rel.freq.int = calculate_density_of_trait_values(vector.length = 1000,
                                             trait.mean = 100,
                                             standard.deviation = 50) * 0.7


norm.dist.ref = create_normal_distribution(vector.length = 1000,
                                           trait.mean = 80,
                                           standard.deviation = 50)

rel.freq.ref = calculate_density_of_trait_values(vector.length = 1000,
                                                 trait.mean = 80,
                                                 standard.deviation = 50) * 0.3


df.dispersal = data.frame(norm.dist.int, rel.freq.int, norm.dist.ref, rel.freq.ref)

#Before Disersal
fig.9a = ggplot(df.dispersal, aes(x=norm.dist.int, y=rel.freq.int))+
  geom_line(colour = "#762a83",
            size = 3)+
  geom_vline(xintercept = 100,
             linetype = "dashed",
             colour = "#762a83",
             size = 2)+
  ylim(0, 5.585188e-03)+
  xlim(-74.51162, 254.51162)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency")+
  ggtitle("1)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

fig.9b = ggplot(df.dispersal, aes(x=norm.dist.ref, y=rel.freq.ref))+
  geom_line(colour = "#1b7837",
            size = 3)+
  geom_vline(xintercept = 80,
             linetype = "dashed",
             colour = "#1b7837",
             size = 2)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency")+
  ggtitle("2)")+
  ylim(0, 5.585188e-03)+
  xlim(-74.51162, 254.51162)+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())


#Leavers and Stayers::
fig.9c = ggplot(df.dispersal, aes(x=norm.dist.int, y=rel.freq.int*0.2))+
  geom_line(colour = "#762a83",
            size = 3)+
  geom_vline(xintercept = 100,
             linetype = "dashed",
             colour = "#762a83",
             size = 2)+
  ylim(0, 5.585188e-03)+
  xlim(-74.51162, 254.51162)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency")+
  ggtitle("5)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

fig.9d = ggplot(df.dispersal, aes(x=norm.dist.int, y=rel.freq.int*0.8))+
  geom_line(colour = "#762a83",
            size = 3)+
  geom_vline(xintercept = 100,
             linetype = "dashed",
             colour = "#762a83",
             size = 2)+
  ylim(0, 5.585188e-03)+
  xlim(-74.51162, 254.51162)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency")+
  ggtitle("3)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

fig.9e = ggplot(df.dispersal, aes(x=norm.dist.ref, y=rel.freq.ref*0.2))+
  geom_line(colour = "#1b7837",
            size = 3)+
  geom_vline(xintercept = 80,
             linetype = "dashed",
             colour = "#1b7837",
             size = 2)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency")+
  ylim(0, 5.585188e-03)+
  xlim(-74.51162, 254.51162)+
  ggtitle("4)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

fig.9f = ggplot(df.dispersal, aes(x=norm.dist.ref, y=rel.freq.ref*0.8))+
  geom_line(colour = "#1b7837",
            size = 3)+
  geom_vline(xintercept = 80,
             linetype = "dashed",
             colour = "#1b7837",
             size = 2)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency")+
  ylim(0, 5.585188e-03)+
  xlim(-74.51162, 254.51162)+
  ggtitle("6)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

#Intervention Site
fig.9g = ggplot(df.dispersal, aes(x=norm.dist.int, y=rel.freq.int*0.8))+
  geom_line(colour = "#762a83",
            size = 3)+
  geom_line(aes(x=norm.dist.ref, y=rel.freq.ref*0.2),
            colour = "#1b7837",
            size = 3)+
  geom_vline(xintercept = 100, linetype = "dashed",
             colour ="#762a83", size = 2)+
  geom_vline(xintercept = 80, linetype = "dashed",
             colour = "#1b7837", size = 2)+
  geom_vline(xintercept = 95, linetype = "dashed",
             colour = "#b2182b", size = 2)+
  ylim(0, 5.585188e-03)+
  xlim(-74.51162, 254.51162)+
  ggtitle("7)")+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

#refugia
fig.9h = ggplot(df.dispersal, aes(x=norm.dist.ref, y=rel.freq.ref*0.8))+
  geom_line(colour = "#1b7837", size = 3)+
  geom_line(aes(x=norm.dist.int, y=rel.freq.int*0.2),
            colour = "#762a83", size = 3)+
  geom_vline(xintercept = 100, linetype = "dashed",
             colour ="#762a83", size = 2)+
  geom_vline(xintercept = 80, linetype = "dashed",
             colour = "#1b7837", size = 2)+
  geom_vline(xintercept = 90, linetype = "dashed",
             colour = "#ff7f00", size = 2)+
  ylim(0, 5.585188e-03)+
  xlim(-74.51162, 254.51162)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency")+
  ggtitle("8)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

plot.layout9 = "
A##B
DECF
G##H
"

fig.9a + fig.9b + fig.9c + fig.9d + fig.9e + fig.9f + fig.9g + fig.9h + plot_layout(design = plot.layout9)




#Figure 10:: Cross Selection Diagram

#Figure 11:: Standard Devation Constant::

norm.dist = create_normal_distribution(trait.mean = 100,
                                       vector.length = 100000,
                                       standard.deviation = 50)

rel.freq = calculate_density_of_trait_values(trait.mean = 100,
                                             vector.length = 100000,
                                             standard.deviation = 50)


df.7 = data.frame(norm.dist, rel.freq)

ggplot(df.7, aes(x=norm.dist))+
  geom_histogram(size = 2,
                 binwidth = 2,
                 alpha = 0.7,
            fill = "#66c2a5")+
  geom_histogram(aes(x=norm.dist+10),
            size = 2,
            binwidth = 2,
            alpha = 0.7,
            fill = "#fc8d62")+
  geom_histogram(aes(x=norm.dist+20),
            size = 2,
            binwidth = 2,
            alpha = 0.7,
            fill = "#a6d854")+
  geom_histogram(aes(x=norm.dist+30),
            size = 2,
            binwidth = 2,
            alpha = 0.7,
            fill = "#8da0cb")+
  geom_histogram(aes(x=norm.dist+40),
            size = 2,
            binwidth = 2,
            alpha = 0.7,
            fill = "#e78ac3")+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())


#Figure 12:: Standard Devation Varies with z_mean::

norm.dist.1 = create_normal_distribution(trait.mean = 100,
                                       vector.length = 100000,
                                       standard.deviation = 50)

rel.freq.1 = calculate_density_of_trait_values(trait.mean = 100,
                                             vector.length = 100000,
                                             standard.deviation = 50)

norm.dist.2 = create_normal_distribution(trait.mean = 110,
                                         vector.length = 100000,
                                         standard.deviation = 55)

rel.freq.2 = calculate_density_of_trait_values(trait.mean = 110,
                                               vector.length = 100000,
                                               standard.deviation = 55)

norm.dist.3 = create_normal_distribution(trait.mean = 120,
                                         vector.length = 100000,
                                         standard.deviation = 60)

rel.freq.3 = calculate_density_of_trait_values(trait.mean = 120,
                                               vector.length = 1000,
                                               standard.deviation = 60)

norm.dist.4 = create_normal_distribution(trait.mean = 130,
                                         vector.length = 100000,
                                         standard.deviation = 65)

rel.freq.4 = calculate_density_of_trait_values(trait.mean = 130,
                                               vector.length = 100000,
                                               standard.deviation = 65)

norm.dist.5 = create_normal_distribution(trait.mean = 140,
                                         vector.length = 100000,
                                         standard.deviation = 70)

rel.freq.5 = calculate_density_of_trait_values(trait.mean = 140,
                                               vector.length = 100000,
                                               standard.deviation = 70)



df.8 = data.frame(norm.dist.1,
                  norm.dist.2,
                  norm.dist.3,
                  norm.dist.4,
                  norm.dist.5,
                  rel.freq.1,
                  rel.freq.2,
                  rel.freq.3,
                  rel.freq.4,
                  rel.freq.5)



ggplot(df.8, aes(x=norm.dist.1))+
  geom_histogram(size = 2,
            fill = "#66c2a5",
            binwidth = 2,
            alpha = 0.7)+
  geom_histogram(aes(x=norm.dist.2),
            size = 2,
            fill = "#fc8d62",
            binwidth = 2,
            alpha = 0.7)+
  geom_histogram(aes(x=norm.dist.3),
            size = 2,
            fill = "#a6d854",
            binwidth = 2,
            alpha = 0.7)+
  geom_histogram(aes(x=norm.dist.4),
            size = 2,
            fill = "#8da0cb",
            binwidth = 2,
            alpha = 0.7)+
  geom_histogram(aes(x=norm.dist.5),
            size = 2,
            fill = "#e78ac3",
            binwidth = 2,
            alpha = 0.7)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())





#Figure 13: truncation selection mixtures:::
norm.dist = create_normal_distribution(vector.length = 1000,
                                       trait.mean = 100,
                                       standard.deviation = 50)
rel.freq = calculate_density_of_trait_values(vector.length = 1000,
                                             trait.mean = 100,
                                             standard.deviation = 50)

df.9 = data.frame(norm.dist, rel.freq)
#Population emerges with a mean PRS z::

fig.13.a = ggplot(df.9, aes(x=norm.dist, y=rel.freq))+
  geom_area(fill = "#b3b3b3")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
  ggtitle("1)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")

#Some will go on to avoid the insecticide, there mean z will not change
fig.13.b = ggplot(df.9, aes(x=norm.dist, y=rel.freq*0.3))+
  geom_area(fill = "#b2df8a")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
  ggtitle("2)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")



#Other propotion will encounter the insecticide::
fig.13.c = ggplot(df.9, aes(x=norm.dist, y=rel.freq*0.7))+
  geom_area(fill = "#b3b3b3")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
  ggtitle("3)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")

#Some will die and some will survive::
df.9.a = df.9%>%
  dplyr::filter(norm.dist >= 160)


fig.13.d = ggplot(df.9, aes(x=norm.dist, y=rel.freq*0.7))+
  geom_area(fill = "#b2182b")+
  geom_area(data=df.5.a, aes(x=norm.dist,
                             y=rel.freq*0.7),
            fill = "#2166ac")+
  geom_vline(xintercept = 160,
             linetype = "dashed",
             size = 2)+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("4)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

fig.13.e = ggplot(df.9, aes(x=norm.dist, y=rel.freq*0.7))+
  geom_area(fill="white", alpha = 0)+
  geom_area(data=df.5.a, aes(x=norm.dist,
                             y=rel.freq*0.7),
            fill = "#2166ac")+
  geom_line(data=df.9.a, aes(x=norm.dist,
                             y=rel.freq*0.7))+
  geom_vline(xintercept = 180,
             linetype = "dashed",
             colour = "purple")+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("5)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())


fig.13.f = ggplot(df.9, aes(x=norm.dist, y=rel.freq*0.7))+
  geom_area(fill="white", alpha = 0)+
  geom_area(data=df.9.a, aes(x=norm.dist,
                             y=rel.freq*0.7*0.2),
            fill = "#0c2c84")+
  geom_line(data=df.9.a, aes(x=norm.dist,
                             y=rel.freq*0.7*0.2))+
  geom_vline(xintercept = 180,
             linetype = "dashed",
             colour = "purple")+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("6)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())
#combine together::

unselected = rel.freq*0.3
selected = rel.freq*0.7

df.9.b = data.frame(unselected, selected, norm.dist)
df.9.b$selected = ifelse(df.9.b$norm.dist >= 160,
                         yes = df.9.b$selected,
                         no = 0)*0.2


df.9.b$total = df.9.b$unselected + df.9.b$selected

fig.13.g = ggplot(df.9.b, aes(x=norm.dist, y=total))+
  geom_area(fill = "#1b9e77")+
  geom_line()+
  geom_vline(xintercept = 130,
             linetype = "dashed",
             colour = "orange")+
  geom_vline(xintercept = 100,
             linetype = "dashed")+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("7)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

#Combine into single plot:::
plot.layout = "
A#B##G
#CDEF#"


fig.13.a + fig.13.b + fig.13.c + fig.13.d + fig.13.e + fig.13.f + fig.13.g + plot_layout(design = plot.layout)





mulitple_g_cycle_overview = function(){

  g.cycle = seq(1, 11, by = 1)
  z.score = seq(100, 200, by = 10)
  f.differential = seq(1, 4, by = 0.3)
  n.individuals = c()
  m.differential = rep(0.7, 11)


  for(i in 1:11){

    n.individuals[i] = 10000*(0.6^i)

  }


  df.cycles = data.frame(g.cycle, z.score, n.individuals, f.differential, m.differential)


  plot.n = ggplot(df.cycles, aes(x=g.cycle, y=n.individuals))+
    geom_col(colour = "#ae017e",
             fill = "#fa9fb5",
             size = 2)+
    xlab("Gonotrophic Cycle")+
    ylab("Number of Ovipostion Events")+
    ggtitle("A")+
    theme_bw()+
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")


  plot.z = ggplot(df.cycles, aes(x=g.cycle, y=z.score))+
    geom_col(colour = "#016c59",
             fill = "#3690c0",
             size = 2)+
    xlab("Gonotrophic Cycle")+
    ylab("Mean Polygenic Resistance Score")+
    ggtitle("B")+
    theme_bw()+
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")


  plot.s = ggplot(df.cycles, aes(x=g.cycle, y=f.differential))+
    geom_col(colour = "#810f7c",
             fill = "#8c96c6",
             size = 2)+
    geom_col(aes(x=g.cycle, y=m.differential),
             width = 0.5,
             colour = "#800026",
             fill= "#fc4e2a",
             size = 2)+
    ylim(0, 4)+
    xlab("Gonotrophic Cycle")+
    ylab("Selection Differential")+
    ggtitle("C")+
    theme_bw()+
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")


  plot.r = ggplot(df.cycles, aes(x=g.cycle, y=0.3*((f.differential+1)/2)))+
    geom_col(colour = "#00441b",
             fill = "#74c476",
             size = 2)+
    xlab("Gonotrophic Cycle")+
    ylab("Response to Selection")+
    ggtitle("D")+
    ylim(0, 4)+
    theme_bw()+
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")


  p.layout = "
AB
CD
"

  final.plot = plot.n + plot.z + plot.s + plot.r + plot_layout(design = p.layout)

  return(final.plot)
}

mulitple_g_cycle_overview()














