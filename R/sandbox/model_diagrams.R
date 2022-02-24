#Multiple rounds of selection.... Figure 17

#Do Male Portion of Graph:::

plot_male_graph_portion = function(){

norm.dist = create_normal_distribution(vector.length = 1000,
                                       trait.mean = 100,
                                       standard.deviation = 50)

rel.freq = calculate_density_of_trait_values(vector.length = 1000,
                                             trait.mean = 100,
                                             standard.deviation = 50)

df.17 = data.frame(norm.dist, rel.freq)

fig.17.a = ggplot(df.17, aes(x=norm.dist, y=rel.freq))+
  geom_area(fill = "#b3b3b3")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")

#Some will go on to avoid the insecticide, there mean z will not change
fig.17.b = ggplot(df.17, aes(x=norm.dist, y=rel.freq*0.6))+
  geom_area(fill = "#b2df8a")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")


#Other propotion will encounter the insecticide::
fig.17.c = ggplot(df.17, aes(x=norm.dist, y=rel.freq*0.4))+
  geom_area(fill = "#b3b3b3")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed")+
  theme_bw()+
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

df.17.a = data.frame(norm.dist, rel.freq)

fig.17.d = ggplot(df.17.a, aes(x=norm.dist, y=rel.freq*0.4))+
  geom_segment(aes(xend=norm.dist, yend=0, colour = norm.dist),
               size = 0) +
  geom_line()+
  scale_colour_gradient(low = "white",
                        high = "#2166ac")+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ylim(0, 0.008)+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")

df.17.a$survival.vals = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = norm.dist,
                                                                                                                                     michaelis.menten.slope = 1,
                                                                                                                                     maximum.bioassay.survival.proportion = 1,
                                                                                                                                     half.population.bioassay.survival.resistance = 900),
                                                                   regression.coefficient = 0.48,
                                                                   regression.intercept = 0.15,
                                                                   current.insecticide.efficacy  = 1)

fig.17.e = ggplot(df.6, aes(x=norm.dist, y=rel.freq*0.4*survival.vals))+
  geom_area(fill = "#2166ac") +
  geom_line()+
  geom_vline(xintercept = 130,
             linetype = "dashed",
             colour = "purple")+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ylim(0, 0.008)+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         legend.position = "none")


df.17$survival.vals = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = df.6$norm.dist,
                                                                                                                                   michaelis.menten.slope = 1,
                                                                                                                                   maximum.bioassay.survival.proportion = 1,
                                                                                                                                   half.population.bioassay.survival.resistance = 900),
                                                                 regression.coefficient = 0.48,
                                                                 regression.intercept = 0.15,
                                                                 current.insecticide.efficacy  = 1)


df.17$unselected = df.17$rel.freq*0.6
df.17$selected = df.17$rel.freq*0.4*df.17$survival.vals


df.17$total = df.17$unselected + df.17$selected




fig.17.f = ggplot(df.17, aes(x=norm.dist, y=total))+
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
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank())

#Combine into single plot:::
plot.layout = "
A#B#F
#CDE#"


final.plot = fig.17.a + fig.17.b + fig.17.c + fig.17.d + fig.17.e + fig.17.f + plot_layout(design = plot.layout) + plot_annotation(title = "Males")

return(final.plot)
}

plot_female_graph_g1 = function(){

  norm.dist = create_normal_distribution(vector.length = 1000,
                                         trait.mean = 100,
                                         standard.deviation = 50)

  rel.freq = calculate_density_of_trait_values(vector.length = 1000,
                                               trait.mean = 100,
                                               standard.deviation = 50)

  df.17 = data.frame(norm.dist, rel.freq)

  fig.17.a = ggplot(df.17, aes(x=norm.dist, y=rel.freq))+
    geom_area(fill = "#b3b3b3")+
    geom_line()+
    ylim(0, 0.008)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    geom_vline(xintercept = 100, colour = "black",
               linetype="dashed")+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")

  #Some will go on to avoid the insecticide, there mean z will not change
  fig.17.b = ggplot(df.17, aes(x=norm.dist, y=rel.freq*0.3))+
    geom_area(fill = "#b2df8a")+
    geom_line()+
    ylim(0, 0.008)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    geom_vline(xintercept = 100, colour = "black",
               linetype="dashed")+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")


  #Other propotion will encounter the insecticide::
  fig.17.c = ggplot(df.17, aes(x=norm.dist, y=rel.freq*0.7))+
    geom_area(fill = "#b3b3b3")+
    geom_line()+
    ylim(0, 0.008)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    geom_vline(xintercept = 100, colour = "black",
               linetype="dashed")+
    theme_bw()+
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

  df.17.a = data.frame(norm.dist, rel.freq)

  fig.17.d = ggplot(df.17.a, aes(x=norm.dist, y=rel.freq*0.7))+
    geom_segment(aes(xend=norm.dist, yend=0, colour = norm.dist),
                 size = 0) +
    geom_line()+
    scale_colour_gradient(low = "white",
                          high = "#2166ac")+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    ylim(0, 0.008)+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")

  df.17.a$survival.vals = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = norm.dist,
                                                                                                                                        michaelis.menten.slope = 1,
                                                                                                                                        maximum.bioassay.survival.proportion = 1,
                                                                                                                                        half.population.bioassay.survival.resistance = 900),
                                                                      regression.coefficient = 0.48,
                                                                      regression.intercept = 0.15,
                                                                      current.insecticide.efficacy  = 1)

  fig.17.e = ggplot(df.6, aes(x=norm.dist, y=rel.freq*0.7*survival.vals))+
    geom_area(fill = "#2166ac") +
    geom_line()+
    geom_vline(xintercept = 130,
               linetype = "dashed",
               colour = "purple")+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    ylim(0, 0.008)+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")


  df.17$survival.vals = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = df.6$norm.dist,
                                                                                                                                      michaelis.menten.slope = 1,
                                                                                                                                      maximum.bioassay.survival.proportion = 1,
                                                                                                                                      half.population.bioassay.survival.resistance = 900),
                                                                    regression.coefficient = 0.48,
                                                                    regression.intercept = 0.15,
                                                                    current.insecticide.efficacy  = 1)


  df.17$unselected = df.17$rel.freq*0.3
  df.17$selected = df.17$rel.freq*0.7*df.17$survival.vals


  df.17$total = df.17$unselected + df.17$selected




  fig.17.f = ggplot(df.17, aes(x=norm.dist, y=total))+
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
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank())

  #Combine into single plot:::
  plot.layout = "
A#B#F
#CDE#"


  final.plot = fig.17.a + fig.17.b + fig.17.c + fig.17.d + fig.17.e + fig.17.f + plot_layout(design = plot.layout) + plot_annotation(title = "Females: Gonotrophic Cycle 1")

  return(list(final.plot, df.17))
}

plot_female_graph_g2 = function(){

  #Situation at end of first gonotrophic cycle:::
  A = plot_female_graph_g1()[[2]]

  fig.17.a = ggplot(A, aes(x=norm.dist, y=total))+
    geom_area(fill = "#b3b3b3")+
    geom_line()+
    ylim(0, 0.008)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    geom_vline(xintercept = 100, colour = "black",
               linetype="dashed")+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")

  #Some will go on to avoid the insecticide, there mean z will not change
  fig.17.b = ggplot(A, aes(x=norm.dist, y=total*0.3))+
    geom_area(fill = "#b2df8a")+
    geom_line()+
    ylim(0, 0.008)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    geom_vline(xintercept = 100, colour = "black",
               linetype="dashed")+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")


  #Other propotion will encounter the insecticide::
  fig.17.c = ggplot(A, aes(x=norm.dist, y=total*0.7))+
    geom_area(fill = "#b3b3b3")+
    geom_line()+
    ylim(0, 0.008)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    geom_vline(xintercept = 100, colour = "black",
               linetype="dashed")+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")

  A$survival.vals = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = norm.dist,
                                                                                                                                        michaelis.menten.slope = 1,
                                                                                                                                        maximum.bioassay.survival.proportion = 1,
                                                                                                                                        half.population.bioassay.survival.resistance = 900),
                                                                      regression.coefficient = 0.48,
                                                                      regression.intercept = 0.15,
                                                                      current.insecticide.efficacy  = 1)

  fig.17.e = ggplot(A, aes(x=norm.dist, y=total*0.7*survival.vals))+
    geom_area(fill = "#2166ac") +
    geom_line()+
    geom_vline(xintercept = 130,
               linetype = "dashed",
               colour = "purple")+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    ylim(0, 0.008)+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")


  A$survival.vals = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = df.6$norm.dist,
                                                                                                                                      michaelis.menten.slope = 1,
                                                                                                                                      maximum.bioassay.survival.proportion = 1,
                                                                                                                                      half.population.bioassay.survival.resistance = 900),
                                                                    regression.coefficient = 0.48,
                                                                    regression.intercept = 0.15,
                                                                    current.insecticide.efficacy  = 1)


  A$unselected = A$total*0.3
  A$selected = A$total*0.7*A$survival.vals


  A$total.2 = A$unselected + A$selected




  fig.17.f = ggplot(A, aes(x=norm.dist, y=total.2))+
    geom_area(fill = "#1b9e77")+
    geom_line()+
    geom_vline(xintercept = 110,
               linetype = "dashed",
               colour = "orange",
               alpha = 0.7)+
    geom_vline(xintercept = 120,
               linetype = "dashed",
               colour = "orange")+
    geom_vline(xintercept = 100,
               linetype = "dashed")+
    ylim(0, 0.008)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank())

  #Combine into single plot:::
  plot.layout = "
##A#D
##BC#"


  final.plot = fig.17.b + fig.17.c + fig.17.e + fig.17.f + plot_layout(design = plot.layout) + plot_annotation(title = "Females: Gonotrophic Cycle 2")

  return(list(final.plot, A))

}

plot_female_graph_g3 = function(){

  #Situation at end of first gonotrophic cycle:::
  A = plot_female_graph_g2()[[2]]

  fig.17.a = ggplot(A, aes(x=norm.dist, y=total.2))+
    geom_area(fill = "#b3b3b3")+
    geom_line()+
    ylim(0, 0.008)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    geom_vline(xintercept = 100, colour = "black",
               linetype="dashed")+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")

  #Some will go on to avoid the insecticide, there mean z will not change
  fig.17.b = ggplot(A, aes(x=norm.dist, y=total.2*0.3))+
    geom_area(fill = "#b2df8a")+
    geom_line()+
    ylim(0, 0.008)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    geom_vline(xintercept = 100, colour = "black",
               linetype="dashed")+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")


  #Other propotion will encounter the insecticide::
  fig.17.c = ggplot(A, aes(x=norm.dist, y=total.2*0.7))+
    geom_area(fill = "#b3b3b3")+
    geom_line()+
    ylim(0, 0.008)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    geom_vline(xintercept = 100, colour = "black",
               linetype="dashed")+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")

  A$survival.vals = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = norm.dist,
                                                                                                                                  michaelis.menten.slope = 1,
                                                                                                                                  maximum.bioassay.survival.proportion = 1,
                                                                                                                                  half.population.bioassay.survival.resistance = 900),
                                                                regression.coefficient = 0.48,
                                                                regression.intercept = 0.15,
                                                                current.insecticide.efficacy  = 1)

  fig.17.e = ggplot(A, aes(x=norm.dist, y=total.2*0.7*survival.vals))+
    geom_area(fill = "#2166ac") +
    geom_line()+
    geom_vline(xintercept = 130,
               linetype = "dashed",
               colour = "purple")+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    ylim(0, 0.008)+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank(),
           legend.position = "none")


  A$survival.vals = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = df.6$norm.dist,
                                                                                                                                  michaelis.menten.slope = 1,
                                                                                                                                  maximum.bioassay.survival.proportion = 1,
                                                                                                                                  half.population.bioassay.survival.resistance = 900),
                                                                regression.coefficient = 0.48,
                                                                regression.intercept = 0.15,
                                                                current.insecticide.efficacy  = 1)


  A$unselected = A$total.2*0.3
  A$selected = A$total.2*0.7*A$survival.vals


  A$total.3 = A$unselected + A$selected




  fig.17.f = ggplot(A, aes(x=norm.dist, y=total.3))+
    geom_area(fill = "#1b9e77")+
    geom_line()+
    geom_vline(xintercept = 110,
               linetype = "dashed",
               colour = "orange",
               alpha = 0.7)+
    geom_vline(xintercept = 120,
               linetype = "dashed",
               colour = "orange",
               alpha=0.7)+
    geom_vline(xintercept = 130,
               linetype = "dashed",
               colour = "orange")+
    geom_vline(xintercept = 100,
               linetype = "dashed")+
    ylim(0, 0.008)+
    xlab("Polygenic Resistance Score")+
    ylab("Frequency in the Population")+
    theme_bw()+
    theme( axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks = element_blank())

  #Combine into single plot:::
  plot.layout = "
##A#D
##BC#"


  final.plot =  fig.17.b + fig.17.c + fig.17.e + fig.17.f + plot_layout(design = plot.layout) + plot_annotation(title = "Females: Gonotrophic Cycle 3")

  return(list(final.plot, A))

}

plot_male_graph_portion()
plot_female_graph_g1()[[1]]
plot_female_graph_g2()[[1]]
plot_female_graph_g3()[[1]]


value = c(1000, 550, 150, 100, 250, 400)
g.cycle = c(1, 2, 3, 1, 2, 3)
type = c("egg", "egg", "egg", "resistance", "resistance", "resistance")
temp.df = data.frame(value, g.cycle, type)

ggplot(temp.df, aes(y=value, x=g.cycle, fill = type))+
  geom_col(colour = "black",
           size = 2,
           position = "dodge")+
  scale_fill_manual(values = c("#c51b8a", "#9ebcda"))+
  xlab("Gonotrophic Cycle")+
  ylab(paste0("Number of Eggs Laid/  \n Polygenic Resistance Score"))+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 20),
        axis.title.y =  element_text(size = 15),
        axis.title.x =  element_text(size = 20))



