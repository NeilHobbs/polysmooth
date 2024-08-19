
norm.dist = create_normal_distribution(vector.length = 1000,
                                       trait.mean = 100,
                                       standard.deviation = 50)

rel.freq = calculate_density_of_trait_values(vector.length = 1000,
                                             trait.mean = 100,
                                             standard.deviation = 50)

survives.smooth = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                trait.mean = norm.dist,
                                                                michaelis.menten.slope = 1,
                                                                half.population.bioassay.survival.resistance = 900)

df.6 = data.frame(norm.dist, rel.freq, survives.smooth)
#Population emerges with a mean PRS z::

fig.8.a = ggplot(df.6, aes(x=norm.dist, y=rel.freq))+
  geom_area(fill = "#b3b3b3")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed", size = 2)+
  theme_bw()+
  ggtitle("Panel 1)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_text(size = 12),
         title = element_text(size = 18),
         legend.position = "none")

#Some will go on to avoid the insecticide, there mean z will not change
fig.8.b = ggplot(df.6, aes(x=norm.dist, y=rel.freq*0.3))+
  geom_area(fill = "#b2df8a")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed", size = 2)+
  geom_label(x= 100, y = 0.006,
             label = paste0("a proportion of individuals\nescape insecticide exposure"),
             colour = "black",
             size = 6)+
  theme_bw()+
  ggtitle("Panel 2)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_text(size = 12),
         title = element_text(size = 18),
         legend.position = "none")

#Other propotion will encounter the insecticide::
fig.8.c = ggplot(df.6, aes(x=norm.dist, y=rel.freq*0.7))+
  geom_area(fill = "#b3b3b3")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_vline(xintercept = 100, colour = "black",
             linetype="dashed", size  = 2)+
  geom_label(x= 100, y = 0.007,
             label = paste0("a proportion of\nindividuals encounter\nthe insecticide"),
             colour = "black",
             size = 6)+
  theme_bw()+
  ggtitle("Panel 3)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_text(size = 12),
         title = element_text(size = 18),
         legend.position = "none")

#These indivuals will surive depending upon their PRS:
#Scaling values to make the process more obvious for plots
A = seq(0, 1, 0.001)[1:1000]
B = A^3

fig.8.d =ggplot(df.6, aes(x=norm.dist,
                 y=rel.freq*0.7))+
  geom_area(fill = "#cb181d")+
  geom_area(aes(x=norm.dist,
                y=rel.freq*((survives.smooth*B)+A/7)*0.7),
            fill = "#3690c0")+
  geom_line(colour = "black", size = 2)+
  geom_line(aes(x=norm.dist,
                y=rel.freq*((survives.smooth*B)+A/7)*0.7),
            colour = "black", size = 1,
            linetype = "dashed")+

  geom_abline(intercept = 0.00065, slope = 0.00001,
              linetype = "dashed",
              linewidth = 2,
              colour = "darkgrey")+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  geom_text(aes(x=70,
                y=0.0019, label = paste0("Survival Probability")),
            colour = "darkgrey",
            size = 5,
            angle = 25)+
  ggtitle("Panel 4)")+
  ylim(0, 0.008)+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_text(size = 12),
         title = element_text(size = 18),
         legend.position = "none")

fig.8.e = ggplot(df.6, aes(x=norm.dist, y=rel.freq*0.7*survives.smooth))+
  geom_area(fill = "#2166ac") +
  geom_line()+
  geom_vline(xintercept = 120,
             linetype = "dashed",
             colour = "purple", size = 2)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ylim(0, 0.008)+
  ggtitle("Panel 5)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_text(size = 12),
         title = element_text(size = 18),
         legend.position = "none")


df.6$unselected = df.6$rel.freq*0.3
df.6$selected = df.6$rel.freq*(((df.6$survives.smooth*B)+A/7)*0.7)


df.6$total = df.6$unselected + df.6$selected


fig.8.f = ggplot(df.6, aes(x=norm.dist, y=total))+
  geom_area(fill = "#1b9e77")+
  geom_line()+
  geom_vline(xintercept = 110,
             linetype = "dashed",
             colour = "orange", size = 2)+
  geom_vline(xintercept = 100,
             linetype = "dashed", size = 2)+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("Panel 6)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_text(size = 12),
         title = element_text(size = 18),
         legend.position = "none")

#Combine into single plot:::
plot.layout = "
A#B#F
#CDE#"


fig.8.a + fig.8.b + fig.8.c + fig.8.d + fig.8.e + fig.8.f + plot_layout(design = plot.layout)


ggsave(
  filename = "chapter3_figure5.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 700,
  height = 400,
  units = "px",
  dpi = 200)
