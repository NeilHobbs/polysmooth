##Figure 7:: Flow Diagram of Truncation Selection::
library(patchwork)

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
fig.7.b = ggplot(df.5, aes(x=norm.dist, y=rel.freq*0.3))+
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
                           size = 3.4)+
  theme_bw()+
  ggtitle("Panel 2)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_text(size = 12),
         title = element_text(size = 18),
         legend.position = "none")

#Other propotion will encounter the insecticide::
fig.7.c = ggplot(df.5, aes(x=norm.dist, y=rel.freq*0.7))+
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
             size = 3.4)+
  theme_bw()+
  ggtitle("Panel 3)")+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_text(size = 12),
         title = element_text(size = 18),
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
             size = 2, colour = "#67000d")+
  geom_line()+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("Panel 4)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_text(size = 12),
         title = element_text(size = 18),
         legend.position = "none")

fig.7.e = ggplot(df.5, aes(x=norm.dist, y=rel.freq*0.7))+
  geom_area(fill="white", alpha = 0)+
  geom_area(data=df.5.a, aes(x=norm.dist,
                             y=rel.freq*0.7),
            fill = "#2166ac")+
  geom_line(data=df.5.a, aes(x=norm.dist,
                             y=rel.freq*0.7))+
  geom_vline(xintercept = 180,
             linetype = "dashed",
             colour = "purple", size = 2)+
  ylim(0, 0.008)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("Panel 5)")+
  theme_bw()+
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_text(size = 12),
         title = element_text(size = 18),
         legend.position = "none")
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


fig.7.a + fig.7.b + fig.7.c + fig.7.d + fig.7.e + fig.7.f + plot_layout(design = plot.layout)

