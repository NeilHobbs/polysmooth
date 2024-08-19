#requires patchwork
plot_smooth_truncation_selection_examples = function(){

z.values = create_normal_distribution(vector.length = 250,
                                      trait.mean = 100,
                                      standard.deviation = 20)

relative.frequency = calculate_density_of_trait_values(vector.length = 250,
                                                       trait.mean = 100,
                                                       standard.deviation = 20)


survives.smooth = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                          trait.mean = z.values,
                                                          michaelis.menten.slope = 1,
                                                          half.population.bioassay.survival.resistance = 900)

df.smooth = data.frame(z.values, relative.frequency, survives.smooth)

survives.truncation = c(rep(0, 220), relative.frequency[221:250])

df.truncation = data.frame(z.values, relative.frequency, survives.truncation)


smooth.selection.plot = ggplot(df.smooth, aes(x=z.values,
               y=relative.frequency))+
  geom_area(fill = "#cb181d")+
  geom_area(aes(x=z.values,
                y=relative.frequency*10000*(survives.smooth^5)), #done to exagerate the curve for better visualisation of the process
            fill = "#3690c0")+
  geom_line(colour = "black", size = 2)+
  geom_line(aes(x=z.values,
                y=relative.frequency*10000*(survives.smooth^5)), #done to exagerate the curve for better visualisation of the process
            colour = "black", size = 1,
            linetype = "dashed")+
  geom_text(label = "Die", x = 100, y = 0.01,
            size = 10, colour = "black")+
  geom_text(label = "Survive", x = 115, y = 0.0015,
            size = 8, colour = "black")+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("Smooth Selection - polysmooth")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30))


truncation.selection.plot = ggplot(df.truncation, aes(x=z.values,
                          y=relative.frequency))+
  geom_area(fill = "#cb181d")+
  geom_area(aes(x=z.values,
                y=survives.truncation),
            fill = "#3690c0")+
  geom_line(colour = "black", size = 2)+
  geom_text(label = "Die", x = 100, y = 0.01,
            size = 10, colour = "black")+
  geom_line(data= data.frame(freq =c(0.009974628, 0),
                             z = c(123.5464, 123.5464)),
            aes(y=freq,
               x=z),
            size = 2, linetype = "dashed")+
  geom_text(label = "Survive", x = 135, y = 0.0015,
            size = 8, colour = "black")+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency in the Population")+
  ggtitle("Truncation Selection - polytruncate")+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30))

the.plot = smooth.selection.plot + truncation.selection.plot

return(the.plot)

}


plot_smooth_truncation_selection_examples()



ggsave(
  filename = "chapter3_figure1.jpeg",
  plot = plot_smooth_truncation_selection_examples(),
  scale = 5,
  width = 850,
  height = 400,
  units = "px",
  dpi = 300)
