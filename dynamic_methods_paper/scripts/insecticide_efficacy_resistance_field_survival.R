##Figure 3:: Insecticide Efficacy, PRS and Field Survival
eff.values = seq(0, 1.25, 0.01)

survival.0 = convert_bioassay_survival_to_field_survival(bioassay.survival = 0,
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

label.df = data.frame(bioassay.survival = c(0, 10, 50, 80, 90))


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
  geom_text(label = paste0("Insecticide is above\nrecommended dose\nfor example\noversprayed"),
            x= 1.15, y = 80, size = 10)+
    xlab("Insecticide Efficacy")+
  ylab("Field Survival (%)")+
  theme_bw()+
  theme(axis.title.y =  element_text(size = 35),
        axis.title.x =  element_text(size = 35),
        axis.text.x = element_text(size = 35, colour = "black"),
        axis.text.y = element_text(size = 35, colour = "black"))


ggsave(
  filename = "chapter3_figure3.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 700,
  height = 400,
  units = "px",
  dpi = 200)
