library(devtools)
load_all()
library(patchwork)

z.values = create_normal_distribution(vector.length = 10000,
                                      trait.mean = 900,
                                      standard.deviation = 20)

relative.frequency = calculate_density_of_trait_values(vector.length = 10000,
                                                       trait.mean = 900,
                                                       standard.deviation = 20)

surv.vals = (convert_bioassay_survival_to_field_survival(convert_resistance_score_to_bioassay_survival(trait.mean = z.values),
                                            current.insecticide.efficacy = 1,
                                            regression.coefficient = 0.48,
                                            regression.intercept = 0.15))


natural.mortality = 0.8^3


##
g.1 = relative.frequency * surv.vals
g.2 = g.1 * natural.mortality * surv.vals
g.3 = g.2 * natural.mortality * surv.vals


df = data.frame(relative.frequency,
                g.1,
                g.2,
                g.3,
                z.values)


p.1 = ggplot(df, aes(y=relative.frequency,
               x=z.values))+
geom_area(fill="skyblue")+
  ylim(0, max(relative.frequency))+
  xlim(min(z.values), max(z.values))+
  geom_vline(xintercept = 900,
             linetype = "dashed",
             colour = "black")+
  ggtitle("Initial Population")+
  ylab("Frequency")+
  xlab("Polygenic Resistance Score")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

p.2 = ggplot(df, aes(y=g.1,
               x=z.values + 5))+
  geom_area(fill = "skyblue")+
  geom_vline(xintercept = 900,
             linetype = "dashed",
             colour = "black")+
  geom_vline(xintercept = 905,
             linetype = "dashed",
             colour = "red")+
  ylim(0, max(relative.frequency))+
  xlim(min(z.values), max(z.values))+
  ylab("Frequency")+
  xlab("Polygenic Resistance Score")+
  ggtitle("Gonotrophic Cycle 1")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())


p.3 = ggplot(df, aes(y=g.2,
               x=z.values + 10))+
  geom_area(fill = "skyblue")+
  geom_vline(xintercept = 900,
             linetype = "dashed",
             colour = "black")+
  geom_vline(xintercept = 910,
             linetype = "dashed",
             colour = "red")+
  ylim(0, max(relative.frequency))+
  xlim(min(z.values), max(z.values))+
  ylab("Frequency")+
  xlab("Polygenic Resistance Score")+
  ggtitle("Gonotrophic Cycle 2")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

p.4 = ggplot(df, aes(y=g.3,
               x=z.values + 20))+
  geom_area(fill = "skyblue")+
  geom_vline(xintercept = 900,
             linetype = "dashed",
             colour = "black")+
  geom_vline(xintercept = 920,
             linetype = "dashed",
             colour = "red")+
  ylim(0, max(relative.frequency))+
  xlim(min(z.values), max(z.values))+
  ylab("Frequency")+
  xlab("Polygenic Resistance Score")+
  ggtitle("Gonotrophic Cycle 3")+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())


df.1 = data.frame(values = c(10, 10, 10, 15, 20, 25, 12.5, 15, 17.5,
                             25, 10, 5),
                  parameter = factor(c(rep("Male Selection Differential", 3),
                                rep("Female Selection Differential", 3),
                                rep("Response to Selection", 3),
                                rep("Oviposition Events", 3)),
                                levels = c("Male Selection Differential",
                                           "Female Selection Differential",
                                           "Response to Selection",
                                           "Oviposition Events")),
                  g.cycle = rep(c(1,2,3), 4))


p.5 = ggplot(df.1, aes(x=g.cycle,
                 y=values,
                 fill = parameter))+
  geom_col()+
  scale_fill_manual(values = c("#1b9e77",
                               "#d95f02",
                               "#7570b3",
                               "#e7298a"))+
  ylab("Value")+
  xlab("Gonotrophic Cycle")+
  facet_grid(. ~ parameter)+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



the.layout = "
ABCD
EEEE
"

p.1 + p.2 + p.3 + p.4 + p.5 +
  plot_layout(design = the.layout)



ggsave(
  filename = "chapter6_figure2.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 600,
  height = 400,
  units = "px",
  dpi = 300)














