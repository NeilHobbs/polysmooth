Mixtures
Monotherapies (pyrethroid)
monotherapies (novel)

Rotation: Mixtures

Sequence: Mixtures

Accidental Micro-Mosaics



x.vals = rep(rep(seq(1, 10, 1), times = 10), 10)
y.vals = rep(rep(seq(1, 10, 1), each = 10), 10)
mixtures = rep(sample(c("Mixture", "None"), 100, replace = TRUE, prob = c(0.7, 0.3)), 10)
micromosaic = rep(sample(c("Mixture", "Pyrethroid", "None"), 100, replace = TRUE, prob = c(0.5, 0.25, 0.25)), 10)
timestep = factor((paste("Deployment", rep(seq(1, 10), each = 100))),
                  levels = c("Deployment 1",  "Deployment 2",  "Deployment 3",  "Deployment 4",  "Deployment 5",  "Deployment 6",  "Deployment 7",  "Deployment 8",  "Deployment 9",  "Deployment 10"))


mixures.rotation = ifelse(timestep %in% c("Deployment 1",  "Deployment 3",  "Deployment 5",    "Deployment 7",   "Deployment 9"),
                          yes = ifelse(mixtures == "Mixture", yes = "mix1", no = "None"),
                          no = ifelse(mixtures == "Mixture", yes = "mix2", no = "None"))

mixures.sequences = ifelse(timestep %in% c("Deployment 1",  "Deployment 2",  "Deployment 3",    "Deployment 4",   "Deployment 5"),
                          yes = ifelse(mixtures == "Mixture", yes = "mix1", no = "None"),
                          no = ifelse(mixtures == "Mixture", yes = "mix2", no = "None"))


irm.strat.df = data.frame(x.vals, y.vals, mixtures, micromosaic, timestep, mixures.rotation , mixures.sequences)

mixtures.plot = ggplot(irm.strat.df, aes(x= x.vals, y = y.vals, fill = mixtures))+
  geom_tile(colour = "grey")+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values = c("#88419d", "white"))+
  facet_grid(. ~ timestep )+
  ylab("Mixture")+
  theme(panel.border = element_rect(colour = "black",
                                    fill = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white", colour = "black"))

micromosaics.plot = ggplot(irm.strat.df, aes(x= x.vals, y = y.vals, fill = micromosaic))+
  geom_tile(colour = "grey")+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values = c("#88419d", "skyblue", "white"))+
  facet_grid(. ~ timestep )+
  ylab("Accidental\nMicro-Mosaic")+
  theme(panel.border = element_rect(colour = "black",
                                    fill = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white", colour = "black"))

pyr.plot = ggplot(irm.strat.df, aes(x= x.vals, y = y.vals, fill = mixtures))+
  geom_tile(colour = "grey")+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values = c("skyblue", "white"))+
  facet_grid(. ~ timestep )+
  ylab("Pyrethroid\nMonotherapy")+
  theme(panel.border = element_rect(colour = "black",
                                    fill = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white", colour = "black"))

novel.plot = ggplot(irm.strat.df, aes(x= x.vals, y = y.vals, fill = mixtures))+
  geom_tile(colour = "grey")+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values = c("coral", "white"))+
  ylab("Novel\nMonotherapy")+
  facet_grid(. ~ timestep )+
  theme(panel.border = element_rect(colour = "black",
                                    fill = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white", colour = "black"))

mix.rot.plot = ggplot(irm.strat.df, aes(x= x.vals, y = y.vals, fill = mixures.rotation))+
  geom_tile(colour = "grey")+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values = c("#88419d", "#238b45", "white"))+
  facet_grid(. ~ timestep )+
  ylab("Mixture\nRotation")+
  theme(panel.border = element_rect(colour = "black",
                                    fill = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white", colour = "black"))

mix.seq.plot = ggplot(irm.strat.df, aes(x= x.vals, y = y.vals, fill = mixures.sequences))+
  geom_tile(colour = "grey")+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(values = c("#88419d", "#238b45", "white"))+
  facet_grid(. ~ timestep )+
  ylab("Mixture\nSequence")+
  theme(panel.border = element_rect(colour = "black",
                                    fill = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white", colour = "black"))


the.layout = "
A
B
C
D
E
F"

mixtures.plot +
  pyr.plot +
  novel.plot +
  micromosaics.plot +
  mix.rot.plot +
  mix.seq.plot +
  plot_layout(design = the.layout)



ggsave(filename = "IRM_strategy_description_visualisation.jpeg",
       plot = last_plot(),
       height = 800,
       width = 1200,
       scale = 5,
       dpi = 600,
       units = "px")














































