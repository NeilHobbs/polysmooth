##smooth selection stuff:::

solo.smooth = read.csv("C:/Users/neilp/OneDrive - LSTM/polysmooth/solo.novel.smooth.csv")
presistant.mixture.smooth = read.csv("C:/Users/neilp/OneDrive - LSTM/polysmooth/mixture.preresistance.smooth.csv")


solo.novel.full = rbind(solo.smooth, solo.smooth, solo.smooth, solo.smooth,
                        solo.smooth, solo.smooth, solo.smooth, solo.smooth)


presistant.mixture.smooth$diff = presistant.mixture.smooth$simulation.duration - solo.novel.full$sim.duration


hist(presistant.mixture.smooth$diff)

presistant.mixture.smooth$category = ifelse(presistant.mixture.smooth$diff > 0,
                                            yes = "favours mixtures",
                                            no = "no change")

presistant.mixture.smooth$start.bioassay.old = c(rep(0, 5000), rep(2, 5000),
                                                 rep(5, 5000), rep(10, 5000), rep(20, 5000),
                                                 rep(50, 5000), rep(80, 5000), rep(90, 5000))

ggplot(presistant.mixture.smooth, aes(x=diff/10,
                                      fill = category))+
  geom_histogram(binwidth = 1,
                 colour = "black")+
  scale_fill_manual(values = c("#8e0152", "#276419", "#999999"))+
  facet_wrap(~start.bioassay.old)+
  xlab("Change in Operational Lifespan (Years)")+
  theme_classic()


round(range(presistant.mixture.smooth$Female.Fitness.Cost), 2)

presistant.mixture.smooth.1 = presistant.mixture.smooth%>%
  dplyr::filter(Female.Fitness.Cost < 0.3)%>%
  dplyr::filter(Male.Fitness.Cost < 0.3)

colnames(presistant.mixture.smooth)
ggplot(presistant.mixture.smooth.1, aes(x=diff/10,
                                        fill = category))+
  geom_histogram(binwidth = 1,
                 colour = "black")+
  scale_fill_manual(values = c("#8e0152", "#276419", "#999999"))+
  facet_wrap(~start.bioassay.old)+
  xlab("Change in Operational Lifespan (Years)")+
  theme_classic()
