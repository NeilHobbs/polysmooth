#Sequences Set 24 - Polysmooth
  #For the calibration of the time to 10% bioassay with a mechanistic calculation of the selection differential.


#Data is from sequences set 24.

sequences.set.24.df = read.csv(".//Simulation Experiments/Data from Simulations/sequence.set.24.csv")

colnames(sequences.set.24.df)


library(ggplot2)
library(dplyr)
#remove the 500 generation values
sequences.set.24.df.10 = sequences.set.24.df%>%
  dplyr::filter(sim.duration.beta.10 != 500)

sequences.set.24.df.20 = sequences.set.24.df%>%
  dplyr::filter(sim.duration.beta.20 != 500)


beta.10.plot = ggplot(sequences.set.24.df.10, aes(x=sim.duration.beta.10))+
  geom_histogram(binwidth = 10,
                 colour = "grey",
                 fill = "black")+
  geom_vline(xintercept = 90, colour = "red")+
  geom_vline(xintercept = 110, colour = "red")+
  xlab("Simulation Duration")+
  facet_wrap(~standard.deviation)+
  ggtitle("Exposure Scaling Factor = 10")+
  theme_classic()

beta.20.plot = ggplot(sequences.set.24.df.20, aes(x=sim.duration.beta.20))+
  geom_histogram(binwidth = 10,
                 colour = "grey",
                 fill = "black")+
  geom_vline(xintercept = 90, colour = "red")+
  geom_vline(xintercept = 110, colour = "red")+
  xlab("Simulation Duration")+
  facet_wrap(~standard.deviation)+
  ggtitle("Exposure Scaling Factor = 20")+
  theme_classic()

gridExtra::grid.arrange(beta.10.plot, beta.20.plot,
                         ncol = 2)


#Best calibrated seems to be beta =20
#with SD in the range of 30-35.
  #Use Beta = 10, SD = 40 as the default values.



