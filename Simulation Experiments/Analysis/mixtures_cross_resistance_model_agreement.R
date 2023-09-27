#Chapter 4:: mixtures - Cross Resistance and Dosing
library(devtools)
load_all()
library(patchwork)


#Read in the datasets: polysmooth
smooth.mixtures.df = read.csv(".//part.3.mixture.cross.resistance.polysmooth.csv")
smooth.solo.df = read.csv(".//part.3.solo.cross.resistance.polysmooth.csv")
smooth.rotation.df = read.csv(".//part.3.rotation.cross.resistance.polysmooth.csv")

#Add in model name:
smooth.mixtures.df$model = "polysmooth"
smooth.solo.df$model = "polysmooth"
smooth.rotation.df$model = "polysmooth"

#read in the datasets: polytruncate

truncation.mixtures.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/part.3.mixture.cross.resistance.csv")
truncation.solo.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/part.3.solo.cross.resistance.csv")
truncation.rotation.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/part.3.rotation.cross.resistance.csv")

#Add in model name:
truncation.mixtures.df$model = "polytruncate"
truncation.solo.df$model = "polytruncate"
truncation.rotation.df$model = "polytruncate"




create_comparison_dataset= function(mixtures.df,
                                       solo.df,
                                       rotation.df,
                                    the.model){


##convert all resistance scores to bioassay survival:::
mixtures.df$insecticide.i.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                   michaelis.menten.slope = 1,
                                                                                   trait.mean = mixtures.df$insecticide.i)
##convert all resistance scores to bioassay survival:::
mixtures.df$insecticide.j.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                   michaelis.menten.slope = 1,
                                                                                   trait.mean = mixtures.df$insecticide.j)


#Subset dataset into the three mixture types
full.dose.df = subset(mixtures.df, dose == 1)
half.dose.50.df = subset(mixtures.df, dose == 0.5)
half.dose.75.df = subset(mixtures.df, dose == 0.75)

##convert all resistance scores to bioassay survival:::
solo.df$deployed.insecticide.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                      half.population.bioassay.survival.resistance = 900,
                                                                                      michaelis.menten.slope = 1,
                                                                                      trait.mean = solo.df$deployed.insecticide)


solo.df$not.deployed.insecticide.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                          half.population.bioassay.survival.resistance = 900,
                                                                                          michaelis.menten.slope = 1,
                                                                                          trait.mean = solo.df$not.deployed.insecticide)



rotation.df$insecticide.i.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                   michaelis.menten.slope = 1,
                                                                                   trait.mean = rotation.df$insecticide.i)
##convert all resistance scores to bioassay survival:::
rotation.df$insecticide.j.bioassay = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = 1,
                                                                                   half.population.bioassay.survival.resistance = 900,
                                                                                   michaelis.menten.slope = 1,
                                                                                   trait.mean = rotation.df$insecticide.j)












#Compare versus solo deployments:
#As heritability and dosing (no decay) the same rate of evolution in both halves of the mixture will be the same


total.difference.fd = (full.dose.df$insecticide.i.bioassay + full.dose.df$insecticide.j.bioassay) - (solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay)
total.difference.50.hd = (half.dose.50.df$insecticide.i.bioassay + half.dose.50.df$insecticide.j.bioassay) - (solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay)
total.difference.75.hd = (half.dose.75.df$insecticide.i.bioassay + half.dose.75.df$insecticide.j.bioassay) - (solo.df$deployed.insecticide.bioassay + solo.df$not.deployed.insecticide.bioassay)

overall.fd = ifelse(total.difference.fd < 0,
                    yes = "mixtures better",
                    no = "mixtures worse")

overall.50.hd = ifelse(total.difference.50.hd < 0,
                       yes = "mixtures better",
                       no = "mixtures worse")

overall.75.hd = ifelse(total.difference.75.hd < 0,
                       yes = "mixtures better",
                       no = "mixtures worse")


solo.mix.df = data.frame(cross.resistance = solo.df$cross.resistance,
                         total.difference.fd,
                         total.difference.75.hd,
                         total.difference.50.hd,
                         overall.fd,
                         overall.50.hd,
                         overall.75.hd,
                        model = the.model)

##Now compare against rotations:::
rot.total.difference.fd = (full.dose.df$insecticide.i.bioassay + full.dose.df$insecticide.j.bioassay) - (rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay)
rot.total.difference.50.hd = (half.dose.50.df$insecticide.i.bioassay + half.dose.50.df$insecticide.j.bioassay) - (rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay)
rot.total.difference.75.hd = (half.dose.75.df$insecticide.i.bioassay + half.dose.75.df$insecticide.j.bioassay) - (rotation.df$insecticide.i.bioassay + rotation.df$insecticide.j.bioassay)

cross.resistance = full.dose.df$cross.resistance

rot.overall.fd = ifelse(rot.total.difference.fd < 0,
                        yes = "mixtures better",
                        no = "mixtures worse")

rot.overall.50.hd = ifelse(rot.total.difference.50.hd < 0,
                           yes = "mixtures better",
                           no = "mixtures worse")

rot.overall.75.hd = ifelse(rot.total.difference.75.hd < 0,
                           yes = "mixtures better",
                           no = "mixtures worse")

rot.mix.df = data.frame(cross.resistance,
                        rot.total.difference.fd,
                        rot.total.difference.50.hd,
                        rot.total.difference.75.hd,
                        rot.overall.fd,
                        rot.overall.50.hd,
                        rot.overall.75.hd,
                        model = the.model
)

return(list(solo.mix.df, rot.mix.df))

}


polysmooth.compares = create_comparison_dataset(mixtures.df = smooth.mixtures.df,
                                                solo.df = smooth.solo.df,
                                                rotation.df = smooth.rotation.df,
                                                the.model = "polysmooth")


polytruncate.compares = create_comparison_dataset(mixtures.df = truncation.mixtures.df,
                                                  solo.df = truncation.solo.df,
                                                  rotation.df = truncation.rotation.df,
                                                  the.model = "polytruncate")



#############################
#Versus Monotherapy "solo" ::::



#fd
fd.fd.solo = polysmooth.compares[[1]]$overall.fd == polytruncate.compares[[1]]$overall.fd
#hd75
hd.75.solo = polysmooth.compares[[1]]$overall.75.hd == polytruncate.compares[[1]]$overall.75.hd
#hd50
hd.50.solo = polysmooth.compares[[1]]$overall.50.hd == polytruncate.compares[[1]]$overall.50.hd

cross.resistance = polysmooth.compares[[1]]$cross.resistance


fd = data.frame(table(solo.df$fd.fd.solo, solo.df$cross.resistance))
hd.75 = data.frame(table(solo.df$hd.75.solo, solo.df$cross.resistance))
hd.50 = data.frame(table(solo.df$hd.50.solo, solo.df$cross.resistance))

#magnitude of differences::::

#fd
fd.fd.solo.mag = polysmooth.compares[[1]]$total.difference.fd - polytruncate.compares[[1]]$total.difference.fd
#hd75
hd.75.solo.mag = polysmooth.compares[[1]]$total.difference.75.hd - polytruncate.compares[[1]]$total.difference.75.hd
#hd50
hd.50.solo.mag = polysmooth.compares[[1]]$total.difference.50.hd - polytruncate.compares[[1]]$total.difference.50.hd


solo.df = data.frame(fd.fd.solo, hd.75.solo, hd.50.solo, cross.resistance,
                     fd.fd.solo.mag, hd.75.solo.mag, hd.50.solo.mag)




p.1 = ggplot(fd, aes(x=as.factor(Var2), y = Freq,
               fill = Var1))+
  geom_col()+
  xlab("Cross Resistance")+
  ylab("Count")+
  scale_fill_manual(values = c("skyblue", "coral"))+
  ggtitle("FD_FD")+
  theme_bw()+
  theme(legend.position = "none")

p.2 = ggplot(hd.75, aes(x=as.factor(Var2), y = Freq,
               fill = Var1))+
  geom_col()+
  xlab("Cross Resistance")+
  ylab("Count")+
  scale_fill_manual(values = c("coral", "skyblue"))+
  ggtitle("HD_HD (retains 75%)")+
  theme_bw()+
  theme(legend.position = "none")

p.3 =ggplot(hd.50, aes(x=as.factor(Var2), y = Freq,
                  fill = Var1))+
  geom_col()+
  xlab("Cross Resistance")+
  ylab("Count")+
  scale_fill_manual(values = c("coral", "skyblue"))+
  ggtitle("HD_HD (retains 50%)")+
  theme_bw()+
  theme(legend.position = "none")


p.4 = ggplot(solo.df, aes(y=abs(fd.fd.solo.mag),
                    x = as.factor(cross.resistance)))+
  geom_violin()+
  xlab("Cross Resistance")+
  ylab("Difference")+
  theme_bw()

p.5 = ggplot(solo.df, aes(y=abs(hd.75.solo.mag),
                    x = as.factor(cross.resistance)))+
  geom_violin()+
  xlab("Cross Resistance")+
  ylab("Difference")+
  theme_bw()

p.6 = ggplot(solo.df, aes(y=abs(hd.50.solo.mag),
                    x = as.factor(cross.resistance)))+
  geom_violin()+
  xlab("Cross Resistance")+
  ylab("Difference")+
  theme_bw()


the.layout = "
ABC
DEF"

p.1 + p.2 + p.3 +
  p.4 + p.5 + p.6 + plot_layout(design = the.layout)




#############
#versus rotations::::::::
#fd
fd.fd.rot = polysmooth.compares[[2]]$rot.overall.fd == polytruncate.compares[[2]]$rot.overall.fd
#hd75
hd.75.rot = polysmooth.compares[[2]]$rot.overall.75.hd == polytruncate.compares[[2]]$rot.overall.75.hd
#hd50
hd.50.rot = polysmooth.compares[[2]]$rot.overall.50.hd == polytruncate.compares[[2]]$rot.overall.50.hd

cross.resistance = polysmooth.compares[[2]]$cross.resistance

rot.df = data.frame(fd.fd.rot, hd.75.rot, hd.50.rot, cross.resistance)

fd = data.frame(table(rot.df$fd.fd.rot, rot.df$cross.resistance))
hd.75 = data.frame(table(rot.df$hd.75.rot, rot.df$cross.resistance))
hd.50 = data.frame(table(rot.df$hd.50.rot, rot.df$cross.resistance))

#fd
fd.fd.rot.mag = polysmooth.compares[[2]]$rot.total.difference.fd - polytruncate.compares[[2]]$rot.total.difference.fd
#hd75
hd.75.rot.mag = polysmooth.compares[[2]]$rot.total.difference.75.hd - polytruncate.compares[[2]]$rot.total.difference.75.hd
#hd50
hd.50.rot.mag = polysmooth.compares[[2]]$rot.total.difference.50.hd - polytruncate.compares[[2]]$rot.total.difference.50.hd


rot.df = data.frame(fd.fd.rot, hd.75.rot, hd.50.rot,
                    fd.fd.rot.mag, hd.75.rot.mag, hd.50.rot.mag,
                    cross.resistance)


p.1 = ggplot(fd, aes(x=as.factor(Var2), y = Freq,
                     fill = Var1))+
  geom_col()+
  xlab("Cross Resistance")+
  ylab("Count")+
  scale_fill_manual(values = c("coral", "skyblue"))+
  ggtitle("FD_FD")+
  theme_bw()+
  theme(legend.position = "none")

p.2 = ggplot(hd.75, aes(x=as.factor(Var2), y = Freq,
                        fill = Var1))+
  geom_col()+
  xlab("Cross Resistance")+
  ylab("Count")+
  scale_fill_manual(values = c("coral", "skyblue"))+
  ggtitle("HD_HD (retains 75%)")+
  theme_bw()+
  theme(legend.position = "none")

p.3 =ggplot(hd.50, aes(x=as.factor(Var2), y = Freq,
                       fill = Var1))+
  geom_col()+
  xlab("Cross Resistance")+
  ylab("Count")+
  scale_fill_manual(values = c("coral", "skyblue"))+
  ggtitle("HD_HD (retains 50%)")+
  theme_bw()+
  theme(legend.position = "none")


p.4 = ggplot(rot.df, aes(y=abs(fd.fd.rot.mag),
                          x = as.factor(cross.resistance)))+
  geom_violin()+
  xlab("Cross Resistance")+
  ylab("Difference")+
  theme_bw()

p.5 = ggplot(rot.df, aes(y=abs(hd.75.rot.mag),
                          x = as.factor(cross.resistance)))+
  geom_violin()+
  xlab("Cross Resistance")+
  ylab("Difference")+
  theme_bw()

p.6 = ggplot(rot.df, aes(y=abs(hd.50.rot.mag),
                          x = as.factor(cross.resistance)))+
  geom_violin()+
  xlab("Cross Resistance")+
  ylab("Difference")+
  theme_bw()



the.layout = "
ABC
DEF"

p.1 + p.2 + p.3 +
  p.4 + p.5 + p.6 + plot_layout(design = the.layout)









