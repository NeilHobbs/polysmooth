##Graphs for multiple gonotrophic cycles:::

library(devtools)
load_all()
library(patchwork)

refugia.vals = create_normal_distribution(vector.length = 1000,
                           trait.mean = 100,
                           standard.deviation = 20)

refugia.frequency.intial = calculate_density_of_trait_values(vector.length = 1000,
                                  trait.mean = 100,
                                  standard.deviation = 20)*0.3

ref.surv.rates = convert_bioassay_survival_to_field_survival(bioassay.survival = convert_resistance_score_to_bioassay_survival(trait.mean = refugia.vals,
                                                                                                                               ),
                                                             regression.coefficient = 0.48,
                                                             regression.intercept =  0.15,
                                                             current.insecticide.efficacy = 1)


intervention.vals = create_normal_distribution(vector.length = 1000,
                                               trait.mean = 120,
                                               standard.deviation = 20)

intervention.frequency.intial = calculate_density_of_trait_values(vector.length = 1000,
                                                        trait.mean = 120,
                                                        standard.deviation = 20)*0.7


int.surv.rates = (convert_bioassay_survival_to_field_survival(
  bioassay.survival = convert_resistance_score_to_bioassay_survival(
    trait.mean = intervention.vals,
    michaelis.menten.slope = 1,
  ),
  regression.coefficient = 0.48,
  regression.intercept =  0.15,
  current.insecticide.efficacy = 1))



#Do Selection
int.freq.1 = (intervention.frequency.intial * int.surv.rates)
ref.freq.1 = refugia.frequency.intial #no selection

#Do migration
int.in.int.1 = int.freq.1 * 0.7
int.in.ref.1 = int.freq.1 * 0.3
ref.in.ref.1 = int.freq.1 * 0.7
ref.in.int.1 = int.freq.1 * 0.3

#Do Selection 2::
int.in.int.2a = (int.in.int.1*0.8*int.surv.rates)
int.in.ref.2a = int.in.ref.1 * 0.8
ref.in.ref.2a = ref.in.ref.1 * 0.8
ref.in.int.2a = (ref.in.int.1*0.8*ref.surv.rates)

#Do Migration 2::
int.in.int.2b = (int.in.int.2a * 0.7) + (int.in.ref.2a * 0.3)
int.in.ref.2b = (int.in.ref.2a * 0.7) + (int.in.int.2a * 0.3)
ref.in.ref.2b = (ref.in.ref.2a * 0.7) + (ref.in.int.2a * 0.3)
ref.in.int.2b = (ref.in.int.2a * 0.7) + (ref.in.ref.2a * 0.3)

df = data.frame(refugia.frequency.intial,
                refugia.vals,
                ref.freq.1,
                intervention.frequency.intial,
                intervention.vals,
                int.freq.1,
                int.in.int.1,
                int.in.ref.1,
                ref.in.ref.1,
                ref.in.int.1,
                int.in.int.2a,
                int.in.ref.2a,
                ref.in.ref.2a,
                ref.in.int.2a,
                int.in.int.2b,
                int.in.ref.2b,
                ref.in.ref.2b,
                ref.in.int.2b)


#Emerging Populations :::
emerging.refugia.plot = ggplot(df, aes(y=refugia.frequency.intial,
                                       x=refugia.vals))+
  geom_line(size = 2, colour = "blue")+
  geom_vline(colour = "blue", xintercept = 100,
             linetype = "dashed", size = 1.25)+
  ylim(0, 0.014)+
  xlim(35, 185)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency")+
  theme_bw()+
  theme(plot.background = element_rect(fill="darkgreen"),
        axis.text = element_blank(), axis.ticks = element_blank())

emerging.intervention.plot = ggplot(df, aes(y=intervention.frequency.intial,
                                       x=intervention.vals))+
  geom_line(size = 2, colour = "red")+
  geom_vline(colour = "red", xintercept = 120,
             linetype = "dashed", size = 1.25)+
  ylim(0, 0.014)+
  xlim(35, 185)+
  xlab("Polygenic Resistance Score")+
  ylab("Frequency")+
  theme_bw()+
  theme(plot.background = element_rect(fill="purple"),
        axis.text = element_blank(), axis.ticks = element_blank())

##Selection happens::

int.selection.1.plot = ggplot(df, aes(y=int.freq.1,
                                 x=intervention.vals))+
  geom_line(size = 2, colour = "red")+
  geom_vline(colour = "red", xintercept = 120,
             linetype = "dashed", size = 1.25)+
  geom_vline(colour = "darkred", xintercept = 125,
             linetype = "dashed", size = 1.25)+
  xlab(" ")+
  ylab(" ")+
  ylim(0, 0.014)+
  xlim(35, 185)+
  theme_bw()+theme(plot.background = element_rect(fill="purple"),
                   axis.text = element_blank(), axis.ticks = element_blank())



#Then do migration:::
##Laying eggs in Intervention G1:::
 laying.g1.int.plot =  ggplot(df, aes(x=intervention.vals,
                 y=int.in.int.1))+
    geom_line(colour = "red", size = 2)+
    geom_line(aes(x=refugia.vals,
                  y=ref.in.int.1),
              colour = "blue", size = 2)+
   geom_vline(colour = "blue", xintercept = 100,
              linetype = "dashed", size = 1.25)+
   geom_vline(colour = "red", xintercept = 120,
              linetype = "dashed", size = 1.25)+
   geom_vline(colour = "darkred", xintercept = 125,
              linetype = "dashed", size = 1.25)+
   xlab(" ")+
   ylab(" ")+
    ylim(0, 0.014)+
    xlim(35, 185)+
    theme_bw()+theme(plot.background = element_rect(fill="purple"),
                     axis.text = element_blank(), axis.ticks = element_blank())


  ##Laying eggs in Refugia G1:::
  laying.g1.refugia.plot = ggplot(df, aes(x=intervention.vals,
                 y=int.in.ref.1))+
    geom_line(colour = "red", size = 2)+
    geom_line(aes(x=refugia.vals,
                  y=ref.in.ref.1),
              colour = "blue", size = 2)+
    geom_vline(colour = "blue", xintercept = 100,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "red", xintercept = 120,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "darkred", xintercept = 125,
               linetype = "dashed", size = 1.25)+
    ylim(0, 0.014)+
    xlim(35, 185)+
    xlab(" ")+
    ylab(" ")+
    theme_bw()+theme(plot.background = element_rect(fill="darkgreen"),
                     axis.text = element_blank(), axis.ticks = element_blank())



###Now do second round of selection:::
  selection.2.plot = ggplot(df, aes(x=intervention.vals,
                 y=int.in.int.2a))+
    geom_line(colour = "red", size = 2)+
    geom_line(aes(x=refugia.vals,
                  y=ref.in.int.2a),
              colour = "blue", size = 2)+
    geom_vline(colour = "blue", xintercept = 100,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "darkblue", xintercept = 105,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "red", xintercept = 120,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "darkred", xintercept = 130,
               linetype = "dashed", size = 1.25)+
    ylim(0, 0.014)+
    xlim(35, 185)+
    xlab(" ")+
    ylab(" ")+
    theme_bw()+
    theme(plot.background = element_rect(fill="purple"),
          axis.text = element_blank(), axis.ticks = element_blank())


#No selection in refugia, just natural mortality
  natural.mortality.refugia = ggplot(df, aes(x=intervention.vals,
                 y=ref.in.int.2a))+
    geom_line(colour = "red", size = 2)+
    geom_line(aes(x=refugia.vals,
                  y=ref.in.ref.2a),
              colour = "blue", size = 2)+
    geom_vline(colour = "blue", xintercept = 100,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "red", xintercept = 120,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "darkred", xintercept = 125,
               linetype = "dashed", size = 1.25)+
    ylim(0, 0.014)+
    xlim(35, 185)+
    xlab(" ")+
    ylab(" ")+
    theme_bw()+
    theme(plot.background = element_rect(fill="darkgreen"),
          axis.text = element_blank(), axis.ticks = element_blank())


##Now do second round of migration (to where they lay eggs):::

  laying.g2.intervention = ggplot(df, aes(x=intervention.vals,
                 y=int.in.int.2b))+
    geom_line(colour = "red", size = 2)+
    geom_line(aes(x=refugia.vals,
                  y=ref.in.int.2b),
              colour = "blue", size = 2)+
    geom_vline(colour = "blue", xintercept = 100,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "darkblue", xintercept = 103,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "red", xintercept = 120,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "darkred", xintercept = 127,
               linetype = "dashed", size = 1.25)+
    ylim(0, 0.014)+
    xlim(35, 185)+
    xlab(" ")+
    ylab(" ")+
    theme_bw()+
    theme(plot.background = element_rect(fill="purple"),
          axis.text = element_blank(), axis.ticks = element_blank())



  laying.g2.refugia = ggplot(df, aes(x=intervention.vals,
                 y=int.in.ref.2b))+
    geom_line(colour = "red", size = 2)+
    geom_line(aes(x=refugia.vals,
                  y=ref.in.ref.2b),
              colour = "blue", size = 2)+
    geom_vline(colour = "blue", xintercept = 100,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "darkblue", xintercept = 102,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "red", xintercept = 120,
               linetype = "dashed", size = 1.25)+
    geom_vline(colour = "darkred", xintercept = 126,
               linetype = "dashed", size = 1.25)+
    ylim(0, 0.014)+
    xlim(35, 185)+
    xlab(" ")+
    ylab(" ")+
    theme_bw()+theme(plot.background = element_rect(fill="darkgreen"),
                     axis.text = element_blank(), axis.ticks = element_blank())



 library(patchwork)

the.layout = "
AAA#CCC#DDD#FFF#HHH
AAA#CCC#DDD#FFF#HHH
BBB#####EEE#GGG#III
BBB#####EEE#GGG#III
"



emerging.intervention.plot+
emerging.refugia.plot+
int.selection.1.plot+
laying.g1.int.plot+
laying.g1.refugia.plot+
selection.2.plot+
natural.mortality.refugia+
laying.g2.intervention+
laying.g2.refugia + plot_layout(design = the.layout)


sum(int.in.ref.2b)
sum(int.in.int.2b)

