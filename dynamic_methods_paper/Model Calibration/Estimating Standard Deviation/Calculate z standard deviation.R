library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(devtools)
load_all()

#Data from published WHO Cylinder Assays
dataset1 = read_excel("dynamic_methods_paper/Model Calibration/Estimating Standard Deviation/WHO Bioassay with SD SE or CI.xlsx",
                      sheet = "Underpinning Sustainable Vector")

dataset2 = read_excel("dynamic_methods_paper/Model Calibration/Estimating Standard Deviation/WHO Bioassay with SD SE or CI.xlsx",
                      sheet = "Watsenga et al. Malar J  2018")

dataset3 = read_excel("dynamic_methods_paper/Model Calibration/Estimating Standard Deviation/WHO Bioassay with SD SE or CI.xlsx",
                      sheet = "Alemayehuet al. Parasites Vec")


#Estimate number of bioassays (WHO says 25 mosquitoes per assay)
#Round as bioassays are whole events
dataset1$bioassays = round(dataset1$no.mosquitoes/25)
dataset2$bioassays = round(dataset2$no.mosquitoes/25)

#Estimate number of mosquitoes:
dataset3$no.mosquitoes = dataset3$bioassays*25

dataset1$dataset = "1"
dataset2$dataset = "2"
dataset3$dataset = "3"

dataset = rbind(dataset1, dataset2, dataset3)

#Switches as now working with survival. High mortality = low survival
dataset$lower.ci.prop = 1 - dataset$Upper95CI/100
dataset$upper.ci.prop = 1 - dataset$Lower95CI/100
dataset$survival = (100 - dataset$Mortality)/100

z.value = c()
z.min.ci = c()
z.max.ci = c()

for(i in 1:196){
  z.value[i] = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                               michaelis.menten.slope = 1,
                                               half.population.bioassay.survival.resistance = 900,
                                               bioassay.survival = dataset$survival[i],
                                               estimate.precision = 0.0001,
                                               minimum.resistance.value = 0,
                                               maximum.resistance.value = 90000)


  z.min.ci[i] = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                michaelis.menten.slope = 1,
                                                half.population.bioassay.survival.resistance = 900,
                                                bioassay.survival = dataset$lower.ci.prop[i],
                                                estimate.precision = 0.0001,
                                                minimum.resistance.value = 0,
                                                maximum.resistance.value = 90000)

  z.max.ci[i] = convert_bioassay_survival_to_resistance_score(maximum.bioassay.survival.proportion = 1,
                                                michaelis.menten.slope = 1,
                                                half.population.bioassay.survival.resistance = 900,
                                                bioassay.survival = dataset$upper.ci.prop[i],
                                                estimate.precision = 0.0001,
                                                minimum.resistance.value = 0,
                                                maximum.resistance.value = 90000)
}

dataset$z.value = z.value
dataset$z.min.ci = z.min.ci
dataset$z.max.ci = z.max.ci

#https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
calculate_SD_from_CI = function(min.ci,
                                max.ci,
                                N){

  SD = sqrt(N)*(max.ci - min.ci)/3.92

  return(SD)
}

dataset$z.stdev = calculate_SD_from_CI(min.ci = dataset$z.min.ci,
                                       max.ci = dataset$z.max.ci,
                                       N = dataset$bioassays)


dataset.bioassays = dataset%>%
  dplyr::filter(bioassays >= 4)

#Z values for starting at 0 - 100::
dataset.0.100 = dataset.bioassays%>%
  dplyr::filter(survival <= 0.1)


plot.a = ggplot(dataset.0.100, aes(y=z.stdev,
                          x = Insecticide,
                          colour = Insecticide))+
  geom_boxplot(data = dataset.0.100, aes(y=z.stdev,
                                         x="All"),
               colour = "black", size = 1.2)+
  geom_boxplot(size = 1.2)+
  ylab("Standard Deviation (z) of Bioassay")+
  xlab("Insecticide")+
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x=element_text(angle=90,hjust=1))

plot.b = ggplot(dataset.0.100, aes(x=z.value, y=z.stdev,
                          colour = Insecticide))+
  geom_point()+
  ylab("Standard Deviation (z)")+
  xlab("Mean Polygenic Resistance Score (z) of Bioassay")+
  theme_classic()+
  theme(legend.position = "none")

plot.c = cowplot::get_legend(ggplot(dataset.0.100, aes(y=z.stdev,
                                                        x = Insecticide,
                                                        colour = Insecticide))+
                                geom_boxplot(data = dataset.0.100, aes(y=z.stdev,
                                                                       x="All"),
                                             colour = "black", size = 1.2)+
                                geom_boxplot(size = 1.2)+
                                ylab("Standard Deviation (z)")+
                                xlab("Mean Polygenic Resistance Score (z)")+
                                theme_classic()+
                                theme(legend.position = "bottom"))


layout = "
AAABBB
AAABBB
AAABBB
#CCCC#
"
plot.a + plot.b + plot.c +
  plot_layout(design = layout)


ggsave(
  filename = "chapter3_figureS5.1.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 800,
  height = 600,
  units = "px",
  dpi = 300)



summary(lm(z.stdev ~ z.value,
           data = dataset.0.100))


dataset.low.survival = dataset%>%
  dplyr::filter(z.value < 100)

ggplot(dataset.bioassays, aes(x=z.value,
                              y=z.stdev))+
  geom_point(colour = "black")+
  xlab("Mean Polygenic Resistance Score (z)")+
  ylab("Standard Deviation (z)")+
  theme_classic()


dataset.bioassays.3600 = dataset.bioassays%>%
  dplyr::filter(z.value < 3600) #corresponds to 80% bioassay survival


plot.d = ggplot(dataset.bioassays, aes(x=z.value, y=z.stdev))+
  geom_point(colour = "red")+
  geom_point(data = dataset.bioassays.3600, aes(x=z.value, y=z.stdev),
             colour = "blue")+
  geom_vline(xintercept = 3600, linetype = "dashed")+
  xlab("Mean Polygenic Resistance Score")+
  ylab("Standard Deviation of Polygenic Resistance Score")+
  theme_bw()

plot.e = ggplot(dataset.bioassays.3600, aes(x=z.value, y=z.stdev))+
  geom_point(colour = "blue")+
  geom_smooth(method = "lm")+
  xlab("Mean Polygenic Resistance Score")+
  ylab("Standard Deviation of Polygenic Resistance Score")+
  theme_bw()


plot.d + plot.e

ggsave(
  filename = "chapter3_figureS5.2.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 600,
  height = 300,
  units = "px",
  dpi = 300)




z.sd.lm = lm(z.stdev ~ z.value,
           data = dataset.bioassays.3600)

summary(z.sd.lm)

confint(z.sd.lm)

sd_changes_with_z = function(current.z,
                             z.sd.intercept,
                             z.sd.coefficient){


  current.sd = (current.z*z.sd.coefficient) + z.sd.intercept

}


expected.sd = sd_changes_with_z(seq(0, 3600, 0.1),
                                18.19349,
                                0.40220 )








z.sd.lm.all = lm(z.stdev ~ z.value,
             data = dataset.bioassays)

summary(z.sd.lm.all)

confint(z.sd.lm.all)




ggplot(dataset.bioassays, aes(x=z.value, y=z.stdev))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Mean Polygenic Resistance Score")+
  ylab("Standard Deviation of Polygenic Resistance Score")+
  theme_bw()

ggplot(dataset.bioassays, aes(x=z.value))+
  geom_histogram()+
  xlab("Mean Polygenic Resistance Score")+
  theme_bw()


