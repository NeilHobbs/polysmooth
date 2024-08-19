#####################################################
# Analysis script to look at the IRM value of IRS ###
#####################################################

library(ggplot2)
library(patchwork)

irs_1 = data.table::fread(".\\how.many.irs.is.mixture_1.csv")
irs_2 = data.table::fread(".\\how.many.irs.is.mixture_1_2.csv")
irs_3 = data.table::fread(".\\how.many.irs.is.mixture_3.csv")
irs_4 = data.table::fread(".\\how.many.irs.is.mixture_4.csv")
half_dose_mixtures = data.table::fread(".\\how.many.irs.is.mixture_half_dose_mixtures.csv")


##Mixture Full Dose:::
mix.i = irs_2$mix.full.1 * 100
mix.j = irs_2$mix.full.2 * 100
mix.end.total = mix.i + mix.j


#mixtures half dose 50%
mix.i.50 = half_dose_mixtures$half.mix.50.1 * 100
mix.j.50 = half_dose_mixtures$half.mix.50.2 * 100
mix.end.total.50 = mix.i.50 + mix.j.50



#Mixtires half dose 75%
mix.i.75= half_dose_mixtures$half.mix.75.1 * 100
mix.j.75 = half_dose_mixtures$half.mix.75.2 * 100
mix.end.total.75 = mix.i.75 + mix.j.75



#combinations --> 1 irs
combi1.i = irs_1$combination.1.1 * 100
combi1.j = irs_1$combination.1.2 * 100
combi1.end.total = combi1.i + combi1.j
combi1.irs.diff = mix.j - combi1.j
combi1.irs.diff.50 = mix.j.50 - combi1.j
combi1.irs.diff.75 = mix.j.75 - combi1.j

#combinations --> 2 irs
combi2.i = irs_2$combination.2.1 * 100
combi2.j = irs_2$combination.2.2 * 100
combi2.k = irs_2$combination.2.3 * 100

mean.irs.2 = (combi2.j + combi2.k)/2

combi2.end.total = combi2.i + combi2.j + combi2.k
combi2.irs.diff = mix.j - mean.irs.2
combi2.irs.diff.50 = mix.j.50 - mean.irs.2
combi2.irs.diff.75 = mix.j.75 - mean.irs.2
combi2.end.total = combi2.i + mean.irs.2


#combination --> 3 irs
combi3.i = irs_3$combination.3.1 * 100
combi3.j = irs_3$combination.3.2 * 100
combi3.k = irs_3$combination.3.3 * 100
combi3.l = irs_3$combination.3.4 * 100

mean.irs.3 = (combi3.j + combi3.k + combi3.l)/3
combi3.end.total = combi3.i + mean.irs.3
combi3.irs.diff = mix.j - mean.irs.3
combi3.irs.diff.50 = mix.j.50 - mean.irs.3
combi3.irs.diff.75 = mix.j.75 - mean.irs.3


#combination --> 4 irs
combi4.i = irs_4$combination.4.1 * 100
combi4.j = irs_4$combination.4.2 * 100
combi4.k = irs_4$combination.4.3 * 100
combi4.l = irs_4$combination.4.4 * 100
combi4.m = irs_4$combination.4.5 * 100

mean.irs.4 = (combi4.j + combi4.k + combi4.l + combi4.m)/4
combi4.end.total = combi4.i + mean.irs.4
combi4.irs.diff = mix.j - mean.irs.4
combi4.irs.diff.50 = mix.j.50 - mean.irs.4
combi4.irs.diff.75 = mix.j.75 - mean.irs.4





df.1 = data.frame(mix.end.total,
                  mix.end.total.50,
                  mix.end.total.75,
                  combi1.end.total,
                  combi2.end.total, combi3.end.total,
                  combi4.end.total)

df.i = data.frame(mix.i, mix.i.50,
                  mix.i.75,
                  combi1.i,
                  combi2.i, combi3.i)

df.mean.irs = data.frame(mix.j, combi1.j,
                         mean.irs.2,
                         mean.irs.3,
                         combi1.irs.diff,
                         combi2.irs.diff,
                         combi3.irs.diff,
                         combi4.irs.diff,
                         combi1.irs.diff.50,
                         combi2.irs.diff.50,
                         combi3.irs.diff.50,
                         combi4.irs.diff.50,
                         combi1.irs.diff.75,
                         combi2.irs.diff.75,
                         combi3.irs.diff.75,
                         combi4.irs.diff.75)

irs.diff = c(combi1.irs.diff, combi2.irs.diff, combi3.irs.diff)
number.irs = rep(c(1,2,3), each = 30000)
df.irs.diff = data.frame(irs.diff, number.irs)

############FULL DOSE MIXTURES########

######################################
# "total amount of resistance" #######
######################################

total.combi.1 = ggplot(df.1, aes(x=mix.end.total - combi1.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total - combi1.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total - combi1.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 5),
                     breaks = seq(-30, 5, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))


total.combi.2 = ggplot(df.1, aes(x=mix.end.total - combi2.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total - combi2.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total - combi2.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 5),
                     breaks = seq(-30, 5, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

total.combi.3 = ggplot(df.1, aes(x=mix.end.total - combi3.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total - combi3.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total - combi3.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 5),
                     breaks = seq(-30, 5, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

total.combi.4 = ggplot(df.1, aes(x=mix.end.total - combi4.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total - combi4.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total - combi4.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 5),
                     breaks = seq(-30, 5, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))



# total.combi.1 / total.combi.2 / total.combi.3 / total.combi.4

############################################
# Comparative Protection for Insecticide i #
############################################

protect.i.1 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i - combi1.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i - combi1.i)),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i - combi1.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-20, 5),
                     breaks = seq(-20, 5, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))


protect.i.2 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i - combi2.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i - combi2.i)),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i - combi2.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-20, 5),
                     breaks = seq(-20, 5, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

protect.i.3 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i - combi3.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i - combi3.i)),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i - combi3.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-20, 5),
                     breaks = seq(-20, 5, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

protect.i.4 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i - combi4.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i - combi4.i)),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i - combi4.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-20, 5),
                     breaks = seq(-20, 5, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

# protect.i.1 / protect.i.2 / protect.i.3 / protect.i.4

################################################
# Resistance to Insecticide Partner ############
################################################

partner.1 = ggplot(df.1)+
  geom_histogram(aes(x = combi1.irs.diff),
                 alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i - combi1.i)), #as i and j have same proporteies they are equal
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i - combi1.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticide")+
  ggtitle("1 IRS Available")+
  scale_x_continuous(limits = c(-20, 10),
                     breaks = seq(-20, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

partner.2 = ggplot(df.1)+
  geom_histogram(aes(x = combi2.irs.diff,
  ),
  alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(combi2.irs.diff),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(combi2.irs.diff),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticides")+
  ggtitle("2 IRS Available")+
  scale_x_continuous(limits = c(-20, 10),
                     breaks = seq(-20, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

partner.3 = ggplot(df.1)+
  geom_histogram(aes(x = combi3.irs.diff),
                 alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(combi3.irs.diff),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(combi3.irs.diff),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticides")+
  ggtitle("3 IRS Available")+
  scale_x_continuous(limits = c(-20, 10),
                     breaks = seq(-20, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

partner.4 = ggplot(df.1)+
  geom_histogram(aes(x = combi4.irs.diff),
                 alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(combi4.irs.diff),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(combi4.irs.diff),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticides")+
  ggtitle("4 IRS Available")+
  scale_x_continuous(limits = c(-20, 10),
                     breaks = seq(-20, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))



# partner.1 / partner.2 / partner.3 / partner.4


the.layout  = "
AEI
BFJ
CGK
DHL"

# the.layout  = "
# ADG
# BEH
# CFI"

protect.i.1 + protect.i.2 + protect.i.3 + protect.i.4+
  partner.1 + partner.2 + partner.3+ partner.4+
  total.combi.1 + total.combi.2 + total.combi.3+ total.combi.4+
  plot_layout(design = the.layout) + plot_annotation(title = "Combination ITN + IRS vs Full Dose Mixture ITN")

ggsave(
  filename = "combinations_vs_fulldose_mixtures.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 1600,
  height = 1200,
  units = "px",
  dpi = 600)


# ggsave(
#   filename = "chapter7_figure5.jpeg",
#   plot = last_plot(),
#   scale = 5,
#   width = 800,
#   height = 800,
#   units = "px",
#   dpi = 400)




###########HALF DOSE MIXTURES 50######

######################################
# "total amount of resistance" #######
######################################


total.combi.1 = ggplot(df.1, aes(x=mix.end.total.50 - combi1.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total.50 - combi1.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total.50 - combi1.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 30),
                     breaks = seq(-30, 30, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))


total.combi.2 = ggplot(df.1, aes(x=mix.end.total.50 - combi2.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total.50 - combi2.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total.50 - combi2.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 30),
                     breaks = seq(-30, 30, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

total.combi.3 = ggplot(df.1, aes(x=mix.end.total.50 - combi3.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total.50 - combi3.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total.50 - combi3.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 30),
                     breaks = seq(-30, 30, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

total.combi.4 = ggplot(df.1, aes(x=mix.end.total.50 - combi4.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total.50 - combi4.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total.50 - combi4.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 30),
                     breaks = seq(-30, 30, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))



# total.combi.1 / total.combi.2 / total.combi.3 / total.combi.4

############################################
# Comparative Protection for Insecticide i #
############################################

protect.i.1 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i.50 - combi1.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i.50 - combi1.i)),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i.50 - combi1.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-15, 15),
                     breaks = seq(-15, 15, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))


protect.i.2 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i.50 - combi2.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i.50 - combi2.i)),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i.50 - combi2.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-15, 15),
                     breaks = seq(-15, 15, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

protect.i.3 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i.50 - combi3.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i.50 - combi3.i)),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i.50 - combi3.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-15, 15),
                     breaks = seq(-15, 15, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

protect.i.4 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i.50 - combi4.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i.50 - combi4.i)),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i.50 - combi4.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-15, 15),
                     breaks = seq(-15, 15, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

# protect.i.1 / protect.i.2 / protect.i.3 / protect.i.4

################################################
# Resistance to Insecticide Partner ############
################################################

partner.1 = ggplot(df.1)+
  geom_histogram(aes(x = combi1.irs.diff.50),
                 alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i.50 - combi1.i)), #i and j have equal properties in the mixture so are identical
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i.50 - combi1.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticide")+
  ggtitle("1 IRS Available")+
  scale_x_continuous(limits = c(-25, 20),
                     breaks = seq(-25, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

partner.2 = ggplot(df.1)+
  geom_histogram(aes(x = combi2.irs.diff.50,
  ),
  alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(combi2.irs.diff.50),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(combi2.irs.diff.50),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticides")+
  ggtitle("2 IRS Available")+
  scale_x_continuous(limits = c(-25, 20),
                     breaks = seq(-25, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

partner.3 = ggplot(df.1)+
  geom_histogram(aes(x = combi3.irs.diff.50),
                 alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(combi3.irs.diff.50),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(combi3.irs.diff.50),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticides")+
  ggtitle("3 IRS Available")+
  scale_x_continuous(limits = c(-25, 20),
                     breaks = seq(-25, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

partner.4 = ggplot(df.1)+
  geom_histogram(aes(x = combi4.irs.diff.50),
                 alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(combi4.irs.diff.50),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(combi4.irs.diff.50),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticides")+
  ggtitle("4 IRS Available")+
  scale_x_continuous(limits = c(-25, 20),
                     breaks = seq(-25, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))



the.layout  = "
AEI
BFJ
CGK
DHL"


protect.i.1 + protect.i.2 + protect.i.3 + protect.i.4+
  partner.1 + partner.2 + partner.3+ partner.4+
  total.combi.1 + total.combi.2 + total.combi.3+ total.combi.4+
  plot_layout(design = the.layout) + plot_annotation(title = "Combination ITN + IRS vs Half Dose (retains 50% efficacy) Mixture ITN")


ggsave(
  filename = "combinations_vs_halfdose50_mixtures.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 1600,
  height = 1200,
  units = "px",
  dpi = 600)



###########HALF DOSE MIXTURES 75######
######################################
# "total amount of resistance" #######
######################################


total.combi.1 = ggplot(df.1, aes(x=mix.end.total.75 - combi1.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total.75 - combi1.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total.75 - combi1.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 30),
                     breaks = seq(-30, 30, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))


total.combi.2 = ggplot(df.1, aes(x=mix.end.total.75 - combi2.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total.75 - combi2.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total.75 - combi2.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 30),
                     breaks = seq(-30, 30, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

total.combi.3 = ggplot(df.1, aes(x=mix.end.total.75 - combi3.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total.75 - combi3.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total.75 - combi3.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 30),
                     breaks = seq(-30, 30, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

total.combi.4 = ggplot(df.1, aes(x=mix.end.total.75 - combi4.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(mix.end.total.75 - combi4.end.total),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(mix.end.total.75 - combi4.end.total),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black", linewidth = 1.2)+
  xlab("Total Difference for Combinations vs Mixtures")+
  scale_x_continuous(limits = c(-30, 30),
                     breaks = seq(-30, 30, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))



# total.combi.1 / total.combi.2 / total.combi.3 / total.combi.4

############################################
# Comparative Protection for Insecticide i #
############################################

protect.i.1 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i.75 - combi1.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i.75 - combi1.i)), #i and j are identical when in mixture
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i.75 - combi1.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-15, 15),
                     breaks = seq(-15, 15, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))


protect.i.2 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i.75 - combi2.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i.75 - combi2.i)),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i.75 - combi2.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-15, 15),
                     breaks = seq(-15, 15, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

protect.i.3 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i.75 - combi3.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i.75 - combi3.i)),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i.75 - combi3.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-15, 15),
                     breaks = seq(-15, 15, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

protect.i.4 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i.75 - combi4.i),
                 alpha = 1, fill = "coral", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i.75 - combi4.i)),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i.75 - combi4.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in ITN Insecticide")+
  scale_x_continuous(limits = c(-15, 15),
                     breaks = seq(-15, 15, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

# protect.i.1 / protect.i.2 / protect.i.3 / protect.i.4

################################################
# Resistance to Insecticide Partner ############
################################################

partner.1 = ggplot(df.1)+
  geom_histogram(aes(x = combi1.irs.diff.75),
                 alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean((mix.i.75 - combi1.i)), #i and j are identical when in mixture
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median((mix.i.75 - combi1.i)),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticide")+
  ggtitle("1 IRS Available")+
  scale_x_continuous(limits = c(-25, 20),
                     breaks = seq(-25, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

partner.2 = ggplot(df.1)+
  geom_histogram(aes(x = combi2.irs.diff.75,
  ),
  alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(combi2.irs.diff.75),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(combi2.irs.diff.75),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticides")+
  ggtitle("2 IRS Available")+
  scale_x_continuous(limits = c(-25, 20),
                     breaks = seq(-25, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

partner.3 = ggplot(df.1)+
  geom_histogram(aes(x = combi3.irs.diff.75),
                 alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(combi3.irs.diff.75),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(combi3.irs.diff.75),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticides")+
  ggtitle("3 IRS Available")+
  scale_x_continuous(limits = c(-25, 20),
                     breaks = seq(-25, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))

partner.4 = ggplot(df.1)+
  geom_histogram(aes(x = combi4.irs.diff.75),
                 alpha = 1, fill = "skyblue", binwidth = 1, colour = "black")+
  geom_vline(xintercept = mean(combi4.irs.diff.75),
             linetype = "dashed", colour = "green", linewidth = 1.2)+
  geom_vline(xintercept = median(combi4.irs.diff.75),
             linetype = "dashed", colour = "orange", linewidth = 1.2)+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black", linewidth = 1.2)+
  xlab("Difference in Parter Insecticides")+
  ggtitle("4 IRS Available")+
  scale_x_continuous(limits = c(-25, 20),
                     breaks = seq(-25, 10, 5))+
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()+
  theme(axis.text = element_text(size = 12 , colour = "black"),
        axis.title = element_text(size = 12 , colour = "black"))



the.layout  = "
AEI
BFJ
CGK
DHL"


protect.i.1 + protect.i.2 + protect.i.3 + protect.i.4+
  partner.1 + partner.2 + partner.3+ partner.4+
  total.combi.1 + total.combi.2 + total.combi.3+ total.combi.4+
  plot_layout(design = the.layout) + plot_annotation(title = "Combination ITN + IRS vs Half Dose (retains 75% efficacy) Mixture ITN")


ggsave(
  filename = "combinations_vs_halfdose75_mixtures.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 1600,
  height = 1200,
  units = "px",
  dpi = 600)










####################################
#### Sensitvity Analysis ###########
####################################

diff.combi1.end.total  = mix.end.total - combi1.end.total
diff.combi2.end.total  = mix.end.total - combi2.end.total
diff.combi3.end.total  = mix.end.total - combi3.end.total

sensitivty.analysis.df = data.frame(irs_2[, c(10:20)],
                                    diff.combi1.end.total,
                                    diff.combi2.end.total,
                                    diff.combi3.end.total)


sensitivity_analysis_gam = function(i){

  parameter.names= c("coverages.llin",
                     "coverages.irs",
                     "coverages.both",
                     "encounter.llin",
                     "encounter.irs",
                     "encounter.both",
                     "Heritability",
                     "Male.Insecticide.Exposure",
                     "Female.Insecticide.Exposure",
                     "Intervention.Coverage",
                     "Dispersal")

  x.axis.title = c("Coverage ITN Only",
                   "Coverage IRS Only",
                   "Coverage Both",
                   "Encounter ITN Only",
                   "Encounter IRS Only",
                   "Encounter Both",
                   "Heritability",
                   "Male Insecticide Exposure",
                   "Female Insecticide Exposure",
                   "Intervention Coverage",
                   "Dispersal")

  model.method = c(rep("lm", 6), rep("gam", 5))

  sensitivty.analysis.df.temp = sensitivty.analysis.df|>
    dplyr::select("diff.combi1.end.total",
                  "diff.combi2.end.total",
                  "diff.combi3.end.total",
                  parameter.names[i])

  sensitivty.analysis.df.temp$x.parameter = sensitivty.analysis.df.temp[, 4]


  temp.plot = ggplot(sensitivty.analysis.df.temp, aes(x= x.parameter,
                                        y = diff.combi1.end.total))+
    geom_smooth(method = model.method[i],
                fill = "blue",
                colour = "blue")+
    geom_smooth(method = model.method[i], aes(x=x.parameter,
                                    y=diff.combi2.end.total,
                                    fill = "red",
                                    colour = "red"))+
    geom_smooth(method = model.method[i], aes(x=x.parameter,
                                    y=diff.combi3.end.total),
                                    fill = "orange",
                                    colour = "orange")+
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "black")+
    ylab("Difference Total End Bioassay Survival")+
    xlab(paste0(x.axis.title[i]))+
    theme_bw()+
    theme(axis.title.y =  element_blank(),
          axis.title.x =  element_text(size = 10),
          axis.text.x = element_text(size = 8, colour = "black", angle = 90),
          axis.text.y = element_text(size = 8, colour = "black"),
          legend.position = "none")



  return(temp.plot)
}



plot.list = list()
for(j in 1:11){

  plot.list[[j]] = sensitivity_analysis_gam(i=j)

}


the.layout = "
ABCD
EFGH
IJK#
"
plot.list[[1]]+
  plot.list[[2]]+
  plot.list[[3]]+
  plot.list[[4]]+
  plot.list[[5]]+
  plot.list[[6]]+
  plot.list[[7]]+
  plot.list[[8]]+
  plot.list[[9]]+
  plot.list[[10]]+
  plot.list[[11]]+
 plot_layout(design = the.layout)

ggsave(
  filename = "chapter7_figure~6.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 800,
  height = 600,
  units = "px",
  dpi = 300)
