#####################################################
# Analysis script to look at the IRM value of IRS ###
#####################################################

library(ggplot2)
library(patchwork)

irs_1 = data.table::fread(".\\how.many.irs.is.mixture_1.csv")
irs_2 = data.table::fread(".\\how.many.irs.is.mixture_1_2.csv")
irs_3 = data.table::fread(".\\how.many.irs.is.mixture_3.csv")

##Mixture Full Dose:::
mix.i = irs_2$mix.full.1 * 100
mix.j = irs_2$mix.full.2 * 100
mix.end.total = mix.i + mix.j


#combinations --> 1 irs
combi1.i = irs_1$combination.1.1 * 100
combi1.j = irs_1$combination.1.2 * 100
combi1.end.total = combi1.i + combi1.j
combi1.irs.diff = mix.j - combi1.j

#combinations --> 2 irs
combi2.i = irs_2$combination.2.1 * 100
combi2.j = irs_2$combination.2.2 * 100
combi2.k = irs_2$combination.2.3 * 100

mean.irs.2 = (combi2.j + combi2.k)/2

combi2.end.total = combi2.i + combi2.j + combi2.k
combi2.irs.diff = mix.j - mean.irs.2
combi2.end.total = combi2.i + mean.irs.2


#combination --> 3 irs
combi3.i = irs_3$combination.3.1 * 100
combi3.j = irs_3$combination.3.2 * 100
combi3.k = irs_3$combination.3.3 * 100
combi3.l = irs_3$combination.3.4 * 100

mean.irs.3 = (combi3.j + combi3.k + combi3.l)/3
combi3.end.total = combi3.i + mean.irs.3
combi3.irs.diff = mix.j - mean.irs.3

df.1 = data.frame(mix.end.total, combi1.end.total,
                  combi2.end.total, combi3.end.total)

df.i = data.frame(mix.i, combi1.i,
                  combi2.i, combi3.i)

df.mean.irs = data.frame(mix.j, combi1.j,
                         mean.irs.2,
                         mean.irs.3,
                         combi1.irs.diff,
                         combi2.irs.diff,
                         combi3.irs.diff)

irs.diff = c(combi1.irs.diff, combi2.irs.diff, combi3.irs.diff)
number.irs = rep(c(1,2,3), each = 30000)
df.irs.diff = data.frame(irs.diff, number.irs)

######################################
# "total amount of resistance" #######
######################################

total.combi.1 = ggplot(df.1, aes(x=mix.end.total - combi1.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 0.1)+
  geom_vline(xintercept = mean(mix.end.total - combi1.end.total),
             linetype = "dashed", colour = "green")+
  geom_vline(xintercept = median(mix.end.total - combi1.end.total),
             linetype = "dashed", colour = "orange")+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  ggtitle("1 IRS Available")+
  xlab("Total Difference for Combinations vs Mixtures")+
  xlim(-30, 5)+
  theme_bw()

total.combi.2 = ggplot(df.1, aes(x=mix.end.total - combi2.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 0.1)+
  geom_vline(xintercept = mean(mix.end.total - combi2.end.total),
             linetype = "dashed", colour = "green")+
  geom_vline(xintercept = median(mix.end.total - combi2.end.total),
             linetype = "dashed", colour = "orange")+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  ggtitle("2 IRS Available")+
  xlab("Total Difference for Combinations vs Mixtures")+
  xlim(-30, 5)+
  theme_bw()

total.combi.3 = ggplot(df.1, aes(x=mix.end.total - combi3.end.total))+
  geom_histogram(alpha = 1, fill = "#af8dc3", binwidth = 0.1)+
  geom_vline(xintercept = mean(mix.end.total - combi3.end.total),
             linetype = "dashed", colour = "green")+
  geom_vline(xintercept = median(mix.end.total - combi3.end.total),
             linetype = "dashed", colour = "orange")+
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "black")+
  ggtitle("3 IRS Available")+
  xlab("Total Difference for Combinations vs Mixtures")+
  xlim(-30, 5)+
  theme_bw()

total.combi.1 / total.combi.2 / total.combi.3

############################################
# Comparative Protection for Insecticide i #
############################################

protect.i.1 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i - combi1.i),
                 alpha = 1, fill = "coral", binwidth = 0.1)+
  geom_vline(xintercept = mean((mix.i - combi1.i)),
             linetype = "dashed", colour = "green")+
  geom_vline(xintercept = median((mix.i - combi1.i)),
             linetype = "dashed", colour = "orange")+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black")+
  xlab("Difference in LLIN Insecticide")+
  ggtitle("1 IRS Available")+
  xlim(-20, 5)+
  theme_bw()

protect.i.2 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i - combi2.i),
                 alpha = 1, fill = "coral", binwidth = 0.1)+
  geom_vline(xintercept = mean((mix.i - combi2.i)),
             linetype = "dashed", colour = "green")+
  geom_vline(xintercept = median((mix.i - combi2.i)),
             linetype = "dashed", colour = "orange")+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black")+
  xlab("Difference in LLIN Insecticide")+
  ggtitle("2 IRS Available")+
  xlim(-20, 5)+
  theme_bw()

protect.i.3 = ggplot(df.1)+
  geom_histogram(aes(x = mix.i - combi3.i),
                 alpha = 1, fill = "coral", binwidth = 0.1)+
  geom_vline(xintercept = mean((mix.i - combi3.i)),
             linetype = "dashed", colour = "green")+
  geom_vline(xintercept = median((mix.i - combi3.i)),
             linetype = "dashed", colour = "orange")+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black")+
  xlab("Difference in LLIN Insecticide")+
  ggtitle("3 IRS Available")+
  xlim(-20, 5)+
  theme_bw()

protect.i.1 / protect.i.2 / protect.i.3

################################################
# Resistance to Insecticide Partner ############
################################################

partner.1 = ggplot(df.1)+
  geom_histogram(aes(x = combi1.irs.diff),
                 alpha = 1, fill = "skyblue", binwidth = 0.1)+
  geom_vline(xintercept = mean((mix.i - combi1.i)),
             linetype = "dashed", colour = "green")+
  geom_vline(xintercept = median((mix.i - combi1.i)),
             linetype = "dashed", colour = "orange")+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black")+
  xlab("Difference in Parter Insecticide")+
  ggtitle("1 IRS Available")+
  xlim(-20, 5)+
  theme_bw()

partner.2 = ggplot(df.1)+
  geom_histogram(aes(x = combi2.irs.diff,
  ),
  alpha = 1, fill = "skyblue", binwidth = 0.1)+
  geom_vline(xintercept = mean(combi2.irs.diff),
             linetype = "dashed", colour = "green")+
  geom_vline(xintercept = median(combi2.irs.diff),
             linetype = "dashed", colour = "orange")+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black")+
  xlab("Difference in Parter Insecticide")+
  ggtitle("2 IRS Available")+
  xlim(-20, 5)+
  theme_bw()

partner.3 = ggplot(df.1)+
  geom_histogram(aes(x = combi3.irs.diff),
                 alpha = 1, fill = "skyblue", binwidth = 0.1)+
  geom_vline(xintercept = mean(combi3.irs.diff),
             linetype = "dashed", colour = "green")+
  geom_vline(xintercept = median(combi3.irs.diff),
             linetype = "dashed", colour = "orange")+
  geom_vline(xintercept = 0,
             linetype = "dashed", colour = "black")+
  xlab("Difference in Parter Insecticide")+
  ggtitle("3 IRS Available")+
  xlim(-20, 5)+
  theme_bw()

partner.1 / partner.2 / partner.3


the.layout  = "
ADH
BEI
CFJ"

protect.i.1 + protect.i.2 + protect.i.3+
  partner.1 + partner.2 + partner.3+
  total.combi.1 + total.combi.2 + total.combi.3+
  plot_layout(design = the.layout)



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

  x.axis.title = c("Coverage LLIN Only",
                   "Coverage IRS Only",
                   "Coverage Both",
                   "Encounter LLIN Only",
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


