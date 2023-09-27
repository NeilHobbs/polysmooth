#combinations::::
library(devtools)
load_all()
library(patchwork)
library(rpart)
library(rpart.plot)

irs_llin = data.table::fread("combinations.set_combinations.comparator.csv")
llin = data.table::fread("combinations.set_LLIN_only.comparator.csv")


#Negative values --> LLIN only better
#Positive values --> LLIN+IRS better

irs_llin$difference.i = llin$end.insecticide.i - irs_llin$end.insecticide.i
irs_llin$difference.j = llin$end.insecticide.j - irs_llin$end.insecticide.j
irs_llin$difference.total = irs_llin$difference.i + irs_llin$difference.j

irs_llin$outcome.i = ifelse(irs_llin$difference.i <= -0.01,
                            yes = "LLIN",
                            no = ifelse(irs_llin$difference.i >= 0.01,
                                        yes = "LLIN + IRS",
                                        no = "Draw"))

irs_llin$outcome.j = ifelse(irs_llin$difference.j <= -0.01,
                            yes = "LLIN",
                            no = ifelse(irs_llin$difference.j >= 0.01,
                                        yes = "LLIN + IRS",
                                        no = "Draw"))

irs_llin$outcome.total = ifelse(irs_llin$difference.total <= -0.01,
                                yes = "LLIN",
                                no = ifelse(irs_llin$difference.total >= 0.01,
                                            yes = "LLIN + IRS",
                                            no = "Draw"))

irs_llin$initial.pyrethroid = as.factor(rep(c("0.5", "10", "20", "50", "80"), each = 10000))



i.df = data.frame(table(irs_llin$outcome.i,
      irs_llin$initial.pyrethroid))|>
  dplyr::rename(initial.pyrethroid = "Var2")

j.df = data.frame(table(irs_llin$outcome.j,
                        irs_llin$initial.pyrethroid))|>
  dplyr::rename(initial.pyrethroid = "Var2")

ij.df = data.frame(table(irs_llin$outcome.total,
                         irs_llin$initial.pyrethroid))|>
  dplyr::rename(initial.pyrethroid = "Var2")

ggplot(subset(irs_llin,
              outcome.i != "Draw"), aes(x=difference.i * 100,
                                        fill = outcome.i))+
  scale_fill_manual(values = c("coral", "skyblue"))+
  geom_histogram(binwidth = 1)+
  geom_label(data = i.df,
             mapping = aes(x= 15, y = rep(c(1100, 700, 300), 5), label = Freq),
             inherit.aes = FALSE, fill = rep(c("grey", "coral", "skyblue"), 5))+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  xlab(paste0("LLIN Difference\nBioassay Survival (%)"))+
  facet_grid(initial.pyrethroid~.)+
  ylab("count")+
  theme_bw()+
  theme(legend.position = "none")

ggplot(subset(irs_llin,
              outcome.j != "Draw"), aes(x=difference.j * 100,
                     fill = outcome.j))+
  scale_fill_manual(values = c("coral", "skyblue"))+
  geom_histogram(binwidth = 1)+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_label(data = j.df,
             mapping = aes(x= -20, y = rep(c(1100, 600), 5), label = Freq),
             inherit.aes = FALSE, fill = rep(c("grey", "coral"), 5))+
  xlab(paste0("IRS Difference\nBioassay Survival (%)"))+
  facet_grid(initial.pyrethroid~.)+
  ylab("count")+
  theme_bw()+
  theme(legend.position = "none")

ggplot(subset(irs_llin,
              outcome.total != "Draw"), aes(x=difference.total * 100,
                     fill = outcome.total))+
  scale_fill_manual(values = c("coral", "skyblue"))+
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Initial Pyrethroid Bioassay Survival (%)",
                                         breaks = NULL,
                                         labels = NULL))+
  geom_histogram(binwidth = 1)+
  geom_label(data = ij.df,
             mapping = aes(x= -20, y = rep(c(1000, 600, 250), 5), label = Freq),
             inherit.aes = FALSE, fill = rep(c("grey", "coral", "skyblue"), 5))+
  facet_grid(initial.pyrethroid~.)+
  ylab("count")+
  xlab(paste0("LLIN + IRS Difference\nBioassay Survival (%)"))+
  theme_bw()+
  theme(legend.position = "none")




colnames(irs_llin)
regression_tree = function(the.df,
                           the.seed){


  #convert coverages and resistances to factors:::


  set.seed(the.seed)

  ## 70% of the sample size
  sample.size = floor(0.7 * nrow(the.df))

  the.df = the.df |>
    dplyr::mutate(id = 1:nrow(the.df))




  train.data = data.frame(the.df |> dplyr::sample_frac(0.70))
  test.data  = data.frame(dplyr::anti_join(the.df, train.data, by = 'id'))

  the.model = rpart(formula = outcome.total ~
                      Heritability+
                      Male.Insecticide.Exposure+
                      Female.Insecticide.Exposure+
                      Female.Fitness.Cost+
                      Male.Fitness.Cost+
                      Intervention.Coverage+
                      Dispersal+
                      Between.Gonotrophic.Survival+
                      coverage.i+
                      coverage.j +
                      coverage.ij +
                      f.encounter.i +
                      f.encounter.j +
                      f.encounter.ij +
                      m.encounter.i +
                      m.encounter.j +
                      m.encounter.ij +
                      initial.pyrethroid,
                    cp = 0,
                    control = rpart.control(minbucket = 50,
                                            maxdepth = 10,
                    ),
                    data = train.data,
                    model = TRUE,
                    method = "class")

  prediction = predict(the.model, test.data,
                       type = "class")

  model.accuracy = sum(ifelse(as.character(prediction) == as.character(test.data$outcome.total),
                              yes = 1,
                              no = 0))/(nrow(the.df) - sample.size)*100

  return(list(the.model, model.accuracy))

}


the.model = regression_tree(the.df = irs_llin,
                the.seed = 1335)


rpart.plot(the.model[[1]],
           box.palette = list("grey", "coral", "skyblue"),
           extra = 2,
           under = TRUE,
           tweak = 1.3)


the.model[[2]]


########################
# SENSITIVITY ANALYSIS #
########################

##GAMs for all Parameters stratified by initial pyrethroid resistance
sensitivity_analysis_gam = function(i){

  parameter.names= c("Heritability",
                       "Male.Insecticide.Exposure",
                       "Female.Insecticide.Exposure",
                       "Female.Fitness.Cost",
                       "Male.Fitness.Cost",
                       "Intervention.Coverage",
                       "Dispersal",
                       "Between.Gonotrophic.Survival",
                       "coverage.i",
                       "coverage.j",
                       "coverage.ij",
                       "f.encounter.i",
                       "f.encounter.j",
                       "f.encounter.ij",
                       "m.encounter.i",
                       "m.encounter.j",
                       "m.encounter.ij")

  x.axis.title = c("Heritability",
                   "Male Insecticide Exposure",
                   "Female Insecticide Exposure",
                   "Female Fitness Cost",
                   "Male Fitness Cost",
                   "Intervention Coverage",
                   "Dispersal",
                   "Between Gonotrophic Survival",
                   "Coverage LLIN Only",
                   "Coverage IRS Only",
                   "Coverage IRS + LLIN",
                   "Female Encounters LLIN Only",
                   "Female Encounters IRS Only",
                   "Female Encounters LLIN + IRS",
                   "Male Encounters LLIN Only",
                   "Male Encounters IRS Only",
                   "Male Encounters LLIN + IRS")

  irs_llin.temp = irs_llin|>
    dplyr::select("difference.i", "initial.pyrethroid",
                  parameter.names[i])

  irs_llin.temp$x.parameter = irs_llin.temp[, 3]


  temp.plot = ggplot(irs_llin.temp, aes(x= x.parameter,
                                                    y = difference.i*100,
                     fill = initial.pyrethroid,
                     colour = initial.pyrethroid))+
    geom_smooth(method = "gam")+

    scale_fill_manual(values = c(
      "green",
      "orange",
      "purple",
      "blue",
      "red"
    ))+
    scale_colour_manual(values = c(
      "green",
      "orange",
      "purple",
      "blue",
      "red"
    ))+

    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "black")+
    ylab("Difference Total End Bioassay Survival (%)")+
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
for(j in 1:17){

  plot.list[[j]] = sensitivity_analysis_gam(i=j)

}



the.legend = cowplot::get_legend(ggplot(irs_llin, aes(x= Heritability,
                                                      y = difference.i*100,
                                                      fill = initial.pyrethroid,
                                                      colour = initial.pyrethroid))+
                                   geom_smooth(method = "gam")+
                                    scale_fill_manual(values = c(
                                      "green",
                                      "orange",
                                      "purple",
                                      "blue",
                                      "red"
                                    ))+
                                    scale_colour_manual(values = c(
                                      "green",
                                      "orange",
                                      "purple",
                                      "blue",
                                      "red"))+

                                   guides(fill=guide_legend(title=paste0("Initial Pyrethroid\nBioassay Survival (%)")),
                                          colour=guide_legend(title=paste0("Initial Pyrethroid\nBioassay Survival (%)")))+
                                   theme_bw()+
                                    theme(legend.direction = "horizontal"))



the.layout = "
ABCDE
FGHIJ
KLMNO
PQ#RR
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
  plot.list[[12]]+
  plot.list[[13]]+
  plot.list[[14]]+
  plot.list[[15]]+
  plot.list[[16]]+
  plot.list[[17]]+
  the.legend+ plot_layout(design = the.layout)
















