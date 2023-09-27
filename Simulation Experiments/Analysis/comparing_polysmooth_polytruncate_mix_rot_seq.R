library(devtools)
load_all()
library(patchwork)

polysmooth.df =  read.csv("sequence.rotation.mixture.smooth.fixedsd.csv")
polytruncate.df = read.csv("C:/Users/neilp/OneDrive - LSTM/polytruncate/polytruncate.sequence.rotation.mixture.csv")


#First set outcomes for each model
polysmooth.df$seq.rot.outcome = ifelse(polysmooth.df$sequence.duration > polysmooth.df$rotation.duration,
                                       yes = "sequence win",
                                       no = ifelse(polysmooth.df$sequence.duration < polysmooth.df$rotation.duration,
                                                   yes = "rotation win",
                                                   no = "draw"))

polysmooth.df$seq.mix.outcome = ifelse(polysmooth.df$sequence.duration > polysmooth.df$mixture.duration,
                                       yes = "sequence win",
                                       no = ifelse(polysmooth.df$sequence.duration < polysmooth.df$mixture.duration,
                                                   yes = "mixture win",
                                                   no = "draw"))

polysmooth.df$rot.mix.outcome = ifelse(polysmooth.df$rotation.duration > polysmooth.df$mixture.duration,
                                       yes = "rotation win",
                                       no = ifelse(polysmooth.df$rotation.duration < polysmooth.df$mixture.duration,
                                                   yes = "mixture win",
                                                   no = "draw"))


polytruncate.df$seq.rot.outcome = ifelse(polytruncate.df$sequence.duration > polytruncate.df$rotation.duration,
                                         yes = "sequence win",
                                         no = ifelse(polytruncate.df$sequence.duration < polytruncate.df$rotation.duration,
                                                     yes = "rotation win",
                                                     no = "draw"))

polytruncate.df$seq.mix.outcome = ifelse(polytruncate.df$sequence.duration > polytruncate.df$mixture.duration,
                                         yes = "sequence win",
                                         no = ifelse(polytruncate.df$sequence.duration < polytruncate.df$mixture.duration,
                                                     yes = "mixture win",
                                                     no = "draw"))

polytruncate.df$rot.mix.outcome = ifelse(polytruncate.df$rotation.duration > polytruncate.df$mixture.duration,
                                         yes = "rotation win",
                                         no = ifelse(polytruncate.df$rotation.duration < polytruncate.df$mixture.duration,
                                                     yes = "mixture win",
                                                     no = "draw"))

###Seq vs Rot##
#For the same parameter inputs we are able to see if there is a diverage in which strategy becomes preferable or not

cross.resistance = polysmooth.df$cross.selection

diff.outcome.rot.seq = as.factor(ifelse(polysmooth.df$seq.rot.outcome == "draw" &
                                polytruncate.df$seq.rot.outcome == "draw",
                              yes = "strategies draw",
                              no = ifelse(polysmooth.df$seq.rot.outcome == polytruncate.df$seq.rot.outcome,
                                          yes = "strategies match",
                                          no = ifelse(polysmooth.df$seq.rot.outcome  == "sequence win" &
                                                        polytruncate.df$seq.rot.outcome == "rotation win",
                                                      yes = "strategies diverge",
                                                      no = ifelse(polysmooth.df$seq.rot.outcome  == "rotation win" &
                                                                    polytruncate.df$seq.rot.outcome == "sequence win",
                                                                  yes = "strategies diverge",
                                                                  no = "win becomes draw")))))

diff.outcome.rot.seq = factor(diff.outcome.rot.seq, levels = c("strategies match", "strategies draw", "win becomes draw", "strategies diverge"))


rot.seq.df = data.frame(table(diff.outcome.rot.seq, cross.resistance))

#colours:::
#Red - strategies diverge
#grey - strategies draw
#darkgreen - strategies match
#yellow - win becomes draw
colour.palette = c("#08306b", "#9ecae1", "#fc9272", "#67000d")



plot.compare.seq.rot = ggplot(rot.seq.df, aes(x=cross.resistance,
                                              y=Freq/50, #converts to percentage
                                              fill = diff.outcome.rot.seq))+
  geom_col()+
  xlab("Cross resistance between insecticides")+
  ylab("Percentage")+
  scale_fill_manual(values = colour.palette)+
  guides(fill=guide_legend(title="Outcome"))+
  ggtitle("Sequence & Rotation")+
  theme_bw()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100),
                     breaks = seq(0, 100, 10))+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "black"),plot.margin = margin(0, 0, 0, 0, "pt"))



#seq mix
diff.outcome.seq.mix = as.factor(ifelse(polysmooth.df$seq.mix.outcome == "draw" &
                                polytruncate.df$seq.mix.outcome == "draw",
                              yes = "strategies draw",
                              no = ifelse(polysmooth.df$seq.mix.outcome == polytruncate.df$seq.mix.outcome,
                                          yes = "strategies match",
                                          no = ifelse(polysmooth.df$seq.mix.outcome  == "sequence win" &
                                                        polytruncate.df$seq.mix.outcome == "mixture win",
                                                      yes = "strategies diverge",
                                                      no = ifelse(polysmooth.df$seq.mix.outcome  == "sequence win" &
                                                                    polytruncate.df$seq.mix.outcome == "mixture win",
                                                                  yes = "strategies diverge",
                                                                  no = "win becomes draw")))))

levels(diff.outcome.seq.mix)
diff.outcome.seq.mix = factor(diff.outcome.seq.mix, levels = c("strategies match", "strategies draw", "win becomes draw", "strategies diverge"))


seq.mix.df = data.frame(table(diff.outcome.seq.mix, cross.resistance))

table(diff.outcome.seq.mix, cross.resistance)

seq.mix.df%>%
  dplyr::filter(diff.outcome.seq.mix != "strategies draw")%>%
  dplyr::group_by(cross.resistance)%>%
  dplyr::summarise(sum(Freq))


#excluding where both strategies draw.
plot.compare.seq.mix = ggplot(seq.mix.df, aes(x=cross.resistance,
                                              y=Freq/50,
                                              fill = diff.outcome.seq.mix))+
  geom_col()+
  ylab("Percentage")+
  xlab("Cross resistance between insecticides")+
  scale_fill_manual(values = colour.palette)+
  guides(fill=guide_legend(title="Outcome"))+
  ggtitle("Sequence & Mixture")+
  theme_bw()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100),
                     breaks = seq(0, 100, 10))+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "black"),
        plot.margin = margin(0, 0, 0, 0, "pt"))

#colours:::
#Red - strategies diverge
#grey - strategies draw
#darkgreen - strategies match
#yellow - win becomes draw
#excluding where both strategies draw.


##Colours:::

#Full Agreement : Dark Blue : #08306b
#Agreement : Light Blue : #9ecae1
#Partial Disagreement : Light Red : #fc9272
#Disagreement : Dark Red : #67000d


#rot mix
diff.outcome.rot.mix = as.factor(ifelse(polysmooth.df$rot.mix.outcome == "draw" &
                                polytruncate.df$rot.mix.outcome == "draw",
                              yes = "strategies draw",
                              no = ifelse(polysmooth.df$rot.mix.outcome == polytruncate.df$rot.mix.outcome,
                                          yes = "strategies match",
                                          no = ifelse(polysmooth.df$rot.mix.outcome  == "rotation win" &
                                                        polytruncate.df$rot.mix.outcome == "mixture win",
                                                      yes = "strategies diverge",
                                                      no = ifelse(polysmooth.df$rot.mix.outcome  == "rotation win" &
                                                                    polytruncate.df$rot.mix.outcome == "mixture win",
                                                                  yes = "strategies diverge",
                                                                  no = "win becomes draw")))))

diff.outcome.rot.mix = factor(diff.outcome.rot.mix, levels = c("strategies match", "strategies draw", "win becomes draw", "strategies diverge"))


rot.mix.df = data.frame(table(diff.outcome.rot.mix, cross.resistance))

rot.mix.df%>%
  dplyr::filter(diff.outcome.rot.mix != "strategies draw")%>%
  dplyr::group_by(cross.resistance)%>%
  dplyr::summarise(sum(Freq))


#excluding where both strategies draw.
plot.compare.rot.mix = ggplot(rot.mix.df, aes(x=cross.resistance,
                                              y=Freq/50,
                                              fill = diff.outcome.rot.mix))+
  geom_col()+
  xlab("Cross resistance between insecticides")+
  ylab("Percentage")+
  scale_fill_manual(values = colour.palette)+
  guides(fill=guide_legend(title="Outcome"))+
  ggtitle("Rotation & Mixture")+
  theme_bw()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100),
                     breaks = seq(0, 100, 10))+
  theme(legend.position = "none",
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "black"),
        plot.margin = margin(0, 0, 0, 0, "pt"))

#colours:::
#Red - strategies diverge
#grey - strategies draw
#darkgreen - strategies match
#yellow - win becomes draw
#excluding where both strategies draw.

figure.legend = cowplot::get_legend(ggplot(rot.seq.df, aes(x=cross.resistance,
                       y=Freq/50, #converts to percentage
                       fill = diff.outcome.rot.seq))+
  geom_col()+
  xlab("Cross resistance between insecticides")+
  ylab("Percentage")+
  scale_fill_manual(values = colour.palette,
                    labels = c("Full Agreement",
                               "Agreement",
                               "Partial Disagreement",
                               "Full Disagreement"))+
  guides(fill =guide_legend(title="Outcome",
                            nrow=1,byrow=TRUE,
                            direction = "horizontal"))+
  ggtitle("Sequence & Rotation")+
  theme_bw()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100),
                     breaks = seq(0, 100, 10))+
  theme(axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "black"),
        plot.margin = margin(0, 0, 0, 0, "pt"),
       legend.margin = margin(c(0, 0, 0, 0), unit = "cm"),
       legend.title=element_text(size=15),
       legend.text=element_text(size=15)))

the.layout= "
AAAABBBBCCCC
AAAABBBBCCCC
AAAABBBBCCCC
AAAABBBBCCCC
####DDDD####
"

plot.compare.seq.rot + plot.compare.seq.mix + plot.compare.rot.mix + figure.legend+ plot_layout(design = the.layout)





