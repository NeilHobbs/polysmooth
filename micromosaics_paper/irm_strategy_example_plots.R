
x.values = rep(seq(0, 9, 1), 10)
y.values = rep(seq(0, 9, 1), each = 10)

houses = as.factor(sample(c("present", "absent"), size = 100, replace = TRUE,
                prob = c(0.4, 0.6)))

houses.mm = as.factor(ifelse(houses == "present",
                   yes = sample(x=c("mm.1", "mm.2"), size = 2),
                   no = "absent"))

A.1 = data.frame(x.values,
                 y.values,
                 houses,
                 houses.mm)

#Rotation
rot.1 = ggplot(A.1, aes(x=x.values,
                y=y.values,
                fill = houses))+
  scale_fill_manual(values = c("white", "#cb181d"))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  geom_tile(colour = "black")+
  theme_void()+
  theme(legend.position = "none")

rot.2 = ggplot(A.1, aes(x=x.values,
                        y=y.values,
                        fill = houses))+
  scale_fill_manual(values = c("white", "#2171b5"))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  geom_tile(colour = "black")+
  theme_void()+
  theme(legend.position = "none")

#Mixtures:
mm = ggplot(A.1, aes(x=x.values,
                y=y.values,
                fill = houses.mm))+
  scale_fill_manual(values = c("white","#2171b5", "#cb181d"))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  geom_tile(colour = "black")+
  theme_void()+
  theme(legend.position = "none")



mix = ggplot(A.1, aes(x=x.values,
                       y=y.values,
                       fill = houses))+
  scale_fill_manual(values = c("white", "#3f007d"))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  geom_tile(colour = "black")+
  theme_void()+
  theme(legend.position = "none")

mm.mix = ggplot(A.1, aes(x=x.values,
                     y=y.values,
                     fill = houses.mm))+
  scale_fill_manual(values = c("white", "#3f007d", "#cb181d"))+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  geom_tile(colour = "black")+
  theme_void()+
  theme(legend.position = "none")


the.layout = "
AA#BB#CC#DD
AA#BB#CC#DD
###########
EE#FF#GG#HH
EE#FF#GG#HH
###########
II#JJ#KK#LL
II#JJ#KK#LL
###########
MM#NN#OO#PP
MM#NN#OO#PP
"

rot.1 + rot.2 + rot.1 + rot.2 +
  mm + mm + mm + mm +
  mix + mix + mix + mix +
  mm.mix + mm.mix + mm.mix + mm.mix +
  plot_layout(design = the.layout)















