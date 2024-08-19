mulitple_g_cycle_overview = function(){

  g.cycle = seq(1, 8, by = 1)
  z.score = seq(100, 200, by = 14)
  f.differential = seq(1, 4, by = 0.4)
  n.individuals = c()
  m.differential = rep(0.7, 8)


  for(i in 1:8){

    n.individuals[i] = 10000*(0.6^i)

  }


  df.cycles = data.frame(g.cycle, z.score, n.individuals, f.differential, m.differential)


  plot.n = ggplot(df.cycles, aes(x=g.cycle, y=n.individuals))+
    geom_col(colour = "#ae017e",
             fill = "#fa9fb5",
             size = 1, width = 0.5)+
    ggtitle( paste0("1) Fewer female mosquitoes are alive in each subsequent gonotrophic cycle"))+
    theme_classic()+
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          title = element_text(size  = 30),
          legend.position = "none")


  plot.z = ggplot(df.cycles, aes(x=g.cycle, y=z.score))+
    geom_col(colour = "#016c59",
             fill = "#3690c0",
             size = 1, width = 0.5)+
    ggtitle(paste0("2) Mean resistance of surviving female mosquitoes\n increases in each subsequent gonotrophic cycle"))+
    theme_classic()+
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          title = element_text(size = 30),
          legend.position = "none")


  plot.s = ggplot(df.cycles, aes(x=g.cycle, y=f.differential))+
    geom_col(colour = "#810f7c",
             fill = "#8c96c6",
             size = 1, width = 0.5)+
    geom_col(aes(x=g.cycle, y=m.differential),
             width = 0.25,
             colour = "#800026",
             fill= "#fc4e2a",
             size = 1)+
    ggtitle(paste0("3) Female selection differential increases in each gonotrophic cycle\n & male selection differential remains constant"))+
    theme_classic()+
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          title = element_text(size = 30),
          legend.position = "none")



  plot.r = ggplot(df.cycles, aes(x=g.cycle, y=0.3*((f.differential+1)/2)))+
    geom_col(colour = "#00441b",
             fill = "#74c476",
             size = 1, width = 0.5)+
    xlab("Gonotrophic Cycle")+
    scale_x_continuous(breaks = seq(1, 8, 1))+
    ggtitle(paste0("4) Response increases in each gonotrophic cycle"))+
    theme_classic()+
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 25),
          axis.text.x = element_text(size = 25, colour = "black"),
          title = element_text(size = 30),
          legend.position = "none")


 p.layout = paste0("A\nB\nC\nD")


  final.plot = plot.n +
    plot.z +
    plot.s +
    plot.r +
    plot_layout(design = p.layout)

  return(final.plot)
}

mulitple_g_cycle_overview()


ggsave(
  filename = "chapter3_figure6.jpeg",
  plot = last_plot(),
  scale = 5,
  width = 700,
  height = 400,
  units = "px",
  dpi = 200)


