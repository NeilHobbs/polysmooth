

plot_simulation_advanced = function(sim.dataframe,
                                    return.threshold,
                                    withdrawal.threshold,
                                    sites,
                                    irm.deployment.strategy){
  pals = c( "#e41a1c",
            "#377eb8",
            "#4daf4a",
            "#984ea3",
            "#ff7f00",
            "#ffff33",
            "#a65628",
            "#f781bf",
            "#999999")

  if(irm.deployment.strategy == "singles"){

  if(sites == "both"){

  the.plot  = ggplot(sim.dataframe, aes(x=time.in.generations,
                                        y=100*bioassay.survival,
                                        colour = insecticide.tracked))+
    geom_hline(yintercept = withdrawal.threshold*100,
               colour = "black",
               size = 3,
               alpha = 0.7)+
    geom_hline(yintercept = return.threshold*100,
               colour = "grey",
               size = 3,
               alpha = 0.7)+
    geom_line(size = 2)+
    scale_colour_manual(values = pals)+
    xlab("Time in Mosquito Generations")+
    ylab("WHO Cylinder Bioassay Survival (%)")+
    geom_line(aes(x=time.in.generations,
                  y=-1), colour = "black",
              size = 8)+
    geom_point(aes(x=time.in.generations,
                   y=-1,
                   colour = insecticide.deployed),
               size = 4,
               shape = 15)+
    theme_light()+
    theme(legend.position = "bottom")+
    guides(colour=guide_legend(title="Insecticide"))+
    facet_wrap(~site)
  }

  if(sites == "intervention"){

    sim.dataframe = sim.dataframe%>%
      dplyr::filter(site == "intervention")

    the.plot  = ggplot(sim.dataframe, aes(x=time.in.generations,
                                          y=100*bioassay.survival,
                                          colour = insecticide.tracked))+
      geom_hline(yintercept = withdrawal.threshold*100,
                 colour = "black",
                 size = 3,
                 alpha = 0.7)+
      geom_hline(yintercept = return.threshold*100,
                 colour = "grey",
                 size = 3,
                 alpha = 0.7)+
      geom_line(size = 2)+
      scale_colour_manual(values = pals)+
      xlab("Time in Mosquito Generations")+
      ylab("WHO Cylinder Bioassay Survival (%)")+
      geom_line(aes(x=time.in.generations,
                    y=-1), colour = "black",
                size = 8)+
      geom_point(aes(x=time.in.generations,
                     y=-1,
                     colour = insecticide.deployed),
                 size = 4,
                 shape = 15)+
      theme_light()+
      theme(legend.position = "bottom")+
      guides(colour=guide_legend(title="Insecticide"))
  }

  if(sites == "refugia"){

    sim.dataframe = sim.dataframe%>%
      dplyr::filter(site == "refugia")

    the.plot  = ggplot(sim.dataframe, aes(x=time.in.generations,
                                          y=100*bioassay.survival,
                                          colour = insecticide.tracked))+
      geom_hline(yintercept = withdrawal.threshold*100,
                 colour = "black",
                 size = 3,
                 alpha = 0.7)+
      geom_hline(yintercept = return.threshold*100,
                 colour = "grey",
                 size = 3,
                 alpha = 0.7)+
      geom_line(size = 2)+
      scale_colour_manual(values = pals)+
      xlab("Time in Mosquito Generations")+
      ylab("WHO Cylinder Bioassay Survival (%)")+
      geom_line(aes(x=time.in.generations,
                    y=-1), colour = "black",
                size = 8)+
      geom_point(aes(x=time.in.generations,
                     y=-1,
                     colour = insecticide.deployed),
                 size = 4,
                 shape = 15)+
      theme_light()+
      theme(legend.position = "bottom")+
      guides(colour=guide_legend(title="Insecticide"))
  }
}

  if(irm.deployment.strategy == "mixtures"){

    if(sites == "both"){

      the.plot  = ggplot(sim.dataframe, aes(x=time.in.generations,
                                            y=100*bioassay.survival,
                                            colour = insecticide.tracked))+
        geom_hline(yintercept = withdrawal.threshold*100,
                   colour = "black",
                   size = 3,
                   alpha = 0.7)+
        geom_hline(yintercept = return.threshold*100,
                   colour = "grey",
                   size = 3,
                   alpha = 0.7)+
        geom_line(size = 2)+
        scale_colour_manual(values = pals)+
        xlab("Time in Mosquito Generations")+
        ylab("WHO Cylinder Bioassay Survival (%)")+
        geom_line(aes(x=time.in.generations,
                      y=-1), colour = "black",
                  size = 8)+
        geom_point(aes(x=time.in.generations,
                       y=-1,
                       colour = deployed.mixture.part.1),
                   size = 4,
                   shape = 15)+
        geom_point(aes(x=time.in.generations,
                       y=-1,
                       colour = deployed.mixture.part.2),
                   size = 4,
                   shape = 15)+
        theme_light()+
        theme(legend.position = "bottom")+
        guides(colour=guide_legend(title="Insecticide"))+
        facet_wrap(~site)
    }

    if(sites == "intervention"){

      sim.dataframe = sim.dataframe%>%
        dplyr::filter(site == "intervention")

      the.plot  = ggplot(sim.dataframe, aes(x=time.in.generations,
                                            y=100*bioassay.survival,
                                            colour = insecticide.tracked))+
        geom_hline(yintercept = withdrawal.threshold*100,
                   colour = "black",
                   size = 3,
                   alpha = 0.7)+
        geom_hline(yintercept = return.threshold*100,
                   colour = "grey",
                   size = 3,
                   alpha = 0.7)+
        geom_line(size = 2)+
        scale_colour_manual(values = pals)+
        xlab("Time in Mosquito Generations")+
        ylab("WHO Cylinder Bioassay Survival (%)")+
        geom_line(aes(x=time.in.generations,
                      y=-0.5), colour = "black",
                  size = 6)+
        geom_line(aes(x=time.in.generations,
                      y=-1), colour = "black",
                  size = 6)+
        geom_point(aes(x=time.in.generations,
                       y=-0.5,
                       colour = deployed.mixture.part.1),
                   size = 2,
                   shape = 15)+
        geom_point(aes(x=time.in.generations,
                       y=-1,
                       colour = deployed.mixture.part.2),
                       size = 2,
                       shape = 15)+
        theme_light()+
        theme(legend.position = "bottom")+
        guides(colour=guide_legend(title="Insecticide"))
    }

    if(sites == "refugia"){

      sim.dataframe = sim.dataframe%>%
        dplyr::filter(site == "refugia")

      the.plot  = ggplot(sim.dataframe, aes(x=time.in.generations,
                                            y=100*bioassay.survival,
                                            colour = insecticide.tracked))+
        geom_hline(yintercept = withdrawal.threshold*100,
                   colour = "black",
                   size = 3,
                   alpha = 0.7)+
        geom_hline(yintercept = return.threshold*100,
                   colour = "grey",
                   size = 3,
                   alpha = 0.7)+
        geom_line(size = 2)+
        scale_colour_manual(values = pals)+
        xlab("Time in Mosquito Generations")+
        ylab("WHO Cylinder Bioassay Survival (%)")+
        geom_line(aes(x=time.in.generations,
                      y=-1), colour = "black",
                  size = 8)+
        geom_point(aes(x=time.in.generations,
                       y=-1,
                       colour = deployed.mixture.part.1),
                   size = 4,
                   shape = 15)+
        geom_point(aes(x=time.in.generations,
                       y=-2,
                       colour = deployed.mixture.part.2,
                       size = 4,
                       shape = 15))+
        theme_light()+
        theme(legend.position = "bottom")+
        guides(colour=guide_legend(title="Insecticide"))
    }

  }

  return(the.plot)

}
