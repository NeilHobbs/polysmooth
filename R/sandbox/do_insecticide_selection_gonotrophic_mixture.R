do_insecticide_selection_gonotrophic_mixture = function(intervention.staying.a.i,
                                                        survival.probability.a.i,
                                                        refugia.leaving.b.i,
                                                        survival.probability.b.i,
                                                        intervention.leaving.a.i,
                                                        refugia.staying.b.i,
                                                        intervention.staying.a.j,
                                                        survival.probability.a.j,
                                                        refugia.leaving.b.j,
                                                        survival.probability.b.j,
                                                        intervention.leaving.a.j,
                                                        refugia.staying.b.j,
                                                        female.exposure,
                                                        gonotrophic,
                                                        mean.survival.intervention.j,
                                                        mean.survival.refugia.j,
                                                        mean.survival.intervention.i,
                                                        mean.survival.refugia.i){

  intervention.selection.a.i = (intervention.staying.a.i[[gonotrophic-1]] * female.exposure * survival.probability.a.i*mean.survival.intervention.j) + (intervention.staying.a.i[[gonotrophic-1]] * (1-female.exposure))
  intervention.selection.b.i = (refugia.leaving.b.i[[gonotrophic - 1]] * female.exposure * survival.probability.b.i*mean.survival.refugia.j) + (refugia.leaving.b.i[[gonotrophic - 1]] * (1-female.exposure))
  refugia.selection.a.i = intervention.leaving.a.i[[gonotrophic - 1]]
  refugia.selection.b.i = refugia.staying.b.i[[gonotrophic - 1]]

  intervention.selection.a.j = (intervention.staying.a.j[[gonotrophic-1]] * female.exposure * survival.probability.a.j*mean.survival.intervention.i) + (intervention.staying.a.j[[gonotrophic-1]] * (1-female.exposure))
  intervention.selection.b.j = (refugia.leaving.b.j[[gonotrophic - 1]] * female.exposure * survival.probability.b.j*mean.survival.refugia.i) + (refugia.leaving.b.j[[gonotrophic - 1]] * (1-female.exposure))
  refugia.selection.a.j = intervention.leaving.a.j[[gonotrophic - 1]]
  refugia.selection.b.j = refugia.staying.b.j[[gonotrophic - 1]]

  return(list(intervention.selection.a.i,
         intervention.selection.b.i,
         refugia.selection.a.i,
         refugia.selection.b.i,
         intervention.selection.a.j,
         intervention.selection.b.j,
         refugia.selection.a.j,
         refugia.selection.b.j))
}
