

calculate_new_means_after_selection_gonotrophic_mixtures = function(intervention.selection.a.i,
                                                                    intervention.normal.distribution.i,
                                                                    intervention.selection.b.i,
                                                                    refugia.normal.distribution.i,
                                                                    refugia.selection.a.i,
                                                                    refugia.selection.b.i,
                                                                    intervention.selection.a.j,
                                                                    intervention.normal.distribution.j,
                                                                    intervention.selection.b.j,
                                                                    refugia.normal.distribution.j,
                                                                    refugia.selection.a.j,
                                                                    refugia.selection.b.j
                                                                    ){

#calculate the new means after selection:::
#intervention site::
new.intervention.mean.a.i = sum(intervention.selection.a.i * intervention.normal.distribution.i)/(sum(intervention.selection.a.i))
new.intervention.mean.b.i = sum(intervention.selection.b.i * refugia.normal.distribution.i)/(sum(intervention.selection.b.i))
#refugia::
new.refugia.mean.a.i = sum(refugia.selection.a.i * intervention.normal.distribution.i)/(sum(refugia.selection.a.i))
new.refugia.mean.b.i = sum(refugia.selection.b.i * refugia.normal.distribution.i)/(sum(refugia.selection.b.i))

#intervention site::
new.intervention.mean.a.j = sum(intervention.selection.a.j * intervention.normal.distribution.j)/(sum(intervention.selection.a.j))
new.intervention.mean.b.j = sum(intervention.selection.b.j * refugia.normal.distribution.i)/(sum(intervention.selection.b.j))
#refugia::
new.refugia.mean.a.j = sum(refugia.selection.a.j * intervention.normal.distribution.j)/(sum(refugia.selection.a.j))
new.refugia.mean.b.j = sum(refugia.selection.b.j * refugia.normal.distribution.j)/(sum(refugia.selection.b.j))


return(list(new.intervention.mean.a.i, new.intervention.mean.b.i,
            new.refugia.mean.a.i, new.refugia.mean.b.i,
            new.intervention.mean.a.j, new.intervention.mean.b.j,
            new.refugia.mean.a.j, new.refugia.mean.b.j))

}

