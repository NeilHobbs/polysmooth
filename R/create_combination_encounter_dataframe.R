create_combination_encounter_dataframe = function(total.rows){


  encounter.i = c()
  encounter.j = c()
  encounter.ij = c()

  for(i in 1:total.rows){

    x = runif(3, 0, 1)
    y = x / sum(x)

    encounter.i[i] = y[1]
    encounter.j[i] = y[2]
    encounter.ij[i] = y[3]
  }

  encounter.df = data.frame(encounter.i, encounter.j, encounter.ij)

  return(encounter.df)
}
