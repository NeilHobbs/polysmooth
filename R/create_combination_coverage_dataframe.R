create_combination_coverage_dataframe = function(total.rows){


  ci = c()
  cj = c()
  cij = c()

  for(i in 1:total.rows){

    x = runif(3, 0, 1)
    y = x / sum(x)

    ci[i] = y[1]
    cj[i] = y[2]
    cij[i] = y[3]
  }

  coverage.df = data.frame(ci, cj, cij)

  return(coverage.df)
}
#
# coverage.df = coverage_dataframe(1000)
#
#
# hist(coverage.df$ci)
# hist(coverage.df$ci)
# hist(coverage.df$ci)




