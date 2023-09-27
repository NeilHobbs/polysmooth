
coverages.llin = rep(c(0, 0, 0.25, 0.25,
                   0, 0.5, 0.5, 0.25,
                   0.75, 0.75), 10)

coverages.irs = rep(c(0, 0.25, 0, 0.25,
                  0.5, 0, 0.25, 0.5,
                  0.25, 0), 10)

coverages.both = rep(c(1, 0.75, 0.75, 0.5,
                   0.5, 0.5, 0.25, 0.25,
                   0, 0.25), 10)



encounter.llin = rep(c(0, 0, 0.25, 0.25,
                   0, 0.5, 0, 0.75,
                   0.25, 0.5), each = 10)


encounter.irs = rep(c(0, 0.25, 0, 0.25,
                  0.5, 0, 0.75, 0,
                  0.5, 0.25), each = 10)


encounter.both = rep(c(1, 0.75, 0.75, 0.5,
                   0.5, 0.5, 0.25, 0.25,
                   0.25, 0.25), each = 10)





llin.only = coverages.llin + (coverages.both * encounter.llin)
irs.only = coverages.irs + (coverages.both * encounter.irs)
both = coverages.both * encounter.both

end.contact = paste0("LLIN:", llin.only, "IRS:", irs.only, "both:", both)

df = data.frame(coverages.llin, coverages.irs, coverages.both,
                encounter.llin, encounter.irs, encounter.both,
                end.contact)

df$is.duplicate = duplicated(df$end.contact)

df.1 = subset(df, is.duplicate == FALSE)


coverage.encounter.df = df.1|>
  dplyr::select(-"end.contact", -"is.duplicate")





A = data.frame(do.call("rbind", replicate(500, coverage.encounter.df,
                                          simplify = FALSE)))


df = data.frame(lhs::randomLHS(500, 5))

parameter.space = df|>
  dplyr::rename(Heritability = X1)|>
  dplyr::rename(`Male Insecticide Exposure` = X2)|>
  dplyr::rename(`Female Insecticide Exposure` = X3)|>
  dplyr::rename(`Intervention Coverage` = X4)|>
  dplyr::rename(Dispersal = X5)|>
  dplyr::mutate(Heritability = qunif(Heritability, 0.05, 0.3))|>
  dplyr::mutate(`Male Insecticide Exposure` = qunif(`Male Insecticide Exposure`, 0, 1))|> #
  dplyr::mutate(`Female Insecticide Exposure` = qunif(`Female Insecticide Exposure`, 0.4, 0.9))|> #Defaults from Ian
  dplyr::mutate(`Intervention Coverage` = qunif(`Intervention Coverage`, 0.5, 0.9))|>
  dplyr::mutate(Dispersal = qunif(Dispersal, 0.1, 0.9))

#From mefa R package; was not loading from that package?
rep.data.frame =function(x, ...){
    as.data.frame(lapply(x, rep, ...))}


B = rep.data.frame(parameter.space, each= 60)


parameter.space.df = cbind(A, B)

write.csv(parameter.space.df, "parameter.space.combinations.multi.irs.csv")

