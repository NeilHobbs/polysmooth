#'@title
#'
#'@param heritability = The heritability of a polygenic trait.


wrapper_breeders_equation_male_female_insecticide_fitness = function(trait.mean,
                                                                     female.fitness.cost,
                                                                     male.fitness.cost,
                                                                     female.insecticide.exposure,
                                                                     male.insecticide.exposure,
                                                                     standard.deviation,
                                                                     vector.length,
                                                                     maximum.bioassay.survival.proportion,
                                                                     michaelis.menten.slope,
                                                                     half.population.bioassay.survival.resistance,
                                                                     regression.coefficient,
                                                                     regression.intercept,
                                                                     current.insecticide.efficacy,
                                                                     exposure.scaling.factor,
                                                                     heritability){

 #Female selection differential:
    female.insecticide.fitness.selection.differential = wrapper_calculate_female_insecticide_fitness_selection_differential(female.trait.mean = trait.mean,
                                                                                                                          female.insecticide.exposure = female.insecticide.exposure,
                                                                                                                          standard.deviation= standard.deviation,
                                                                                                                          vector.length = vector.length,
                                                                                                                          maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                          michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                          half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                          regression.coefficient = regression.coefficient,
                                                                                                                          regression.intercept = regression.intercept,
                                                                                                                          current.insecticide.efficacy = current.insecticide.efficacy,
                                                                                                                          exposure.scaling.factor = exposure.scaling.factor,
                                                                                                                          female.fitness.cost = female.fitness.cost)
  #Male selection differential:

    male.insecticide.fitness.selection.differential =  wrapper_calculate_male_insecticide_fitness_selection_differential(male.trait.mean = trait.mean,
                                                                                                                         female.insecticide.exposure = female.insecticide.exposure,
                                                                                                                         male.insecticide.exposure = male.insecticide.exposure,
                                                                                                                         male.fitness.cost = male.fitness.cost,
                                                                                                                         standard.deviation = standard.deviation,
                                                                                                                         vector.length = vector.length,
                                                                                                                         maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                                                                         michaelis.menten.slope = michaelis.menten.slope,
                                                                                                                         half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance,
                                                                                                                         regression.coefficient = regression.coefficient,
                                                                                                                         regression.intercept = regression.intercept,
                                                                                                                         current.insecticide.efficacy = current.insecticide.efficacy,
                                                                                                                         exposure.scaling.factor = exposure.scaling.factor)

    #Incoporate into the Breeder's Equation to get the response

    response.insecticide.fitness = breeders_equation_male_female_insecticide_fitness(heritability = heritability,
                                                                                     male.insecticide.fitness.selection.differential = male.insecticide.fitness.selection.differential,
                                                                                     female.insecticide.fitness.selection.differential = female.insecticide.fitness.selection.differential)

  return(response.insecticide.fitness)
}
