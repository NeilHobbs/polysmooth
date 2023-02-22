#'@title Calculate the relative population contribution to the population of male mosquitoes with a Polygenic Resistance Score after the insecticide encounter in the intervention site.
#'
#'@param female.insecticide.exposure = Proportion of female mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide.
#'@param male.insecticide.exposure = Proportion of male mosquitoes in the intervention site that encounter and are exposed to the deployed insecticide as a proportion of the exposure of female mosquitoes
#'@param vector.length = The length of the vector to be returned. A minimum value of 100,000 is recommmended.
#'@param trait.mean = The mean value of a polygenic trait in a population.
#'@param standard.deviation = The standard deviation of the trait mean in the population.
#'@param maximum.bioassay.survival.proportion = The maximum proportion of mosquitoes that can survive in the bioassay.
#'@param michaelis.menten.slope = The slope in the Michaelis-Menten equation.
#'@param half.population.bioassay.survival.resistance = The Polygenic Resistance Score which gives a 50% survival probability in a WHO cylinder bioassay.
#'@param regression.coefficient = A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param regression.intercept = The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.
#'@param current.insecticide.efficacy = The current efficacy of the insecticide, defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.

calculate_male_density_after_selection = function(female.insecticide.exposure,
                                                  male.insecticide.exposure,
                                                    vector.length,
                                                    trait.mean,
                                                    standard.deviation,
                                                    maximum.bioassay.survival.proportion = 1,
                                                    michaelis.menten.slope = 1,
                                                    half.population.bioassay.survival.resistance = 900,
                                                    regression.coefficient = 0.48,
                                                    regression.intercept = 0.15,
                                                    current.insecticide.efficacy){

#if(female.insecticide.exposure > 1 | female.insecticide.exposure < 0){stop("female.insecticide.exposure must be between 0 and 1")}
#if(male.insecticide.exposure > 1 | male.insecticide.exposure < 0){stop("male.insecticide.exposure must be between 0 and 1")}


  #step 1:create the Normal Distribution of Polygenic Resistance Values
  normal.distribution = create_normal_distribution(vector.length = vector.length,
                                                   trait.mean = trait.mean,
                                                   standard.deviation = standard.deviation)

  #step 2:get the probability densities
  relative.male.contribution.before.selection =  calculate_density_of_trait_values(vector.length = vector.length,
                                                                                     trait.mean = trait.mean,
                                                                                     standard.deviation = standard.deviation)/2

  #step 3: convert the polygenic scores to bioassay survival:
  bioassay.survivals = convert_resistance_score_to_bioassay_survival(maximum.bioassay.survival.proportion = maximum.bioassay.survival.proportion,
                                                                     trait.mean = normal.distribution,
                                                                     michaelis.menten.slope = michaelis.menten.slope,
                                                                     half.population.bioassay.survival.resistance = half.population.bioassay.survival.resistance)

  #step 4: convert bioassay survival to field survival:
  field.survivals = convert_bioassay_survival_to_field_survival(bioassay.survival = bioassay.survivals,
                                                                regression.coefficient = regression.coefficient,
                                                                regression.intercept = regression.intercept,
                                                                current.insecticide.efficacy = current.insecticide.efficacy)


  #step 5:calculate the change in probability densities
  relative.male.contribution.after.selection = relative.male.contribution.before.selection * field.survivals * female.insecticide.exposure * male.insecticide.exposure

  return(relative.male.contribution.after.selection)
}
