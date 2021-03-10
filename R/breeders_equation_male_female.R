#'@title Perform the Breeder's Equation with separate selection differentials for males and females.
#'
#'@param heritability = The heritability of a polygenic trait.
#'@param female.selection.differential = The selection differential for females
#'@param male.selection.differential = The selection differential for males


breeders_equation_male_female = function(heritability,
                                         female.selection.differential,
                                         male.selection.differential){

  response = ((heritability/2) * female.selection.differential) + ((heritability/2) * male.selection.differential)

  return(response)
}
