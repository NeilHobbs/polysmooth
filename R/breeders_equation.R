#'@title Perform the basic Breeder's Equation
#'
#'@param selection.differential = The change in the value of a polygenic trait within a generation.
#'@param heritability = The heritability of a polygenic trait.

breeders_equation = function(selection.differential,
                             heritability){

  response = selection.differential * heritability

  return(response)
}
