% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/return_of_insecticides_to_arsenal.R
\name{return_of_insecticides_to_arsenal}
\alias{return_of_insecticides_to_arsenal}
\title{Returns the insecticides that have been withdrawn back to being available for deployment if they meet the return.threshold criteria}
\usage{
return_of_insecticides_to_arsenal(
  number.of.insecticides,
  current.generation,
  return.threshold,
  simulation.array,
  available.vector,
  withdrawn.vector
)
}
\arguments{
\item{number.of.insecticides}{= the number of insecticides in the simulation}

\item{current.generation}{= the generation in the simulation}

\item{return.threshold}{= the resistance intensity for return to the arsenal (this is the intensity score, not the proportion surviving)}

\item{simulation.array}{= the array that holds the simulation data}

\item{available.vector}{= a vector containing the insecticides available for deployment}

\item{withdraw.vector}{= a vector containing the insecticdes withdrawn from deployment}
}
\description{
This function returns all the insecticides that meet the return criteria back to being available to deploy. Returning an updated
list of two vectors: available.vector[\link{1}] which can be used for deployment. And withdrawn.vector[\link{2}] which cannot be used for deployment.
It is important to not that this function does not withdrawn the insecticides. ]
}
