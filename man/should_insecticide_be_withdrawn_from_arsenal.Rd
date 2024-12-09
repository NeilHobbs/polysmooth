% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/should_insecticide_be_withdrawn_from_arsenal.R
\name{should_insecticide_be_withdrawn_from_arsenal}
\alias{should_insecticide_be_withdrawn_from_arsenal}
\title{Check if insecticides should be withdrawn from the arsenal based on the resistance intensity.}
\usage{
should_insecticide_be_withdrawn_from_arsenal(
  insecticide,
  current.generation,
  withdrawal.threshold,
  simulation.array
)
}
\arguments{
\item{insecticide}{= The insecticide to which the resistance intensity is to be checked against}

\item{current.generation}{= The generation where the simulation is up to.}

\item{withdrawal.threshold}{= The resistance intensity at which an insecticide should no longer be used and therefore withdrawn.}

\item{simulation.array}{= The array which holds the simulation results.}
}
\description{
Check if insecticides should be withdrawn from the arsenal based on the resistance intensity.
}
