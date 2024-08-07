% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/withdrawal_of_insecticides_from_arsenal.R
\name{withdrawal_of_insecticides_from_arsenal}
\alias{withdrawal_of_insecticides_from_arsenal}
\title{Withdraws all the required insecticides from the arsenal}
\usage{
withdrawal_of_insecticides_from_arsenal(
  number.of.insecticides,
  current.generation,
  withdrawal.threshold,
  simulation.array,
  available.vector,
  withdrawn.vector
)
}
\arguments{
\item{number.of.insecticides}{= the number of insecticides in the simulation}

\item{current.generation}{= the generation in the simulation}

\item{withdrawal.threshold}{= the resistance intensity for withdrawal}

\item{simulation.array}{= the array that holds the simulation data}

\item{available.vector}{= a vector containing the insecticides available for deployment}

\item{withdraw.vector}{= a vector containing the insecticdes withdrawn from deployment}
}
\description{
Withdraws all the required insecticides from the arsenal
}
