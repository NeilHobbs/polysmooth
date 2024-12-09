% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_density_after_selection.R
\name{calculate_density_after_selection}
\alias{calculate_density_after_selection}
\title{Calculate the relative population contribution to the population of mosquitoes with a Polygenic Resistance Score after the insecticide encounter in the intervention site.}
\usage{
calculate_density_after_selection(
  insecticide.exposure,
  vector.length,
  trait.mean,
  standard.deviation,
  maximum.bioassay.survival.proportion = 1,
  michaelis.menten.slope = 1,
  half.population.bioassay.survival.resistance = 900,
  regression.coefficient = 0.48,
  regression.intercept = 0.15,
  current.insecticide.efficacy
)
}
\arguments{
\item{insecticide.exposure}{= The proportion of the mosquito population in the intervention site being exposed to an insecticide.}

\item{vector.length}{= The length of the vector to be returned. A minimum value of 100,000 is recommmended.}

\item{trait.mean}{= The mean value of a polygenic trait in a population.}

\item{standard.deviation}{= The standard deviation of the trait mean in the population.}

\item{maximum.bioassay.survival.proportion}{= The maximum proportion of mosquitoes that can survive in the bioassay.}

\item{michaelis.menten.slope}{= The slope in the Michaelis-Menten equation.}

\item{half.population.bioassay.survival.resistance}{= The Polygenic Resistance Score which gives a 50\% survival probability in a WHO cylinder bioassay.}

\item{regression.coefficient}{= A linear model coefficient obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.}

\item{regression.intercept}{= The linear model intercept obtained from performing a linear model on paired experimental hut trials and WHO cylinder bioassays.}

\item{current.insecticide.efficacy}{= The current efficacy of the insecticide, defined as proportion of fully susceptible mosquitoes surviving contact with the insecticide-treated surface.}
}
\description{
Calculate the relative population contribution to the population of mosquitoes with a Polygenic Resistance Score after the insecticide encounter in the intervention site.
}
