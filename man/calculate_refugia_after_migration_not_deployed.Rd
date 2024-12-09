% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/calculate_refugia_after_migration_not_deployed.R
\name{calculate_refugia_after_migration_not_deployed}
\alias{calculate_refugia_after_migration_not_deployed}
\title{Calculate the mean Polygenic Resistance Score of the mosquito population in the refugia site after mosquito migration.}
\usage{
calculate_refugia_after_migration_not_deployed(
  intervention.after.selection,
  refugia.after.selection,
  migration.intervention.to.refugia,
  migration.refugia.to.intervention,
  insecticide.population.suppression
)
}
\arguments{
\item{intervention.after.selection}{= The mean Polygenic Resistance Score of the mosquito population in the intervention site after insecticide and fitness cost based selection.}

\item{refugia.after.selection}{= The mean Polygenic Resistance Score of the mosquito population in the refugia site after  fitness cost based selection.}

\item{migration.intervention.to.refugia}{= The relative number of mosquitoes dispersing from the intervention site to the refugia.}

\item{migration.refugia.to.intervention}{= The relative number of mosquitoes dispersing from the refugia to the intervention site.}

\item{insecticide.population.suppression}{= The impact of insecticides on the relative population size of female mosquitoes the intervention site, who are now ready to lay eggs.}
}
\description{
Calculate the mean Polygenic Resistance Score of the mosquito population in the refugia site after mosquito migration.
}
