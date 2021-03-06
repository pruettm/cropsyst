% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soil.R
\name{calc_saturation_water_content}
\alias{calc_saturation_water_content}
\title{Calculate saturation water content}
\usage{
calc_saturation_water_content(sand_pct, clay_pct)
}
\arguments{
\item{sand_pct}{Percentage of sand in soil layer}

\item{clay_pct}{Percentage of clay in soil layer}
}
\value{
Saturation water content (m3/m3)
}
\description{
Calculate saturation water content
}
\examples{
cropsyst:::calc_saturation_water_content(40, 30)
}
