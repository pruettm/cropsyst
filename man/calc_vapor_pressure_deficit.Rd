% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/et_functions.R
\name{calc_vapor_pressure_deficit}
\alias{calc_vapor_pressure_deficit}
\title{Calculate vapor pressure deficit}
\usage{
calc_vapor_pressure_deficit(es_tmax, es_tmin, ea)
}
\arguments{
\item{es_tmax}{Saturation Vapor Pressure at Maximum Temperature (°C)}

\item{es_tmin}{Saturation Vapor Pressure at Minimum Temperature (°C)}

\item{ea}{Vapor Pressure at Air Temperature (°C)}
}
\value{
Vapor Pressure Deficit (kPa)
}
\description{
Calculate vapor pressure deficit
}
\examples{
cropsyst:::calc_vapor_pressure_deficit(5, 3, 4)
}
