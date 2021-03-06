% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/et_functions.R
\name{calc_reference_et}
\alias{calc_reference_et}
\title{Calculate Reference ET}
\usage{
calc_reference_et(
  lat,
  elevation,
  screen_height,
  crop_height = 0.12,
  canopy_resistance = 70,
  doy,
  tmax,
  tmin,
  rs,
  rhmax,
  rhmin,
  uz
)
}
\arguments{
\item{lat}{latitude (degrees)}

\item{elevation}{elevetation (m)}

\item{screen_height}{screening height (m)}

\item{crop_height}{crop height (m)}

\item{canopy_resistance}{canopy resistance (m/s^2)}

\item{doy}{day of year}

\item{tmax}{maximum daily temperature (C)}

\item{tmin}{minimum daily temperature (C)}

\item{rs}{solar radiation}

\item{rhmax}{maximum daily relative humidity}

\item{rhmin}{minimum daily relative humidity}

\item{uz}{wind speed at}
}
\value{
reference evapotranspiration (mm)
}
\description{
Calculate Reference ET
}
\examples{
cropsyst:::calc_reference_et(45, 1000, 2, 2, 0.05, 30, 30, 20, 600, 100, 50, 10)
}
