% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{cs_add_canopy_cover}
\alias{cs_add_canopy_cover}
\title{Add canopy cover data to cropsyst object}
\usage{
cs_add_canopy_cover(cs, canopy_cover)
}
\arguments{
\item{cs}{cropsyst object}

\item{canopy_cover}{canopy cover data frame}
}
\value{
cropsyst object with canopy cover data
}
\description{
Add canopy cover data to cropsyst object
}
\examples{
cs_add_canopy_cover(cs_setup(), cs_canopy_cover)
}
