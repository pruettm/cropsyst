% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soil.R
\name{calc_b_value}
\alias{calc_b_value}
\title{Calculate Campbell "B" value}
\usage{
calc_b_value(sand_pct, clay_pct)
}
\arguments{
\item{sand_pct}{Percentage of sand in soil layer}

\item{clay_pct}{Percentage of clay in soil layer}
}
\value{
Campbell "B" value
}
\description{
Calculate Campbell "B" value
}
\examples{
cropsyst:::calc_b_value(40, 30)
}
