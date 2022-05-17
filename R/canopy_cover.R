#' Calculate Canopy Cover
#'
#' @param ndvi a time series of ndvi values from planting to harvest date
#'
#' @return canopy cover
#' @export
#'
#' @examples calc_canopy_cover(runif(100))
calc_canopy_cover <- function(ndvi){

  dae <- 1:length(ndvi)
  peak_value <- max(ndvi)
  initial_value <- ndvi[1]
  end_season_value <- ndvi[length(ndvi)]

  # hard coded parameters. do not expose
  Shape_Coef_Before_Peak <- 9
  Shape_Coef_During_Decline <- 9
  Time_Fraction_At_Half_peak_value <- 0.5
  Time_Fraction_At_Half_Decline <- 0.5

  #Derived parameters for the standard green canopy curve
  B1 = 1 / exp(-Shape_Coef_Before_Peak * Time_Fraction_At_Half_peak_value)
  B2 = 1 / exp(-Shape_Coef_During_Decline * Time_Fraction_At_Half_Decline)
  Asympthotic_Value_max = (peak_value - initial_value) * (1 + B1 * exp(-Shape_Coef_Before_Peak * 1)) + initial_value
  Actual_Value_max1 <- initial_value + (Asympthotic_Value_max - initial_value) / (1 + B1 * exp(-Shape_Coef_Before_Peak))
  Actual_Value_max2 <- dplyr::if_else(dae > dae[ndvi == max(ndvi)],
                               (Actual_Value_max1 * (1 + B2) - end_season_value) / B2,
                               (max(ndvi) * (1 + B2) - end_season_value) / B2)

  Asymthotic_Value_Decline <- Actual_Value_max2 + (end_season_value - Actual_Value_max2) * (1 + B2 * exp(-Shape_Coef_During_Decline))

  Relative_TT <-
    dplyr::if_else(dae > dae[ndvi == max(ndvi)],
                   dae/max(dae),
                   (dae - dae[ndvi == max(ndvi)])/(max(dae) - dae[ndvi == max(ndvi)]))

  cc <- dplyr::if_else(dae > dae[ndvi == max(ndvi)],
                       initial_value + (max(ndvi) - ndvi[1])/(1 + B1*exp(-Shape_Coef_Before_Peak*Relative_TT)),
                       Actual_Value_max2 - (Actual_Value_max2-Asymthotic_Value_Decline)/(1 + B2*exp(Shape_Coef_During_Decline*Relative_TT)))
  cc <- pmax(cc, end_season_value)
  cc <- pmin(cc, Actual_Value_max2)

  return(cc)
}
