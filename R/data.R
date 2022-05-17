#' Sample daily weather data
#'
#' A dataset containing daily weather data.
#'
#' @format A data frame with 365 rows and 10 variables:
#' \describe{
#'   \item{date}{date}
#'   \item{precip}{daily precipitation, in mm}
#'   \item{tmax}{maximum daily temperature, in Celcius}
#'   \item{tmin}{minimum daily temperature, in Celcius}
#'   \item{s_rad}{total daily solar radiation, in MJ}
#'   \item{rh_max}{maximum daily relative humidity}
#'   \item{rh_min}{minimum daily relative humidity}
#'   \item{wind_speed}{wind speed, in m/s}
#' }
"cs_weather"

#' Sample soil data
#'
#' A dataset containing the horizon thickness and composition of a soil cross-section
#'
#' @format A data frame with 13 rows and 4 variables:
#' \describe{
#'   \item{horizon_thickness}{horizon thickness, in m}
#'   \item{sand_pct}{percentage of sand}
#'   \item{clay_pct}{percentage of clay}
#'   \item{silt_pct}{percentage of silt}
#'   \item{initial_wc}{initial water content, in m3/m3}
#' }
"cs_soil"

#' Canopy Cover
#'
#' @format A data frame with 151 rows and 2 variables:
#' \describe{
#'   \item{dap}{days after planting}
#'   \item{gcc}{Green Canopy Cover}
#'   \item{tcc}{Total Canopy Cover}
#' }
"cs_canopy_cover"

#' Time series of irrigation application rates
#'
#' @format A data frame with 21 rows and 2 variables:
#' \describe{
#'   \item{dap}{day after planting}
#'   \item{irrigation}{irrigation (mm)}
#' }
"cs_irrigation"

#' Crop input paramters
#'
#' @format A data frame with 21 rows and 2 variables:
#' \describe{
#'   \item{crop}{Crop name}
#'   \item{midseason_et_crop_coef}{Midseason ET crop Coefficient}
#'   \item{max_crop_height}{Max crop height (m)}
#'   \item{max_root_depth}{Max root depth (m)}
#'   \item{max_allowable_depletion}{Soil water depletion for no stress}
#'   \item{transpiration_use_eff}{Transpiration-use efficiency (g/kg)}
#' }
"cs_crop"
