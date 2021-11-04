#' Saturation Vapor Pressure
#'
#' @param temp Air temperature (°C)
#'
#' @return Saturation Vapor Pressure (kPa)
#' @export
#'
#' @examples sat_vapor_pressure(32)
sat_vapor_pressure <- function(temp) {
  return(0.6108 * exp(17.27 * temp / (temp + 237.3)))
}

#' Vapor Pressure
#'
#' @param es_tmax Saturation Vapor Pressure at Maximum Temperature (°C)
#' @param es_tmin Saturation Vapor Pressure at Minimum Temperature (°C)
#' @param rhmax Maximum relative humidity (%)
#' @param rhmin Minimum relative humidity (%)
#'
#' @return Vapor Pressure (kPA)
#' @export
#'
#' @examples vapor_pressure(5, 3, 100, 40)
vapor_pressure <- function(es_tmax, es_tmin, rhmax, rhmin) {
  return((es_tmin * rhmax / 100.0 + es_tmax * rhmin / 100.0) / 2.0)
}

#' Vapor Pressure Deficit
#'
#' @param es_tmax Saturation Vapor Pressure at Maximum Temperature (°C)
#' @param es_tmin Saturation Vapor Pressure at Minimum Temperature (°C)
#' @param ea Vapor Pressure at Air Temperature (°C)
#'
#' @return Vapor Pressure Deficit (kPa)
#' @export
#'
#' @examples vapor_pressure_deficit(5, 3, 4)
vapor_pressure_deficit <- function(es_tmax, es_tmin, ea){
  return((es_tmax + es_tmin) / 2.0 - ea)
}

#' Potential Radiation
#'
#' @param lat latitude (degrees)
#' @param doy day of year
#'
#' @return Potential Radiation (MJ/m2/day)
#' @export
#'
#' @examples potential_radiation(45, 233)
potential_radiation <- function(lat, doy){
  solar_constant <- 118.08
  lat_rad <- lat * pi / 180.0
  doy_sun_radians <- 2.0 * pi * doy / 365.0
  dr <- 1.0 + 0.033 * cos(doy_sun_radians)
  sol_dec <- 0.409 * sin(doy_sun_radians - 1.39)
  sunset_hour_angle <- acos(-tan(lat_rad) * tan(sol_dec))
  term <- sunset_hour_angle * sin(lat_rad) * sin(sol_dec) +
    cos(lat_rad) * cos(sol_dec) * sin(sunset_hour_angle)
  return(solar_constant * dr * term / pi)
}

#' Daily Net Radiation
#'
#' @param pot_rad Potential solar radiation (MJ/m2/day)
#' @param sw_rad Shortwave radiation (MJ/m2/day)
#' @param ea Daily average vapor pressure (kPa)
#' @param tmax Daily maximum temperature (C)
#' @param tmin Daily minimum temperature (C)
#' @param elevation elevation (m)
#'
#' @return Net Radiation (MJ/m2/day)
#' @export
#'
#' @examples net_radiation(900, 800, 1, 30, 10, 300)
net_radiation <- function(pot_rad, sw_rad, ea, tmax, tmin, elevation){
  albedo = 0.23
  Rns = (1.0 - albedo) * sw_rad
  Rso = pot_rad * (0.75 + elevation * 2.e-5)
  # Calculate cloud factor
  F_Cloud = 1.35 * (sw_rad / Rso) - 0.35
  # Calculate humidity factor
  F_Hum = (0.34 - 0.14 * sqrt(ea))
  # Calculate Isothermal LW net radiation
  LWR = 4.903e-9 * ((tmax + 273.0^4) + (tmin + 273.0)^4) / 2.0
  Rnl = LWR * F_Cloud * F_Hum
  return(Rns - Rnl)
}

#' Aerodynamic Resistance
#'
#' @param wind_speed Wind speed (m/s)
#' @param height Measured height of wind speed (m)
#' @param crop_height Crop height (m)
#'
#' @return Aerodynamic resistance (day/m)
#' @export
#'
#' @examples aero_resistance(2, 3, 1.4)
aero_resistance <- function(wind_speed, height, crop_height){
  wind_speed_2m <-
    ifelse(height == 2,
           wind_speed,
           wind_speed*4.87/log(67.75*height-5.42))

  # convert to m/s to m/day
  wind_speed_2m <- wind_speed_2m * 86400

  d <- crop_height*0.667
  zom <- 0.123*crop_height
  zoh <- 0.0123*crop_height

  zm <- 2
  zh <- 2

  von_karman <- 0.41

  term1 <- log((zm-d)/zom)
  term2 <- log((zh - d)/zoh)

  return(term1*term2/((von_karman^2)*wind_speed_2m))

}

# cs_reference_et <- function(lat,
#                          elevation,
#                          screen_height,
#                          crop_height,
#                          canopy_resistance,
#                          doy,
#                          tmax,
#                          tmin,
#                          rs,
#                          rhmax,
#                          rhmin,
#                          uz){
#
#   Cp = 0.001013
#
#   tmean <- (tmax + tmin)/2
#   es_tmean <- sat_vapor_pressure(tmean)
#   es_tmax <- sat_vapor_pressure(tmax)
#   es_tmin <- sat_vapor_pressure(tmin)
#
#   delta <- 4098.0 * es_tmean / (tmean + 237.3)^2
#   lambda <- 2.501 - 0.002361 * tmean
#   pressure <- 101.3 * ((293.0 - 0.0065 * elevation) / 293.0)^5.26
#   gamma <- Cp * pressure / (0.622 * lambda)
#
#   canopy_resistance <- canopy_resistance/86400
#   aero_resistance <- aero_resistance(uz, screen_height, crop_height)
#
#   ea <- vapor_pressure(es_tmax, es_tmin, rhmax, rhmin)
#
#   vapor_pressure_deficit <-
#     vapor_pressure_deficit(es_tmax, es_tmin, ea)
#
#   et_rad_term = delta*net_radiation/(delta + gamma*(1+ rc/ra))
#
#   # Sum Aerodynamic and Radiation Terms
#   ref_et <- et_aero_term + et_rad_term
#   return(ref_et)
#
# }


