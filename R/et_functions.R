#' Calculate saturation vapor pressure
#'
#' @param temp Air temperature (°C)
#'
#' @return Saturation Vapor Pressure (kPa)
#'
#' @examples cropsyst:::calc_sat_vapor_pressure(32)
calc_sat_vapor_pressure <- function(temp) {
  return(0.6108 * exp(17.27 * temp / (temp + 237.3)))
}

#' Calculate vapor pressure
#'
#' @param es_tmax Saturation Vapor Pressure at Maximum Temperature (°C)
#' @param es_tmin Saturation Vapor Pressure at Minimum Temperature (°C)
#' @param rhmax Maximum relative humidity (%)
#' @param rhmin Minimum relative humidity (%)
#'
#' @return Vapor Pressure (kPA)
#'
#' @examples cropsyst:::calc_vapor_pressure(5, 3, 100, 40)
calc_vapor_pressure <- function(es_tmax, es_tmin, rhmax, rhmin) {
  return(((es_tmin * rhmax / 100.0) + (es_tmax * rhmin / 100.0)) / 2)
}

#' Calculate vapor pressure deficit
#'
#' @param es_tmax Saturation Vapor Pressure at Maximum Temperature (°C)
#' @param es_tmin Saturation Vapor Pressure at Minimum Temperature (°C)
#' @param ea Vapor Pressure at Air Temperature (°C)
#'
#' @return Vapor Pressure Deficit (kPa)
#'
#' @examples cropsyst:::calc_vapor_pressure_deficit(5, 3, 4)
calc_vapor_pressure_deficit <- function(es_tmax, es_tmin, ea){
  return(((es_tmax + es_tmin) / 2) - ea)
}

#' Calculate potential radiation
#'
#' @param lat latitude (degrees)
#' @param doy day of year
#'
#' @return Potential Radiation (MJ/m2/day)
#'
#' @examples cropsyst:::calc_potential_radiation(45, 233)
calc_potential_radiation <- function(lat, doy){
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

#' Calculate daily net radiation
#'
#' @param pot_rad Potential solar radiation (MJ/m2/day)
#' @param sw_rad Shortwave radiation (MJ/m2/day)
#' @param ea Daily average vapor pressure (kPa)
#' @param tmax Daily maximum temperature (C)
#' @param tmin Daily minimum temperature (C)
#' @param elevation elevation (m)
#'
#' @return Net Radiation (MJ/m2/day)
#'
#' @examples cropsyst:::calc_net_radiation(900, 800, 1, 30, 10, 300)
calc_net_radiation <- function(pot_rad, sw_rad, ea, tmax, tmin, elevation){
  albedo = 0.23
  Rns = (1.0 - albedo) * sw_rad
  # Rso = pot_rad * (0.75 + elevation * 2.e-5)
  Rso = pot_rad * (0.75)
  # Calculate cloud factor
  F_Cloud = 1.35 * (sw_rad / Rso) - 0.35
  # Calculate humidity factor
  F_Hum = (0.34 - 0.14 * sqrt(ea))
  # Calculate Isothermal LW net radiation
  LWR = 4.903e-9 * ((tmax + 273.0)^4 + (tmin + 273.0)^4) / 2.0
  Rnl = LWR * F_Cloud * F_Hum
  return(Rns - Rnl)
}

#' Calculate Aerodynamic Resistance
#'
#' @param wind_speed Wind speed (m/s)
#' @param height Wind measurement height (m)
#' @param crop_height Crop height (m)
#'
#' @return Aerodynamic resistance (day/m)
#'
#' @examples cropsyst:::calc_aero_resistance(rep(2, 30), 3, 1.4)
calc_aero_resistance <- function(wind_speed, height, crop_height){

  if (height == 2) {
    wind_speed_2m <- wind_speed
  } else {
    wind_speed_2m <- wind_speed*4.87/log(67.75*height-5.42)
  }

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

#' Calculate Daytime Vapor Pressure Deficit
#'
#' @param tmax Daily maximum temperature (C)
#' @param rhmin Daily minimum relative humidity
#'
#' @return Vapor Pressure Deficit (kPa)
#'
#' @examples cropsyst:::calc_daytime_vpd(30, 30)
calc_daytime_vpd <- function(tmax, rhmin){
  daytime_vpd <- calc_sat_vapor_pressure(tmax)*(1 - rhmin/100) * 0.7
  return(daytime_vpd)
}


#' Calculate Reference ET
#'
#' @param lat latitude (degrees)
#' @param elevation elevetation (m)
#' @param screen_height screening height (m)
#' @param crop_height crop height (m)
#' @param canopy_resistance canopy resistance (m/s^2)
#' @param doy day of year
#' @param tmax maximum daily temperature (C)
#' @param tmin minimum daily temperature (C)
#' @param rs solar radiation
#' @param rhmax maximum daily relative humidity
#' @param rhmin minimum daily relative humidity
#' @param uz wind speed at
#'
#' @return reference evapotranspiration (mm)
#'
#' @examples cropsyst:::calc_reference_et(45, 1000, 2, 2, 0.05, 30, 30, 20, 600, 100, 50, 10)
calc_reference_et <- function(lat,
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
                         uz){

  Cp = 0.001013

  tmean <- (tmax + tmin)/2
  es_tmean <- calc_sat_vapor_pressure(tmean)
  es_tmax <- calc_sat_vapor_pressure(tmax)
  es_tmin <- calc_sat_vapor_pressure(tmin)

  delta <- 4098.0 * es_tmean / ((tmean + 237.3)^2)
  lambda <- 2.501 - (0.002361 * tmean)
  pressure <- 101.3 * ((293.0 - 0.0065 * elevation) / 293.0)^5.26
  gamma <- Cp * pressure / (0.622 * lambda)

  canopy_resistance <- canopy_resistance/86400
  aero_resistance <- calc_aero_resistance(uz, screen_height, crop_height)

  ea <- calc_vapor_pressure(es_tmax, es_tmin, rhmax, rhmin)

  vapor_pressure_deficit <-
    calc_vapor_pressure_deficit(es_tmax, es_tmin, ea)

  potential_radiation <- calc_potential_radiation(lat, doy)
  net_radiation <- calc_net_radiation(potential_radiation, rs, ea, tmax, tmin, elevation)

  et_rad_term <- (delta*net_radiation/(delta + gamma*(1 + canopy_resistance/aero_resistance)))/lambda
  volumetric_heat_capacity <- Cp*(3.486*pressure/(1.01*(tmean + 273)))
  et_aero_term <- ((volumetric_heat_capacity*vapor_pressure_deficit/aero_resistance)/
    (delta + gamma * (1 + canopy_resistance/aero_resistance)))/lambda

  # Sum Aerodynamic and Radiation Terms
  ref_et <- et_aero_term + et_rad_term
  return(ref_et)

}

#' Process Weather File
#'
#' @param weather input weather data frame
#' @param lat latitude
#' @param elevation elevelation (m)
#' @param screen_height screening height (m)
#'
#' @return data frame with added variables reference_et and daytime_vpd
#'
#' @examples cropsyst:::cs_process_weather(cs_weather, 45, 1000, 2)
cs_process_weather <- function(weather, lat, elevation, screen_height){
  weather$ref_et <- calc_reference_et(lat = lat,
                                      elevation = elevation,
                                      screen_height = screen_height,
                                      doy = lubridate::yday(weather$date),
                                      tmax = weather$tmax,
                                      tmin = weather$tmin,
                                      rs = weather$s_rad,
                                      rhmax = weather$rh_max,
                                      rhmin = weather$rh_min,
                                      uz = weather$wind_speed)

  weather$daytime_vpd <- pmax(calc_daytime_vpd(weather$tmax, weather$rh_min), 0.1)

  return(weather)
}


