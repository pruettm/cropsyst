#' Calculate saturation water content
#'
#' @param sand_pct Percentage of sand in soil layer
#' @param clay_pct Percentage of clay in soil layer
#'
#' @return Saturation water content (m3/m3)
#'
#' @examples cropsyst:::calc_saturation_water_content(40, 30)
calc_saturation_water_content <- function(sand_pct, clay_pct){
  return(0.332 - 0.0007251 * sand_pct + (log(clay_pct) / log(10)) * 0.1276)
}

#' Calculate bulk density
#'
#' @param saturation_wc Saturation Water Content (m3/m3)
#'
#' @return Bulk density (kg/m3)
#'
#' @examples cropsyst:::calc_bulk_density(0.326)
calc_bulk_density <- function(saturation_wc){
  return(2.65 * (1 - saturation_wc))
}

#' Calculate Campbell "B" value
#'
#' @param sand_pct Percentage of sand in soil layer
#' @param clay_pct Percentage of clay in soil layer
#'
#' @return Campbell "B" value
#'
#' @examples cropsyst:::calc_b_value(40, 30)
calc_b_value <- function(sand_pct, clay_pct){
  return(-(-3.14 - 0.00222*clay_pct^2 - 3.484E-05*sand_pct^2*clay_pct))
}

#' Calculate air entry potential
#'
#' @param sand_pct Percentage of sand in soil layer
#' @param clay_pct Percentage of clay in soil layer
#' @param b_value Campbell "B" value
#' @param saturation_wc Saturation water content (m3/m3)
#'
#' @return Air entry potential (J/kg)
#'
#' @examples cropsyst:::calc_air_entry_potential(92, 3, -4.045, 0.326)
calc_air_entry_potential <-
  function(sand_pct, clay_pct , b_value, saturation_wc){
  a_value <- 100*exp(-4.396 - 0.0715*clay_pct - 0.000488*sand_pct^2 -
                       4.285E-05*sand_pct^2*clay_pct)
  return(-a_value*saturation_wc^(-b_value))
}

#' Calculate saturated hydraulic conductivity
#'
#' @param sand_pct Percentage of sand in soil layer
#' @param clay_pct Percentage of clay in soil layer
#' @param saturation_wc Saturation water content (m3/m3)
#'
#' @return Saturated hydraulic conductivity (kg s/m3)
#'
#' @examples cropsyst:::calc_saturated_hydraulic_conductivity(92, 3, 0.00341)
calc_saturated_hydraulic_conductivity <-
  function(sand_pct, clay_pct, saturation_wc){
  g <- 9.81 # (m/s^2)
  water_density <- 1000 #kg/m^3
  factor <- water_density / (g * 100 * 3600)
  saturated_hydraulic_conductivity <-
    factor*exp(12.012 - 0.0755*sand_pct +
                 (-3.895 + 0.03671*sand_pct - 0.1103*clay_pct +
                    0.00087546*clay_pct^2)*(1/saturation_wc))
  return(saturated_hydraulic_conductivity)
}

#' Calculate water potential at field capacity
#'
#' @param silt_pct Percentage of sand in soil layer
#' @param clay_pct Percentage of clay in soil layer
#'
#' @return Water potential at field capacity (m3/m3)
#'
#' @examples cropsyst:::calc_water_potential_field_capacity(92, 3)
calc_water_potential_field_capacity <- function(silt_pct, clay_pct){
  water_potential_field_capacity <-
    ifelse(silt_pct >= 80, -33, (-13.833 * log(clay_pct)) + 10.356)
  return(pmin(water_potential_field_capacity, -10))
}

#' Process input soil data frame
#'
#' @param soil a data frame with columns (horizon_thickness, sand_pct, clay_pct)
#' and can include intial_wc
#' @param thickness_soil_evap_layer thickness of evaporation layer
#'
#' @return soil data frame with added columns
#' saturation_wc (saturation water content),
#' horizon_bulk_density (bulk density),
#' b_value (Campbell "B" value),
#' air_entry_potential (air entry potential),
#' horizon_field_capacity (horizon field capacity),
#' horizon_permanent_wilting_point (horizon permanent wilting point),
#' layer_thickness (layer thickness), and
#' index (layer index)
#'
#' @examples cropsyst:::cs_process_soil(cs_soil, 0.005)
cs_process_soil <-
  function(soil, thickness_soil_evap_layer){

    # Calculate percent silt
    soil$silt_pct <- 100 - soil$clay_pct - soil$sand_pct

    # Saturation water content
    soil$saturation_wc <- calc_saturation_water_content(soil$sand_pct, soil$clay_pct)

    # Bulk density
    soil$horizon_bulk_density <- calc_bulk_density(soil$saturation_wc)

    # Campbell "B" value
    soil$b_value <- calc_b_value(soil$sand_pct, soil$clay_pct)

    # Air entry potential
    soil$air_entry_potential <-
      calc_air_entry_potential(soil$sand_pct, soil$clay_pct,
                               soil$b_value, soil$saturation_wc)

    # Water potential field capacity
    water_potential_field_capacity <-
      calc_water_potential_field_capacity(soil$silt_pct, soil$clay_pct)

    # Horizon field capacity
    soil$horizon_field_capacity <- soil$saturation_wc*(
      water_potential_field_capacity/soil$air_entry_potential)^(-1/soil$b_value)

    # Horizon permanent wilting point
    soil$horizon_permanent_wilting_point <- soil$saturation_wc*
      (-1500/soil$air_entry_potential)^(-1/soil$b_value)

    # expand data frame create layers 0.1 m thick
    n_repeats <- round(soil$horizon_thickness/0.1)
    soil <- as.data.frame(lapply(soil, rep, n_repeats))

    # split top layer into two layers with the top layer the
    # thickness of evaporative layer and second layer
    # 0.1m - thickness of evaporative layer
    n_repeats <- c(2, rep(1, nrow(soil) - 1))
    soil <- as.data.frame(lapply(soil, rep, n_repeats))

    # add layer thickness column
    soil$layer_thickness <- c(thickness_soil_evap_layer,
                              0.1 - thickness_soil_evap_layer,
                              rep(0.1, nrow(soil) - 2))

    soil$index = 1:nrow(soil)

    # return soil data frame
    return(soil)
}

initialize_wc <- function(weather, soil, planting_date){

  water_density <- 1000

  assign_PAW <- pmin(1, 0.05*soil$index)
  wc_day_before <- soil$horizon_permanent_wilting_point +
    (soil$horizon_field_capacity - soil$horizon_permanent_wilting_point)*
    assign_PAW

  start_date <- dplyr::if_else(lubridate::yday(planting_date) < 240,
                       ydoy(lubridate::year(planting_date) - 1, 240),
                       ydoy(lubridate::year(planting_date), 240))

  start_date <- dplyr::if_else(start_date < min(weather$date),
                               min(weather$date), start_date)

  wc_day_of <- wc_day_before


  for (i in seq.Date(start_date, planting_date, by = "day")) {
    ref_et <- weather$ref_et[weather$date == i]
    flux_in <- weather$precip[weather$date == i]

    wc_day_before[1] <-
      max(soil$horizon_permanent_wilting_point[1]/3,
          wc_day_before[1] - ref_et/(soil$layer_thickness[1]*water_density))


    layer_capacity <- (soil$horizon_field_capacity - wc_day_before)*
      (soil$layer_thickness*water_density)
    cum_capacity <- cumsum(layer_capacity)

    input_left <- flux_in - dplyr::lag(cum_capacity, default = 0)

    wc_day_of <-
      dplyr::if_else(input_left > layer_capacity,
                     soil$horizon_field_capacity,
                     (pmax(input_left, 0)/
                        (soil$layer_thickness*water_density)) + wc_day_before)


    wc_day_before <- wc_day_of

  }

  soil$initial_wc <- wc_day_of

  return(soil)
}




