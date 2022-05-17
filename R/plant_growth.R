calc_crop_height <- function(max_crop_height, total_canopy_cover, tree_fruit){
  if (tree_fruit) {
    return(max_crop_height)
  } else {
    return(max_crop_height*total_canopy_cover/max(total_canopy_cover))
  }
}

calc_root_depth <- function(max_root_depth, total_canopy_cover, perennial){
  if (perennial) {
    return(max_root_depth)
  } else {
    root_depth <- pmax(0.2, 0.2 + (max_root_depth - 0.2)*
                         total_canopy_cover/max(total_canopy_cover))

    return(cummax(root_depth))
  }
}

calc_temp_trapezoidal_response <- function(tmean, C3){
  if (C3) {
    tmin <- 0
    topt_low <- 10
    topt_high <- 25
    tmax <- 35
  } else {
    tmin <- 5
    topt_low <- 15
    topt_high <- 30
    tmax <- 45
  }

  tdr <- dplyr::case_when(!dplyr::between(tmean, tmin, tmax) ~ 0,
                          dplyr::between(tmean, topt_low, topt_high) ~ 1,
                          dplyr::between(tmean, tmin, topt_low) ~
                            1 - (topt_low - tmean)/(topt_low - tmin),
                          dplyr::between(tmean, topt_high, tmax) ~
                            (tmax - tmean)/(tmax - topt_high))
  return(tdr)
}

calc_potential_biomass_production <- function(weather, transpiration_use_eff, C3){
  tmean <- (weather$tmax + weather$tmin)/2
  # daytime_vpd <- pmax(daytime_vpd, 0.5)

  daily_transpiration_use_eff <- transpiration_use_eff/sqrt(weather$daytime_vpd)
  rue_temp_corr <- calc_temp_trapezoidal_response(tmean, C3)
  daily_radiation_use_eff <- (0.26 + 0.266*transpiration_use_eff)*rue_temp_corr
  daily_rue_biomass <- daily_radiation_use_eff*weather$gcc*weather$s_rad
  potential_daily_tue_biomass <- daily_transpiration_use_eff*weather$potential_transpiration

  weather$attainable_transpiration <-
    dplyr::if_else(daily_rue_biomass < potential_daily_tue_biomass,
           daily_rue_biomass / daily_transpiration_use_eff,
           weather$attainable_transpiration)

  weather$potential_biomass_prod <- pmin(potential_daily_tue_biomass, daily_rue_biomass)*10
  weather$cum_potential_biomass <- cumsum(weather$potential_biomass_prod)

  return(weather)

}

biomass_production <- function(actual_transpiration,
                               daytime_vpd,
                               C3,
                               transpiration_use_eff){

  daytime_vpd <- pmax(daytime_vpd, 0.5)

  daily_transpiration_use_eff <- transpiration_use_eff/sqrt(daytime_vpd)

  biomass_production <- daily_transpiration_use_eff*actual_transpiration*10

  return(biomass_production)

}
