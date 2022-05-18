#' Setup CropSyst
#'
#' @param file_path if provided returns CropSyst input data
#'
#' @return Empty CropSyst object
#' @export
#'
#' @examples cs_setup()
cs_setup <- function(file_path = NULL){
  if(!is.null(file_path)){
    return(NULL)
  } else {

    cs <- structure(list(), class = "cropsyst")
    # Default no irrigation
    cs$params <- list(irrigated_fraction = 0, auto_irrigation = FALSE, overhead_irrigation = FALSE)
    return(cs)
  }
}

#' Add weather data to cropsyst object
#'
#' @param cs cropsyst object
#' @param weather weather data frame
#' @param latitude location latitude
#' @param elevation elevation (m)
#' @param screening_height Height of wind measurement (m)
#'
#' @return cropsyst object with added weather data and parameters
#' @export
#'
#' @examples cs_add_weather(cs_setup(), cs_weather, 45, 1000, 2)
cs_add_weather <- function(cs, weather, latitude, elevation, screening_height){
  cs$weather <- weather
  cs$params <- append(cs$params,
                      list(latitude = latitude,
                           elevation = elevation,
                           screening_height = screening_height))
  return(cs)
}

#' Add soil data to cropsyst object
#'
#' @param cs cropsyst object
#' @param soil soil data frame
#' @param thickness_evaporative_layer thickness of soil evaporative layer
#'
#' @return cropsyst object with added soil data and parameters
#' @export
#'
#' @examples cs_add_soil(cs_setup(), cs_soil, 0.005)
cs_add_soil <- function(cs, soil, thickness_evaporative_layer){
  cs$soil <- soil
  cs$params <- append(cs$params,
                      list(thickness_evaporative_layer = thickness_evaporative_layer))

  return(cs)
}

#' Add canopy cover data to cropsyst object
#'
#' @param cs cropsyst object
#' @param canopy_cover canopy cover data frame
#'
#' @return cropsyst object with canopy cover data
#' @export
#'
#' @examples cs_add_canopy_cover(cs_setup(), cs_canopy_cover)
cs_add_canopy_cover <- function(cs, canopy_cover){
  cs$canopy_cover <- canopy_cover
  return(cs)
}

#' Add irrigation data to cropsyst object
#'
#' @param cs cropsyst object
#' @param irrigation irrigation data frame if missing auto irriagtion is applied
#' @param irrigated_fraction fraction of land irrigated in simulated area
#' @param max_allowable_depletion Max soil water dfepletion which triggers irrigation
#' @param min_leaf_water_potential Min leaf water potential which triggers irrigation
#' @param max_system_capacity Max irrigation daily capacity
#' @param dae_first_irrigation Day after emergence of first irrigation
#' @param overhead_irrigation logical is irrigation applied overhead defaults to FALSE
#'
#' @return cropsyst object with added irrigation data and parameters
#' @export
#'
#' @examples cs_add_irrigation(cs_setup(), cs_irrigation, 0.001)
cs_add_irrigation <- function(cs, irrigation,
                              irrigated_fraction,
                              max_allowable_depletion,
                              min_leaf_water_potential,
                              max_system_capacity,
                              dae_first_irrigation,
                              overhead_irrigation = FALSE){

  if(!missing(irrigation)){
    cs$irrigation <- irrigation
  } else {
    if(!missing(max_allowable_depletion)){
      cs$params$max_allowable_depletion = max_allowable_depletion
    } else if(!missing(min_leaf_water_potential)){
      cs$params$min_leaf_water_potential = min_leaf_water_potential
    } else {
      stop("Please provide irrigation schedule or if using auto irrigation max_allowable_depletion or min_leaf_water_potential")
    }
    cs$params$auto_irrigation = TRUE
    cs$params$max_system_capacity <- max_system_capacity
    # cs$params$min_depth_water_application <- min_depth_water_application
    cs$params$dae_first_irrigation <- dae_first_irrigation
    cs$params$overhead_irrigation <- overhead_irrigation
  }

  cs$params$irrigated_fraction <- irrigated_fraction

  return(cs)
}

#' Add crop parameters to cropsyst object
#'
#' @param cs cropsyst object
#' @param planting_date Planting date
#' @param harvest_date Harvest date
#' @param midseason_et_crop_coef mid season ET crop coefficient
#' @param tree_fruit logical TRUE if tree fruit crop
#' @param fruit_harvest_date harvest date of tree fruit
#' @param max_crop_height max crop height (m)
#' @param max_root_depth max root depth (m)
#' @param perrenial logical TRUE if perennial
#' @param max_water_uptake max water uptake
#' @param transpiration_use_eff transpiration use efficiency
#' @param C3 C3 = TRUE c4 = FALSE
#'
#' @return cropsyst object with added crop parameters
#' @export
#'
cs_add_crop <- function(cs, planting_date, harvest_date,
                        midseason_et_crop_coef, tree_fruit,
                        fruit_harvest_date, max_crop_height,
                        max_root_depth, perrenial,
                        max_water_uptake,
                        transpiration_use_eff, C3){
  cs$params <- append(cs$params,
                      list(planting_date = planting_date,
                           harvest_date = harvest_date,
                           midseason_et_crop_coef = midseason_et_crop_coef,
                           tree_fruit = tree_fruit,
                           fruit_harvest_date = fruit_harvest_date,
                           max_crop_height = max_crop_height,
                           max_root_depth = max_root_depth,
                           perrenial = perrenial,
                           max_water_uptake = max_water_uptake,
                           transpiration_use_eff = transpiration_use_eff,
                           C3 = C3))
  return(cs)
}


#' Run CropSyst Model
#'
#' @param cs cropsyst object
#'
#' @return cropsyst model output
#' @export
#'
cs_run <- function(cs){

  # calculate reference et and daytime vpd
  cs$weather <-
    cs_process_weather(cs$weather,
                       cs$params$latitude,
                       cs$params$elevation,
                       cs$params$screening_height)

  # process soil and add inital water content if missing
  cs$soil <- cs_process_soil(cs$soil, cs$params$thickness_evaporative_layer)
  if (!"initial_wc" %in% names(cs$soil)) {
    cs$soil <- initialize_wc(cs$weather, cs$soil, cs$params$planting_date)
  }

  # filter weather to crop growth period and add day after planting variable
  cs$weather <- cs$weather[cs$weather$date >= cs$params$planting_date &
                             cs$weather$date <= cs$params$harvest_date, ]
  cs$weather$dap <- seq.int(nrow(cs$weather))

  # Add Canopy cover data to weather data frame
  cs$weather$gcc <- dplyr::lead(c(cs$canopy_cover$gcc,
                      rep(1E-6, nrow(cs$weather) - nrow(cs$canopy_cover))), default = 1E-6)
  cs$weather$tcc <- dplyr::lead(c(cs$canopy_cover$tcc,
                      rep(1E-6,nrow(cs$weather) - nrow(cs$canopy_cover))), default = 1E-6)

  # lead canopy cover variables
  # lead_total_canopy_cover <-
  #   c(cs$canopy_cover$tcc[2:length(cs$canopy_cover$tcc)],
  #     rep(max(cs$weather$tcc), nrow(cs$weather) - nrow(cs$canopy_cover) + 1))
  # lead_green_canopy_cover <-
  #   c(cs$canopy_cover$gcc[2:length(cs$canopy_cover$gcc)],
  #     rep(0, nrow(cs$weather) - nrow(cs$canopy_cover) + 1))

  # cs$weather$gcc <- lead_green_canopy_cover
  # cs$weather$tcc <- lead_total_canopy_cover

  # calc potential crop et
  cs$weather <-
    calc_potential_crop_et(cs$weather,
                           cs$weather$tcc,
                           cs$weather$gcc,
                           cs$params$midseason_et_crop_coef,
                           cs$params$tree_fruit,
                           cs$params$fruit_harvest_date,
                           max_crop_height = cs$params$max_crop_height)



  # Calc Crop height and Root depth
  cs$weather$crop_height <- calc_crop_height(cs$params$max_crop_height, cs$weather$tcc, cs$params$tree_fruit) # why is this not using the day before?
  cs$weather$root_depth <- calc_root_depth(cs$params$max_root_depth, cs$weather$tcc, cs$params$perrenial)

  cs$weather <-
    calc_potential_biomass_production(cs$weather,
                                      cs$params$transpiration_use_eff,
                                      cs$params$C3)

  # Combine weather and irrigation data
  if("irrigation" %in% names(cs)){
    cs$weather <- merge(cs$weather, cs$irrigation, all.x = TRUE)
    cs$weather$irrigation[is.na(cs$weather$irrigation)] <- 0
  }
  else {
    cs$weather$irrigation <- 0
  }


  # Add variables for for loop
  cs$weather$canopy_interception <- 0
  cs$weather$today_canopy_interception <- 0

  cs$weather$irr_input <- 0
  cs$weather$non_irr_input <- 0

  # Water Depth
  cs$weather$irr_initial_water_depth <- NA
  cs$weather$non_irr_initial_water_depth <- NA

  # cs$weather$irr_drainage <- NA
  # cs$weather$non_irr_drainage <- NA

  cs$weather$actual_transpiration <- NA
  cs$weather$water_stress_index <- NA
  cs$weather$leaf_water_potential <- NA
  cs$weather$canopy_stress_factor <- NA
  cs$weather$canopy_evaporation <- NA

  cs$weather$irr_zone_soil_water_evap <- NA
  cs$weather$non_irr_zone_soil_water_evap <- NA

  cs$weather$actual_evapotranspiration <- NA

  # Water content for first layer
  irr_sublayer_wc <- rep(NA, 5)
  non_irr_sublayer_wc <- rep(NA, 5)

  update_wetted_layer <- FALSE
  update_non_wetted_layer <- FALSE

  water_density <- 1000

  cs$soil$wc_0 <- cs$soil$initial_wc

  cs$irr_soil <- cs$soil
  cs$non_irr_soil <- cs$soil

  for (i in cs$weather$dap) {

    if (i > 1) {
      # Calc canopy interception
      cs$weather <-
        calc_canopy_interception(cs$weather, i, cs$params$overhead_irrigation)
    }

    # calculate soil water input after canopy interception
    cs$weather$irr_input[i] <- max(cs$weather$precip[i] + cs$weather$irrigation[i] - cs$weather$today_canopy_interception[i], 0)
    cs$weather$non_irr_input[i] <- max(cs$weather$precip[i] - cs$weather$today_canopy_interception[i], 0)

    cs$weather$irr_initial_water_depth[i] <-
      sum(cs$irr_soil[[paste0("wc_", i-1)]]*cs$soil$layer_thickness*water_density)
    cs$weather$non_irr_initial_water_depth[i] <-
      sum(cs$non_irr_soil[[paste0("wc_", i-1)]]*cs$soil$layer_thickness*water_density)

    if (cs$weather$irr_input[i] > 0) {update_wetted_layer = TRUE}
    if (cs$weather$non_irr_input[i] > 0) {update_non_wetted_layer = TRUE}

    cs$irr_soil[[paste0("wc_", i)]] <-
      water_infiltration(cs$weather$irr_input[i],
                         cs$irr_soil[[paste0("wc_", i-1)]],
                         cs$soil$horizon_field_capacity,
                         cs$soil$layer_thickness)

    cs$non_irr_soil[[paste0("wc_", i)]] <-
      water_infiltration(cs$weather$non_irr_input[i],
                         cs$non_irr_soil[[paste0("wc_", i-1)]],
                         cs$soil$horizon_field_capacity,
                         cs$soil$layer_thickness)



    # Skip drainage for now
    # cs$weather$irr_drainage[i] <- pmax(cs$weather$irr_input[i] - max(irr_cum_capacity), 0)
    # cs$weather$non_irr_drainage[i] <- pmax(cs$weather$non_irr_input[i] - max(non_irr_cum_capacity), 0)

    if (cs$params$irrigated_fraction == 0) {
      actual_transpiration_out <-
        actual_transpiration(cs$weather, cs$non_irr_soil, i,
                             cs$params$max_water_uptake)
    } else {
      actual_transpiration_out <-
        actual_transpiration(cs$weather, cs$irr_soil, i,
                             cs$params$max_water_uptake)
    }

    cs$weather <- actual_transpiration_out$weather
    soil_water_uptake <- actual_transpiration_out$soil_water_uptake

    evap <-
      actual_soil_water_evaporation(cs$weather, cs$irr_soil, cs$non_irr_soil,
                                    i, cs$params$irrigated_fraction,
                                    update_wetted_layer = update_wetted_layer,
                                    update_non_wetted_layer = update_non_wetted_layer,
                                    irr_sublayer_wc,
                                    non_irr_sublayer_wc)




    cs$weather <- evap$weather
    cs$irr_soil <- evap$irr_soil
    cs$non_irr_soil <- evap$non_irr_soil

    update_wetted_layer <- evap$update_wetted_layer
    update_non_wetted_layer <- evap$update_non_wetted_layer

    irr_sublayer_wc <- evap$irr_sublayer_wc
    non_irr_sublayer_wc <- evap$non_irr_sublayer_wc

    final_wc <-
      final_wc_update(cs$weather, cs$irr_soil, cs$non_irr_soil, i,
                      cs$params$irrigated_fraction, soil_water_uptake)

    cs$irr_soil <- final_wc$irr_soil
    cs$non_irr_soil <- final_wc$non_irr_soil

    if(!"irrigation" %in% names(cs) & cs$params$irrigated_fraction > 0){
      if ("max_allowable_depletion" %in% names(cs$params)) {
        cs$weather <-
          schedule_irrigation(cs$weather, cs$irr_soil, i,
                              irrigated_fraction = cs$params$irrigated_fraction,
                              max_allowable_depletion = cs$params$max_allowable_depletion,
                              max_irrigation_capacity = cs$params$max_irrigation_capacity,
                              dae_first_irrigation = cs$params$dae_first_irrigation)
      } else if ("min_leaf_water_potential" %in% names(cs$params)){
        cs$weather <-
          schedule_irrigation(cs$weather, cs$irr_soil, i,
                              irrigated_fraction = cs$params$irrigated_fraction,
                              min_leaf_water_potential = cs$params$min_leaf_water_potential,
                              max_irrigation_capacity = cs$params$max_irrigation_capacity,
                              dae_first_irrigation = cs$params$dae_first_irrigation)
      }
    }


  }

  cs$weather$biomass_production <-
    biomass_production(cs$weather$actual_transpiration,
                       cs$weather$daytime_vpd,
                       cs$params$C3,
                       cs$params$transpiration_use_eff)

  cs$weather$cum_biomass_production <- cumsum(cs$weather$biomass_production)


  return(cs)
}
