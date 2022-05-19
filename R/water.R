calc_potential_crop_et <- function(weather, total_canopy_cover, green_canopy_cover, midseason_et_coef,
                          tree_fruit, fruit_harvest_date, max_crop_height){
  et_coef_at_canopy_cover_of_one <-
    (midseason_et_coef+max(total_canopy_cover)-1)/max(total_canopy_cover)

  adj_et_coef_cc_one <- et_coef_at_canopy_cover_of_one +
    (0.04*(weather$wind_speed-2) - 0.004*(weather$rh_min-45)) *
    (max_crop_height/3)^3

  weather$et_crop_coef <- 1 + (adj_et_coef_cc_one-1)*total_canopy_cover
  weather$et_crop_coef[tree_fruit & weather$date >= fruit_harvest_date] <- 0.2

  weather$potential_crop_et <- weather$et_crop_coef*weather$ref_et
  weather$potential_transpiration <- weather$potential_crop_et*green_canopy_cover
  weather$attainable_transpiration <- weather$potential_transpiration
  weather$attainable_soil_water_evaporation <- weather$potential_crop_et*
    (1 - weather$tcc)

  return(weather)

}


calc_canopy_interception <- function(weather, dae, overhead_irrigation){

  if (overhead_irrigation) {
    irrigation <- weather$irrigation[dae]
  } else {
    irrigation <- 0
  }
  max_canopy_interception  <- 1 #mm
  available_canopy_interception <- max_canopy_interception*weather$tcc[dae]

  weather$canopy_interception[dae] <-
    min(weather$canopy_interception[dae-1] + weather$precip[dae] + irrigation,
        available_canopy_interception)

  weather$today_canopy_interception[dae] <-
    weather$canopy_interception[dae] -
    weather$canopy_interception[dae-1]

  return(weather)
}

water_infiltration <- function(input, wc_day_before, field_capacity, thickness, dae){

  water_density <- 1000 #kg/m3
  update_wetted_layer <- FALSE

  wc <- wc_day_before

  available_storage_layer_1 <-
    (field_capacity[1]-wc_day_before[1])*thickness[1]*water_density

  irr_storage_layer_1 <- min(available_storage_layer_1, input)

  if (irr_storage_layer_1 > 0 | dae == 1) {
    update_wetted_layer <- TRUE
  }

  input <- max(input - available_storage_layer_1, 0)

  wc[1] <- wc_day_before[1] + (irr_storage_layer_1/(thickness[1]*water_density))

  layer_capacity <- (field_capacity[2:length(wc)] - wc_day_before[2:length(wc)])*
    (thickness[2:length(wc)]*water_density)

  cum_capacity <- cumsum(layer_capacity)

  input_left <- pmax(input - dplyr::lag(cum_capacity, default = 0), 0)

  wc[2:length(wc)] <-
    dplyr::if_else(input_left > layer_capacity,
                   field_capacity[2:length(wc)],
               (input_left/
                  (thickness[2:length(wc)]*water_density)) + wc_day_before[2:length(wc)])

  drainage <- min(input_left)

  return(list(wc = wc, update_wetted_layer = update_wetted_layer, drainage = drainage))
}

actual_transpiration <- function(weather, soil, dae, max_water_uptake_cc_one){

  soil <- soil[-1, ]

  # Hard Coded Parameters
  onset_of_stress <- -1200
  onset_canopy_stress <- -1000
  wilt <- -1600
  stop_canopy_expansion <- -1400
  water_potential_at_field_capacity <- -30

  if(weather$attainable_transpiration[dae] > weather$canopy_interception[dae]){
    transpiration_attainable <- weather$attainable_transpiration[dae] - weather$canopy_interception[dae]
    canopy_evaporation <- weather$canopy_interception[dae]
    weather$canopy_interception[dae] <- 0
  } else {
    weather$canopy_interception[dae] <- weather$canopy_interception[dae] - weather$attainable_transpiration[dae]
    canopy_evaporation <- weather$attainable_transpiration[dae]
    transpiration_attainable <- 1E-6
  }

  potential_transpiration <- transpiration_attainable
  frac_canopy_interception_sr <- weather$gcc[dae]
  max_water_uptake <- max_water_uptake_cc_one*frac_canopy_interception_sr

  attainable_crop_uptake <- min(potential_transpiration, max_water_uptake)

  plant_hydraulic_cond <- max_water_uptake / (-30 - onset_of_stress)

  # calculate root fraction
  root_fraction <- calc_root_fraction(soil$layer_thickness, weather$root_depth[dae])
  if (weather$root_depth[dae] > sum(soil$layer_thickness) & sum(root_fraction) < 1) {
    root_fraction <- root_fraction/sum(root_fraction)
  }

  # Adjust plant hydraulic conductance based on soil dryness
  root_hydraulic_cond <- plant_hydraulic_cond / 0.65
  top_hydraulic_cond <- plant_hydraulic_cond / 0.35

  soil_water_potential <-
    calc_soil_water_potential(soil$horizon_bulk_density,
                              soil$air_entry_potential,
                              soil$b_value,
                              soil[[paste0("wc_", dae)]])


  root_activity_factor <-
    dplyr::if_else(soil_water_potential>water_potential_at_field_capacity,
           1,
           1 - ((soil_water_potential - water_potential_at_field_capacity) /
                  (wilt - water_potential_at_field_capacity)) ^ 8)

  root_activity_factor <- pmin(root_activity_factor, 1)
  root_activity_factor <- pmax(root_activity_factor, 0)

  layer_root_conductance_adjustment <- root_activity_factor*root_fraction

  root_conductance_adjustment <- sum(layer_root_conductance_adjustment)
  layer_root_hydraulic_cond <- root_hydraulic_cond*layer_root_conductance_adjustment

  layer_top_hydraulic_conductance <-
    top_hydraulic_cond*layer_root_conductance_adjustment/
    root_conductance_adjustment


  layer_plant_hydraulic_conductance <-
    ifelse(layer_root_conductance_adjustment > 0,
            layer_top_hydraulic_conductance*layer_root_hydraulic_cond/
              (layer_root_hydraulic_cond + layer_top_hydraulic_conductance),
            0)

  root_hydraulic_cond <- root_hydraulic_cond*root_conductance_adjustment
  plant_hydraulic_cond <- (root_hydraulic_cond*top_hydraulic_cond)/
    (root_hydraulic_cond + top_hydraulic_cond)

  # Calculate Average Soil Water Potential
  average_soil_water_potential <-
    sum(soil_water_potential*layer_root_conductance_adjustment/
          root_conductance_adjustment)

  # Calculate Leaf Water Potential
  leaf_water_potential <- average_soil_water_potential -
    (attainable_crop_uptake/plant_hydraulic_cond)


  if (leaf_water_potential < onset_of_stress) {
    if (leaf_water_potential < wilt) {
      plant_hydraulic_conductance_adjustment <- 0.5
    } else {
      plant_hydraulic_conductance_adjustment <-
        0.5 + (0.5*((leaf_water_potential - wilt)/(onset_of_stress - wilt))^0.5)
    }
    adjusted_hydraulic_conductance <-
      plant_hydraulic_conductance_adjustment * plant_hydraulic_cond
    leaf_water_potential <-
      (adjusted_hydraulic_conductance*average_soil_water_potential*
         (onset_of_stress - wilt) + wilt * attainable_crop_uptake) /
      (adjusted_hydraulic_conductance*
         (onset_of_stress - wilt) + attainable_crop_uptake)
  }

  leaf_water_potential <- max(leaf_water_potential, wilt)

  # 'Reduce transpiration when leaf water potential is less than the critical leaf water potential at the onset of stomatal closure

  if (leaf_water_potential < onset_of_stress & leaf_water_potential > wilt) {
    transpiration <- attainable_crop_uptake*(leaf_water_potential - wilt)/
      (onset_of_stress - wilt)
    transpiration_ratio <- transpiration/attainable_crop_uptake
  } else {
    transpiration_ratio <- 1
  }

  if (leaf_water_potential > wilt) {
    soil_water_uptake <- layer_plant_hydraulic_conductance*
      (soil_water_potential - leaf_water_potential)*transpiration_ratio
  } else {
    soil_water_uptake <- rep(0, nrow(soil))
  }

  # calc crop water uptake

  crop_uptake <- sum(soil_water_uptake)

  water_stress_index <- ifelse(potential_transpiration > 0,
                1 - crop_uptake/potential_transpiration,
                0)

  canopy_stress_factor <-
    dplyr::case_when(leaf_water_potential < onset_canopy_stress &
                       leaf_water_potential >= stop_canopy_expansion ~
                       (leaf_water_potential - stop_canopy_expansion)/
                       (onset_canopy_stress - stop_canopy_expansion),
                     leaf_water_potential < onset_canopy_stress &
                       leaf_water_potential >= stop_canopy_expansion &
                       leaf_water_potential <= wilt ~ 0,
                     TRUE ~ 1)

  # crop_uptake <- if_else(leaf_water_potential < onset_canopy_stress &
  #                          leaf_water_potential >= stop_canopy_expansion )


  # Add variables to weather data frame
  weather$actual_transpiration[dae] <- crop_uptake + canopy_evaporation
  weather$water_stress_index[dae] <- water_stress_index
  weather$leaf_water_potential[dae] <- leaf_water_potential
  weather$canopy_stress_factor[dae] <- canopy_stress_factor
  weather$canopy_evaporation[dae] <- canopy_evaporation

  return(list(weather = weather, soil_water_uptake = soil_water_uptake))


}

calc_root_fraction <- function(thickness, root_depth){

  depth <- cumsum(thickness)

  root_frac <-
    dplyr::case_when(
      root_depth > depth ~ thickness*(2*(root_depth - depth) + thickness)/(root_depth^2),
      root_depth < depth - thickness + 1E-5 ~ 0,
      TRUE ~ ((root_depth - depth + thickness) / root_depth)^2
    )

  # root_frac <- rep(NA, length(thickness))
  # root_frac[root_depth > depth] <- thickness*(2*(root_depth - depth) + thickness)/(root_depth^2)
  # root_frac[root_depth < depth - thickness + 1E-5] <- 0
  # root_frac[is.na(root_frac)] <- ((root_depth - depth + thickness) / root_depth)^2

  if (root_depth > max(depth) & sum(root_frac) < 1) {
    root_frac <- root_frac/sum(root_frac)
  }

  return(root_frac)

}

calc_soil_water_potential <-
  function(bulk_density, air_entry_potential, campbell_b, water_content){
    saturation_water_content <- 1 - bulk_density/2.65
    water_potential <- air_entry_potential*(water_content/saturation_water_content)^(-campbell_b)
    return(water_potential)
  }

actual_soil_water_evaporation <- function(weather,
                                          irr_soil,
                                          non_irr_soil,
                                          dae,
                                          irrigated_fraction,
                                          update_wetted_layer,
                                          update_non_wetted_layer,
                                          irr_sublayer_wc,
                                          non_irr_sublayer_wc){
  n_evaporation_sublayers <- 5 #Number of evaporation sublayers of layer 1
  top_layer_thickness <- irr_soil$layer_thickness[1] / n_evaporation_sublayers
  pwp_top_layer <- irr_soil$horizon_permanent_wilting_point[1]
  air_dry_wc <- pwp_top_layer/3  #This is an approximation to air-dry water content
  delay_coef <- 0.9
  water_density <- 1000

  delay_factor <- 1 + delay_coef*((exp(1:n_evaporation_sublayers-1)^-0.5) - 1)

  irr_potential_evaporation <- weather$attainable_soil_water_evaporation[dae]*irrigated_fraction
  # Note to remove multiplication by non irrigated fraction?
  non_irr_potential_evaporation <- weather$attainable_soil_water_evaporation[dae]

  if(dae == 1 | update_wetted_layer){
    irr_sublayer_wc <- rep(irr_soil[[paste0("wc_", dae)]][1], n_evaporation_sublayers)
    update_wetted_layer <- FALSE
  }
  if(dae == 1 | update_non_wetted_layer){
    non_irr_sublayer_wc <- rep(non_irr_soil[[paste0("wc_", dae)]][1], n_evaporation_sublayers)
    update_non_wetted_layer <- FALSE
  }

  max_attainable_irr_soil_water_evap <-
    pmax((irr_sublayer_wc - air_dry_wc)*top_layer_thickness*water_density*
           delay_factor, 0)
  max_attainable_non_irr_soil_water_evap <-
    pmax((non_irr_sublayer_wc - air_dry_wc)*top_layer_thickness*water_density*
           delay_factor, 0)

  irr_soil_water_evap <- rep(NA, n_evaporation_sublayers)
  non_irr_soil_water_evap <- rep(NA, n_evaporation_sublayers)

  for (i in 1:n_evaporation_sublayers) {
    irr_soil_water_evap[i] <-
      dplyr::case_when(irr_sublayer_wc[i] > pwp_top_layer ~ irr_potential_evaporation,
                       irr_sublayer_wc[i] > air_dry_wc & irr_sublayer_wc[i] <= pwp_top_layer ~
                         irr_potential_evaporation*
                         ((irr_sublayer_wc[i] - air_dry_wc)/(pwp_top_layer - air_dry_wc))^2,
                       irr_sublayer_wc[i] <= air_dry_wc ~ 0)
    irr_soil_water_evap[i] <- min(irr_soil_water_evap[i], max_attainable_irr_soil_water_evap[i])
    irr_potential_evaporation <- max(0, irr_potential_evaporation - irr_soil_water_evap[i])

    non_irr_soil_water_evap[i] <-
      dplyr::case_when(non_irr_sublayer_wc[i] > pwp_top_layer ~ non_irr_potential_evaporation,
                       non_irr_sublayer_wc[i] > air_dry_wc & non_irr_sublayer_wc[i] <= pwp_top_layer ~
                         non_irr_potential_evaporation*
                         ((non_irr_sublayer_wc[i] - air_dry_wc)/(pwp_top_layer - air_dry_wc))^2,
                       non_irr_sublayer_wc[i] <= air_dry_wc ~ 0)
    non_irr_soil_water_evap[i] <- min(non_irr_soil_water_evap[i], max_attainable_non_irr_soil_water_evap[i])
    non_irr_potential_evaporation <- max(0, non_irr_potential_evaporation - non_irr_soil_water_evap[i])
  }

  # irr_soil_water_evap <-
  #   dplyr::case_when(irr_sublayer_wc > pwp_top_layer ~ irr_potential_evaporation,
  #                    irr_sublayer_wc > air_dry_wc & irr_sublayer_wc <= pwp_top_layer ~
  #                      irr_potential_evaporation*
  #                      ((irr_sublayer_wc - air_dry_wc)/(pwp_top_layer - air_dry_wc))^2,
  #                    irr_sublayer_wc <= air_dry_wc ~ 0)
  #
  # irr_soil_water_evap <- pmin(irr_soil_water_evap, max_attainable_irr_soil_water_evap)
  # irr_potential_evaporation <- pmax(irr_potential_evaporation - irr_soil_water_evap, 0)
  #
  # non_irr_soil_water_evap <-
  #   dplyr::case_when(non_irr_sublayer_wc > pwp_top_layer ~ non_irr_potential_evaporation,
  #                    non_irr_sublayer_wc > air_dry_wc & non_irr_sublayer_wc <= pwp_top_layer ~
  #                      non_irr_potential_evaporation*
  #                      ((non_irr_sublayer_wc - air_dry_wc)/(pwp_top_layer - air_dry_wc))^2,
  #                    non_irr_sublayer_wc <= air_dry_wc ~ 0)
  # non_irr_soil_water_evap <- pmin(non_irr_soil_water_evap, max_attainable_non_irr_soil_water_evap)
  # non_irr_potential_evaporation <- pmax(non_irr_potential_evaporation - non_irr_soil_water_evap, 0)

  irr_sublayer_wc <- irr_sublayer_wc - irr_soil_water_evap/(top_layer_thickness*water_density)
  non_irr_sublayer_wc <- non_irr_sublayer_wc - non_irr_soil_water_evap/(top_layer_thickness*water_density)

  irr_layer_1_wc = sum(irr_sublayer_wc/n_evaporation_sublayers)
  non_irr_layer_1_wc = sum(non_irr_sublayer_wc/n_evaporation_sublayers)

  today_irr_soil_water_evap = sum(irr_soil_water_evap)
  today_non_irr_soil_water_evap = sum(non_irr_soil_water_evap)

  irr_soil[[paste0("wc_", dae)]][1] <- irr_layer_1_wc
  non_irr_soil[[paste0("wc_", dae)]][1] <- non_irr_layer_1_wc

  weather$irr_zone_soil_water_evap[dae] <- today_irr_soil_water_evap
  weather$non_irr_zone_soil_water_evap[dae] <- today_non_irr_soil_water_evap

  weather$actual_evapotranspiration[dae] <-
    today_non_irr_soil_water_evap*(1 - irrigated_fraction) +
    today_irr_soil_water_evap*irrigated_fraction +
    weather$actual_transpiration[dae]

  return(list(weather = weather,
              irr_soil = irr_soil,
              non_irr_soil = non_irr_soil,
              update_wetted_layer = update_wetted_layer,
              update_non_wetted_layer = update_non_wetted_layer,
              irr_sublayer_wc = irr_sublayer_wc,
              non_irr_sublayer_wc = non_irr_sublayer_wc))

}


final_wc_update <-
  function(weather, irr_soil, non_irr_soil, dae, irrigated_fraction, soil_water_uptake){

    water_density <- 1000
    uptake_deficit <- 0
    for (i in 2:nrow(irr_soil)) {
      non_irr_soil[[paste0("wc_", dae)]][i] <- non_irr_soil[[paste0("wc_", dae)]][i] -
        (0/(non_irr_soil$layer_thickness[i]*water_density))

      irr_soil[[paste0("wc_", dae)]][i] <- irr_soil[[paste0("wc_", dae)]][i] -
        (soil_water_uptake[i-1]/(non_irr_soil$layer_thickness[i]*water_density))

      if (irr_soil[[paste0("wc_", dae)]][i] < irr_soil$horizon_permanent_wilting_point[i]) {
        uptake_deficit <- uptake_deficit +
          (irr_soil$horizon_permanent_wilting_point[i]-
             irr_soil[[paste0("wc_", dae)]][i])*irr_soil$layer_thickness[i]*water_density

        irr_soil[[paste0("wc_", dae)]][i] <- irr_soil$horizon_permanent_wilting_point[i]

      } else {
        if (uptake_deficit > 0) {
          available_deficit <-
            (irr_soil[[paste0("wc_", dae)]][i] -
               irr_soil$horizon_permanent_wilting_point[i])*
            irr_soil$layer_thickness[i]*water_density
          if (available_deficit > 0) {
            irr_soil[[paste0("wc_", dae)]][i] <- irr_soil[[paste0("wc_", dae)]][i] -
              uptake_deficit/(irr_soil$layer_thickness[i]*water_density)
            uptake_deficit <- 0
          } else {
            irr_soil[[paste0("wc_", dae)]][i] <- irr_soil$horizon_permanent_wilting_point[i]
            uptake_deficit <- uptake_deficit-available_deficit
          }
        }
      }
    }

    # non_irr_soil[[paste0("wc_", dae)]] <- non_irr_soil[[paste0("wc_", dae)]] -
    #   (0/(non_irr_soil$layer_thickness*water_density))
    #
    #
    # irr_soil[[paste0("wc_", dae)]] <- irr_soil[[paste0("wc_", dae)]] -
    #   (c(0, soil_water_uptake)/(irr_soil$layer_thickness*water_density))

    return(list(irr_soil = irr_soil, non_irr_soil = non_irr_soil))


}

schedule_irrigation <- function(weather, irr_soil, dae,
                                irrigated_fraction,
                                max_allowable_depletion,
                                min_leaf_water_potential,
                                max_irrigation_capacity,
                                dae_first_irrigation){

  water_density <- 1000

  layer_bottom_depth <- cumsum(irr_soil$layer_thickness)
  soil_water_depletion_mm <-
    sum(
      dplyr::if_else(layer_bottom_depth <= max(weather$root_depth),
                     (irr_soil$horizon_field_capacity - irr_soil[[paste0("wc_", dae)]])*
                       irr_soil$layer_thickness*water_density,
                     0))

  root_zone_water_depletion <-
    sum(
      dplyr::if_else(layer_bottom_depth <= weather$root_depth[dae]*0.7,
                     (irr_soil$horizon_field_capacity - irr_soil[[paste0("wc_", dae)]])*
                       irr_soil$layer_thickness*water_density, 0))

  root_zone_plant_available_water <-
    sum(
      dplyr::if_else(layer_bottom_depth <= weather$root_depth[dae]*0.7,
                     (irr_soil$horizon_field_capacity - irr_soil$horizon_permanent_wilting_point)*
                       irr_soil$layer_thickness*water_density, 0))

  root_zone_plant_available_water_depletion <-
    root_zone_water_depletion/root_zone_plant_available_water

  if (!missing(max_allowable_depletion)) {
    irrigate_today <-
      root_zone_plant_available_water_depletion > max_allowable_depletion &
      dae > dae_first_irrigation
  } else if (!missing(min_leaf_water_potential)){
    irrigate_today <-
      weather$leaf_water_potential[dae] < min_leaf_water_potential &
      dae > dae_first_irrigation
  }

  irrigation <- min(soil_water_depletion_mm, max_irrigation_capacity/irrigated_fraction)


  if (dae < max(weather$dap) & irrigate_today) {
    weather$irrigation[dae + 1] <- irrigation
  }

  return(weather)
}

