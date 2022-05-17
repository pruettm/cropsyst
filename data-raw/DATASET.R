## code to prepare `cs_weather` dataset goes here
library(readr)
library(dplyr)

cs_weather <- read_tsv("../cropsyst_examples/excel/Data/Bushland-2016.dat",
                       col_names = c("year", "doy", "precip", "tmax", "tmin",
                                     "s_rad", "rh_max", "rh_min",
                                     "wind_speed")) |>
  mutate(date = ydoy(year, doy)) |>
  select(date, everything(), -year, -doy)

usethis::use_data(cs_weather, overwrite = TRUE)

## code to prepare `cs_soil` dataset goes here

cs_soil <- read_tsv("../cropsyst_examples/excel/Data/Bushland Soil 2016-SE.dat",
                    skip = 1,
                    col_names = c("horizon_thickness", "sand_pct",
                                  "clay_pct", "initial_wc")) |>
  mutate(silt_pct = 100-clay_pct-sand_pct)


usethis::use_data(cs_soil, overwrite = TRUE)

## code to prepare `cs_canopy_cover` dataset goes here

cs_canopy_cover <-
  read_tsv("../cropsyst_examples/excel/Data/Canopy Cover 2016-SE.dat",
           col_names = c("gcc", "tcc")) |>
  mutate(dap = row_number())


usethis::use_data(cs_canopy_cover, overwrite = TRUE)

## code to prepare `cs_canopy_cover` dataset goes here

cs_irrigation <-
  read_tsv("../cropsyst_examples/excel/Data/Irrigation 2016-SE.dat",
           col_names = c("dap", "irrigation"))

usethis::use_data(cs_irrigation, overwrite = TRUE)

## code to prepare `cs_crop` dataset goes here


cs_crop <-
  readxl::read_excel("../cropsyst_examples/excel/CropSyst ET.xlsm",
                     sheet = "Crop Parameters", range = "E8:O68",
                     col_names = c("crop", "empty", "card_temp_min", "card_temp_opt",
                                   "card_temp_max", "midseason_et_crop_coef",
                                   "max_crop_height", "max_root_depth",
                                   "max_allowable_depletion", "empty", "transpiration_use_eff")) |>
  select(-starts_with(c("empty", "card")))

usethis::use_data(cs_crop, overwrite = TRUE)





