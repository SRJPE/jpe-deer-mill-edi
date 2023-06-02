library(tidyverse)
library(lubridate)
# save cleaned data to `data/`
# raw data were acquired from Ryan Revnak and are described and QCd here:
# https://github.com/SRJPE/JPE-datasets/blob/main/data-raw/qc-markdowns/rst/deer-creek/deer_creek_rst_data_qc.Rmd
# https://github.com/SRJPE/JPE-datasets/blob/main/data-raw/qc-markdowns/rst/mill-creek/mill_creek_rst_qc.Rmd


# Recent data from 2022 were scanned by Ryan Revnak and team and entered by FlowWest team
# Release and recaptures only for 2021/2022 - mill creek (not on deer creek)
deer_rst <- read_csv("data-raw/deer_rst.csv")
mill_rst <- read_csv("data-raw/mill_rst.csv")
glimpse(deer_rst)
glimpse(mill_rst)

# historical --------------------------------------------------------------

# catch
# date, location, count, fork_lenght, weight
catch_historical <- deer_rst |>
  select(date, location, count, fork_length, weight) |>
  bind_rows(mill_rst |>
              select(date, location, count, fork_length, weight)) |>
  rename(stream = location) |>
  mutate(stream = ifelse(grepl("Deer", stream), "deer creek", "mill creek"),
         species = "chinook salmon",
         lifestage = NA,
         mort = F,
         is_plus_count = F)

# trap
# date, location, flow, time_for_10_revolutions, tubs_debris, trap_condition_code, turbidity, weather, water_temp
trap_historical <- deer_rst |>
  select(date, location, flow, time_for_10_revolutions, tubs_of_debris, trap_condition_code, turbidity, weather, water_temperature_celsius) |>
  rename(water_temperature = water_temperature_celsius) |>
  bind_rows(mill_rst |>
              select(date, location, flow, time_for_10_revolutions, tubs_of_debris, trap_condition_code, water_temperature, turbidity, weather)) |>
  rename(stream = location) |>
  mutate(stream = ifelse(grepl("Deer", stream), "deer creek", "mill creek"),
         # convert time for 10 revolutions to rev per minute (10/time_for_10_rev *x/60)
         # assumes this is taken when first arrive at trap
         rpm_before = ifelse(time_for_10_revolutions > 0, 600/time_for_10_revolutions, 0),
         # these are errors
         rpm_before = ifelse(rpm_before > 15, NA, rpm_before),
         # assumed one tub is 5 gallons
         debris_gal = tubs_of_debris*5) |>
  group_by(date, stream) |>
  distinct() |>
  select(-time_for_10_revolutions, -tubs_of_debris)

# 2021 --------------------------------------------------------------------

# catch 2021
catch_2021_raw <- read_csv("data-raw/mill_2021_catch.csv")

# trap 2021
trap_2021_raw <- read_csv("data-raw/mill_2021_trap.csv")

# release
release_2021_raw <- read_csv("data-raw/mill_2021_release.csv")

# recapture
recapture_2021_raw <- read_csv("data-raw/mill_2021_recaptures.csv")

# lookup tables -----------------------------------------------------------
species_lookup <- tibble(species = unique(catch_2021_raw$species),
                         species_clean = c("chinook salmon",
                                     "rainbow trout",
                                     "riffle sculpin",
                                     "hard head",
                                     "sacramento sucker",
                                     "ammocoete",
                                     "speckled dace",
                                     "pacific lamprey",
                                     "sacramento pikeminnow",
                                     "pacific lamprey",
                                     "california roach",
                                     "speckled dace",
                                     "bullfrog",
                                     "prickley scuplpin",
                                     "rainbow trout",
                                     "rainbow trout",
                                     "pacific lamprey",
                                     "hard head",
                                     "riffle sculpin",
                                     "sacramento pikeminnow",
                                     "speckled dace",
                                     "ammocoete",
                                     "riffle sculpin",
                                     "riffle sculpin",
                                     "sucker",
                                     "hard head",
                                     "sacramento pikeminnow",
                                     "hard head",
                                     "prickley sculpin",
                                     "steelhead",
                                     "riffle sculpin",
                                     "sacramento pikeminnow",
                                     "pacific lamprey",
                                     "prickley sculpin",
                                     "sacramento pikeminnow",
                                     "riffle sculpin",
                                     "california roach",
                                     "sacramento pikeminnow",
                                     "pacific lamprey",
                                     "pacific lamprey",
                                     "hard head",
                                     "california roach",
                                     "hard head",
                                     "pacific lamprey",
                                     "sacramento pikeminnow",
                                     "steelhead",
                                     "ammocoete",
                                     "california roach"))
lifestage_lookup <- tibble(lifestage = unique(catch_2021_raw$lifestage),
                         lifestage_clean = c("silvery parr",
                                       NA,
                                       "smolt",
                                       "parr",
                                       "fry",
                                       "sac fry",
                                       "fry",
                                       "parr",
                                       "silvery parr",
                                       "smolt",
                                       "silvery parr",
                                       "sac fry",
                                       NA,
                                       NA,
                                       NA,
                                       NA,
                                       NA))


# cleaning ----------------------------------------------------------------

catch_2021_mort <- filter(catch_2021_raw, mort > 0) |>
  select(stream, date, species, mort) |>
  rename(count = mort) |>
  mutate(mort = T)
catch_2021_clean <- catch_2021_raw |>
  filter(is.na(mort) | mort == 0) |>
  mutate(mort = F) |>
  bind_rows(catch_2021_mort) |>
  # standardize species and lifestage
  left_join(species_lookup) |>
  left_join(lifestage_lookup) |>
  mutate(species = species_clean,
         lifestage = lifestage_clean) |>
  select(-c(lifestage_clean, species_clean))


#filter(trap_2021_raw, !is.na(debris_gal))
# 1= sunny, 2= partly cloudy, 3= cloudy, 4= rain, 5= snow, and 6= fog (using the codes in the JPE-datasets markdown)
trap_2021_clean <- trap_2021_raw |>
  rename(water_temperature = water_temp,
         flow = flow_cfs,
         rpm_before = before_rpms,
         rpm_after = after_rpms,
         weather = weather_code) |>
  mutate(time = hms::as_hms(time),
         date = as_datetime(paste0(date, " ", time)),
         weather = case_when(weather == 1 ~ "sunny",
                             weather == 2 ~ "partly cloudy",
                             weather == 3 ~ "cloudy",
                             weather == 4 ~ "rainy",
                             weather == 5 ~ "snow",
                             weather == 6 ~ "fog")) |>
  select(-c(trap_location, comments, debris_code, turbidity_ntu, hours_fished, total_revs, time))

releases_2021_clean <- release_2021_raw |>
  rename(min_rotations = min_rotations_during_efficiency,
         max_rotations = max_rotations_during_efficiency_test,
         min_flow = min_flow_cfs,
         max_flow = max_flow_cfs,
         mean_flow = mean_flow_cfs,
         min_fork_length = min_fl_released,
         max_fork_length = max_fl_released,
         mean_fork_length = mean_fl_released,
         release_lifestage = life_stage_released,
         number_released = total_number_released,
         turbidity = turbidity_ntu,
         recapture_start_date = recapture_begining_date,
         recapture_start_time = recapture_begining_time) |>
  mutate(release_time = hms::as_hms(release_time),
         release_date = as_datetime(paste0(release_date, " ", release_time)),
         recapture_start_time = hms::as_hms(recapture_start_time),
         recapture_start_date = as_datetime(paste0(recapture_start_date, " ", recapture_start_time)),
         recapture_end_time = hms::as_hms(recapture_end_time),
         recapture_end_date = as_datetime(paste0(recapture_end_date, " ", recapture_end_time))) |>
select(-c(trap_location, min_fl_recaptured, max_fl_recaptured, mean_fl_recaptured, total_recaptured, estimated_efficiency, `pdf_page_#`, total_number_marked, recapture_start_time, recapture_end_time, release_time))

recaptures_2021_clean <- recapture_2021_raw
# combine -----------------------------------------------------------------

catch_clean <- bind_rows(catch_historical,
                         catch_2021_clean) |>
  select(-mort, -is_plus_count)
write_csv(catch_clean, "data/deer_mill_catch_edi.csv")
trap_clean <- bind_rows(trap_historical,
                        trap_2021_clean) |>
  # this is gage data so not useful to have here too
  select(-flow) |>
  select(date, stream, trap_condition_code, turbidity, weather, water_temperature, debris_gal, rpm_before, rpm_after)
write_csv(trap_clean, "data/deer_mill_trap_edi.csv")
release_clean <- releases_2021_clean |>
  select(-c(min_fork_length, max_fork_length, mean_fork_length, release_lifestage, mean_flow, mark_color, turbidity,
            min_flow, max_flow, mean_flow))
write_csv(release_clean, "data/deer_mill_release_edi.csv")
recapture_clean <- recaptures_2021_clean
write_csv(recapture_clean, "data/deer_mill_recapture_edi.csv")
