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
         debris_gal = tubs_of_debris*5,
         weather = ifelse(weather == "rain", "rainy", weather)) |>
  group_by(date, stream) |>
  distinct() |>
  select(-time_for_10_revolutions, -tubs_of_debris)

# 2021 --------------------------------------------------------------------

# catch 2021
catch_2021_raw <- read_csv("data-raw/mill_2021_catch.csv")

# trap 2021
trap_2021_raw <- read_csv("data-raw/mill_2021_trap.csv")

# release
releases_2021_clean <- read_csv("data-raw/mill_2021_release.csv") |>
  rename(min_rotations = min_rotations_during_efficiency,
         max_rotations = max_rotations_during_efficiency_test,
         min_flow = min_flow_cfs,
         max_flow = max_flow_cfs,
         mean_flow = mean_flow_cfs,
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
         recapture_end_date = as_datetime(paste0(recapture_end_date, " ", recapture_end_time)),
         min_fl_released = ifelse(min_fl_released == "not recorded", NA, min_fl_released),
         mean_fl_released = ifelse(mean_fl_released == "not recorded", NA, mean_fl_released),
         max_fl_released = ifelse(max_fl_released == "not recorded", NA, max_fl_released),
         stream = "mill creek") |>
  select(-c(trap_location, min_fl_recaptured, max_fl_recaptured, mean_fl_recaptured,
            total_recaptured, estimated_efficiency, `pdf_page_#`, total_number_marked,
            recapture_start_time, recapture_end_time, release_time, recapture_start_date,
            recapture_end_date, turbidity,
            mark_color, min_flow, max_flow, mean_flow, min_rotations, max_rotations, release_lifestage)) |>
  mutate(release_id = c("mill202101", "mill202102", "mill202103")) |> glimpse()

# recapture
recapture_2021_clean <- read_csv("data-raw/mill_2021_recaptures.csv") |>
  mutate(recapture_date = date) |>
  group_by(recapture_date) |>
  summarize(mean_fl_recaptured = mean(fork_length, na.rm = TRUE),
            min_fl_recaptured = min(fork_length, na.rm = TRUE),
            max_fl_recaptured = max(fork_length, na.rm = TRUE),
            total_recaptured = n()) |>
  mutate(release_id = c("mill202101", "mill202102", "mill202102", "mill202102", "mill202103", "mill202103", "mill202103", NA, NA, NA, NA),
         stream = "mill creek") |>
  filter(!is.na(release_id)) |>
  group_by(release_id, stream) |>
  summarize(recapture_date = as.Date(min(recapture_date)),
            mean_fl_recaptured = mean(mean_fl_recaptured, na.rm = TRUE),
            min_fl_recaptured = min(min_fl_recaptured, na.rm = TRUE),
            max_fl_recaptured = max(max_fl_recaptured, na.rm = TRUE),
            total_recaptured = sum(total_recaptured)) |> glimpse()


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


# 2022 --------------------------------------------------------------------

# catch 22 (haha)
mill_catch_2022_raw <- read_csv("data-raw/mill_2022_catch.csv") |>
  mutate(date = as.Date(date))
deer_catch_2022_raw <- read_csv("data-raw/deer_2022_catch.csv") |>
  mutate(date = as.Date(date))
catch_2022_raw <- bind_rows(mill_catch_2022_raw, deer_catch_2022_raw)

# trap 2022
mill_trap_2022_raw <- read_csv("data-raw/mill_2022_trap.csv") |>
  mutate(date = as.Date(date),
         time = as.character(time),
         stream = ifelse(stream == "mill", "mill creek", stream))
deer_trap_2022_raw <- read_csv("data-raw/deer_2022_trap.csv") |>
  mutate(time = hms::as_hms(time),
         time = as.character(time),
         date = as.Date(date))
trap_2022_raw <- bind_rows(mill_trap_2022_raw, deer_trap_2022_raw)

recapture_2021_clean |> glimpse()
releases_2021_clean |> glimpse()

# EFFICIENCY 2022
# removed old efficiency (just realeases and recaptures, inconsistant with new data and seemed inaccurate) data from mill shared May 13, 2024
mill_new_efficiency <- read_csv("data-raw/mill_efficiency_data_2022.csv") |>
  janitor::clean_names() |>
  select(-c(max_trap_rotations, min_trap_rotations, min_velocity, max_velocity,
            min_discharge, max_discharge, min_turbidity, max_turbidity, river_miles)) |>
  mutate(release_date = as.Date(release_date_time, format = "%m/%d/%y"),
         recapture_date = as.Date(recap_date_time, format = "%m/%d/%y"),
         mean_fl_released = ifelse(release_size_avg == 0, NA, release_size_avg),
         mean_fl_recaptured = ifelse(recap_size_avg == 0, NA, recap_size_avg)) |>
  rename(total_recaptured = total_recap,
         number_released = total_release) |>
  select(-c(release_date_time, recap_date_time, recap_size_avg, release_size_avg, estimated_efficiency)) |>
  mutate(release_id = c("mill202201", "mill202202", "mill202203", "mill202204", "mill202205", "mill202206", "mill202207"),
         stream = "mill creek",
         release_origin = "hatchery") |> glimpse()

mill_2022_release <- mill_new_efficiency |> select(release_id, stream, release_origin, release_date, number_released, mean_fl_released) |> glimpse()
mill_2022_recaptured <- mill_new_efficiency |> select(release_id, stream, recapture_date, total_recaptured, mean_fl_recaptured) |> glimpse()

deer_new_efficiency <- read_csv("data-raw/deer_efficiency_data_2022.csv") |>
  janitor::clean_names() |>
  select(-c(max_trap_rotations, min_trap_rotations, min_velocity, max_velocity,
            min_discharge, max_discharge, min_turbidity, max_turbidity, river_miles)) |>
  mutate(release_date = as.Date(release_date_time, format = "%m/%d/%y"),
         recapture_date = as.Date(recap_date_time, format = "%m/%d/%y"),
         mean_fl_released = ifelse(release_size_avg == 0, NA, release_size_avg),
         mean_fl_recaptured = ifelse(recap_size_avg == 0, NA, recap_size_avg)) |>
  rename(total_recaptured = total_recap,
         number_released = total_release) |>
  select(-c(release_date_time, recap_date_time, recap_size_avg, release_size_avg, estimated_efficiency)) |>
  mutate(release_id = c("deer202201", "deer202202", "deer202203", "deer202204", "deer202205", "deer202206"),
         stream = "deer creek",
         release_origin = "hatchery") |> glimpse()

deer_2022_release <- deer_new_efficiency |> select(release_id, stream, release_origin, release_date, number_released, mean_fl_released) |> glimpse()
deer_2022_recaptured <- deer_new_efficiency |> select(release_id, stream, recapture_date, total_recaptured, mean_fl_recaptured) |> glimpse()


# 2022 lookup tables ------------------------------------------------------
species_lookup_2022 <- tibble(species = unique(catch_2022_raw$species),
                         species_clean = c("chinook salmon",
                                           "rainbow trout",
                                           "hard head",
                                           "pacific lamprey",
                                           "pacific lamprey",
                                           "sacramento pikeminnow",
                                           "prickly sculpin",
                                           "riffle sculpin",
                                           "river lamprey",
                                           "ammocoete",
                                           "sacramento sucker",
                                           "speckled dace",
                                           "prickly sculpin",
                                           "california roach",
                                           "cyprinid fry",
                                           "riffle sculpin",
                                           "california roach",
                                           "california roach",
                                           "pacific lamprey",
                                           "hard head",
                                           NA,
                                           "pacific lamprey",
                                           "rainbow trout",
                                           "river lamprey",
                                           "sacramento pikeminnow",
                                           "california roach",
                                           "sacramento sucker",
                                           "rainbow trout",
                                           "riffle sculpin",
                                           "california roach",
                                           "prickly sculpin",
                                           "unknown sculpin",
                                           "unknown fry"))
lifestage_lookup_2022 <- tibble(lifestage = unique(catch_2022_raw$lifestage),
                           lifestage_clean = c("smolt",
                                               "silvery parr",
                                               NA,
                                               "parr",
                                               NA,
                                               NA,
                                               NA,
                                               "fry",
                                               NA,
                                               NA,
                                               "sac fry",
                                               "alevin"))

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

catch_2022_mort <- filter(catch_2022_raw, mort > 0) |>
  select(stream, date, species, mort) |>
  rename(count = mort) |>
  mutate(mort = T)
catch_2022_clean <- catch_2022_raw |>
  filter(is.na(mort) | mort == 0) |>
  mutate(mort = F) |>
  bind_rows(catch_2022_mort) |>
  # standardize species and lifestage
  left_join(species_lookup_2022) |>
  left_join(lifestage_lookup_2022) |>
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

trap_2022_clean <- trap_2022_raw |>
  rename(water_temperature = water_temp,
         flow = flow_cfs,
         rpm_before = before_rpms,
         rpm_after = after_rpms,
         weather = weather_code) |>
  mutate(date = as_datetime(paste0(date, " ", time)),
         weather = case_when(weather == 1 ~ "sunny",
                             weather == 2 ~ "partly cloudy",
                             weather %in% c(3, "windy/cloudy") ~ "cloudy",
                             weather == 4 ~ "rainy",
                             weather == 5 ~ "snow",
                             weather == 6 ~ "fog")) |>
  select(-c(trap_location, comments, hours_fished, total_revs, time))






# combine -----------------------------------------------------------------
# Catch
catch_clean <- bind_rows(catch_historical,
                         catch_2021_clean,
                         catch_2022_clean) |>
  select(-mort, -is_plus_count)
write_csv(catch_clean, "data/deer_mill_catch_edi.csv")
# Trap
trap_clean <- bind_rows(trap_historical,
                        trap_2021_clean,
                        trap_2022_clean) |>
  # this is gage data so not useful to have here too
  select(-flow) |>
  select(date, stream, trap_condition_code, turbidity, weather, water_temperature, debris_gal, rpm_before, rpm_after) |>
  rename(trap_condition = trap_condition_code)
write_csv(trap_clean, "data/deer_mill_trap_edi.csv")

# Release
release_clean <- bind_rows(releases_2021_clean |> mutate(release_date = as.Date(release_date)), mill_2022_release, deer_2022_release) |>
  select(-c(min_fl_released, max_fl_released)) |>  glimpse() # remove because all NA
write_csv(release_clean, "data/deer_mill_release_edi.csv")

# Recaptures
recapture_clean <- bind_rows(recapture_2021_clean |> ungroup(), mill_2022_recaptured, deer_2022_recaptured) |> glimpse()
write_csv(recapture_clean, "data/deer_mill_recapture_edi.csv")
