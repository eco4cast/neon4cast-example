renv::activate()
renv::restore()

library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)

dir.create("drivers", showWarnings = FALSE)

forecast_date <- as.character(Sys.Date())

#Step 0: Define team name and team members 

team_name <- "air2waterSat"

team_list <- list(list(individualName = list(givenName = "Quinn", 
                                             surName = "Thomas"),
                       organizationName = "Virginia Tech",
                       electronicMailAddress = "rqthomas@vt.edu"))

#Step 1: Download latest target data and site description data

target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6)

site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-aquatics/master/Aquatic_NEON_Field_Site_Metadata_20210928.csv")

#Step 2: Get drivers

s3_past <- arrow::s3_bucket("neon4cast-drivers/noaa/gefs-v12/stage3/parquet",  
                              endpoint_override =  "data.ecoforecast.org",
                              anonymous=TRUE)
df_past <- arrow::open_dataset(s3_past, partitioning = c("site_id"))


s3_future <- arrow::s3_bucket("neon4cast-drivers/noaa/gefs-v12/stage2/parquet",  
                            endpoint_override =  "data.ecoforecast.org",
                            anonymous=TRUE)
df_future <- arrow::open_dataset(s3_future, partitioning = c("start_date", "cycle"))

sites <- unique(target$siteID)
noaa_past <- df_past |> 
  dplyr::filter(site_id %in% sites,
                variable == "air_temperature") |> 
  dplyr::collect()

noaa_future <- df_future |> 
  dplyr::filter(site_id %in% sites,
                cycle == 0,
                start_date == forecast_date,
                variable == "air_temperature") |> 
  dplyr::collect()

# Step 2.4 Aggregate (to day) and convert units of drivers

noaa_past_mean <- noaa_past %>% 
  mutate(date = as_date(time)) %>% 
  group_by(date) %>% 
  summarize(air_temperature = mean(predicted, na.rm = TRUE), .groups = "drop") %>% 
  rename(time = date) %>% 
  mutate(air_temperature = air_temperature - 273.15)

noaa_future_mean <- noaa_future %>% 
  mutate(date = as_date(time)) %>% 
  group_by(date, ensemble) %>% 
  summarize(air_temperature = mean(predicted, na.rm = TRUE), .groups = "drop") %>% 
  rename(time = date) %>% 
  mutate(air_temperature = air_temperature - 273.15)

#Step 2.5: Merge in past NOAA data into the targets file, matching by date.

target <- left_join(target, noaa_past_mean, by = "time")

#Step 3.0: Generate forecasts for each site

forecast <- NULL

for(i in 1:length(sites)){
  
  # Get site information for elevation
  site_info <- site_data %>% filter(field_site_id == sites[i]) 
  
  #Fit linear model based on past data: water temperature = m * air temperature + b
  fit <- lm(target$temperature~target$air_temperature)
  
  #use linear regression to forecast water temperature for each ensemble member
  forecasted_temperature <- fit$coefficients[1] + fit$coefficients[2] * noaa_future_mean$air_temperature
  
  #use forecasted temperature to predict oyxgen by assuming that oxygen is saturated.  
  forecasted_oxygen <- rMR::Eq.Ox.conc(forecasted_temperature, elevation.m = ,site_info$field_mean_elevation_m, 
                                       bar.press = NULL, 
                                       bar.units = NULL,
                                       out.DO.meas = "mg/L",
                                       salinity = 0, 
                                       salinity.units = "pp.thou")
  
  temperature <- tibble(time = noaa_future_mean$time,
                        site_id = sites[i],
                        ensemble = noaa_future_mean$ensemble,
                        forecast = 1,
                        predicted = forecasted_temperature,
                        variable = "temperature")
  
  oxygen <- tibble(time = noaa_future_mean$time,
                   site_id = sites[i],
                   ensemble = noaa_future_mean$ensemble,
                   forecast = 1,
                   predicted = forecasted_oxygen,
                   variable = "oxygen")
  
  
  #Build site level dataframe.  Note we are not forecasting chla
  forecast <- dplyr::bind_rows(temperature, oxygen)
}

#Visualize forecast.  Is it reasonable?
forecast %>% 
  ggplot(aes(x = time, y = predicted, group = ensemble)) +
  geom_line() +
  facet_grid(variable~site_id, scale ="free")

#Forecast output file name in standards requires for Challenge.  
# csv.gz means that it will be compressed
forecast_file <- paste0("aquatics","-",min(forecast$time),"-",team_name,".csv.gz")

#Write csv to disk
write_csv(forecast, forecast_file)

#Confirm that output file meets standard for Challenge
#neon4cast::forecast_output_validator(forecast_file)

# Step 4: Generate metadata

model_metadata = list(
  forecast = list(
    model_description = list(
      forecast_model_id =  "air2waterSat",  #What goes here
      name = "Air temperatuer to water temperature linear regression plus assume saturated oxygen", 
      type = "empirical",  
      repository = "https://github.com/rqthomas/neon4cast-example" 
    ),
    initial_conditions = list(
      status = "absent"
    ),
    drivers = list(
      status = "propagates",
      complexity = 1, #Just temperature
      propagation = list( 
        type = "ensemble", 
        size = 31) 
    ),
    parameters = list(
      status = "absent"
    ),
    random_effects = list(
      status = "absent"
    ),
    process_error = list(
      status = "absent"
    ),
    obs_error = list(
      status = "absent"
    )
  )
)

metadata_file <- neon4cast::generate_metadata(forecast_file, team_list, model_metadata)

# Step 5: Submit forecast!


neon4cast::submit(forecast_file = forecast_file, metadata = metadata_file, ask = FALSE)
