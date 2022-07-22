renv::activate()
renv::restore()

library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)

Sys.setenv("AWS_DEFAULT_REGION" = "data",
           "AWS_S3_ENDPOINT" = "ecoforecast.org")

dir.create("drivers", showWarnings = FALSE)

forecast_date <- Sys.Date()

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

#Step 2.1: Download Paste NOAA forecast stacked together

sites <- unique(target$siteID)
for(i in 1:length(sites)){
  neon4cast::get_stacked_noaa_s3(".",site = sites[i], averaged = FALSE)
}

#Step 2.2: Download NOAA future forecast

sites <- unique(target$siteID)
for(i in 1:length(sites)){
  neon4cast::get_noaa_forecast_s3(".",model = "NOAAGEFS_1hr",site = sites[i],date = forecast_date,cycle = "00")
}

#Step 2.3 Create data frames of drivers

noaa_past <- neon4cast::stack_noaa(dir = "drivers", model = "NOAAGEFS_1hr_stacked")
noaa_future <- neon4cast::stack_noaa(dir = "drivers", model = "NOAAGEFS_1hr", forecast_date = forecast_date)

# Step 2.4 Aggregate (to day) and convert units of drivers

noaa_past_mean <- noaa_past %>% 
  mutate(date = as_date(time)) %>% 
  group_by(date) %>% 
  summarize(air_temperature = mean(air_temperature, na.rm = TRUE), .groups = "drop") %>% 
  rename(time = date) %>% 
  mutate(air_temperature = air_temperature - 273.15)

noaa_future_mean <- noaa_future %>% 
  mutate(date = as_date(time)) %>% 
  group_by(date, ensemble) %>% 
  summarize(air_temperature = mean(air_temperature, na.rm = TRUE), .groups = "drop") %>% 
  rename(time = date) %>% 
  mutate(ensemble = as.numeric(stringr::str_sub(ensemble, start = 4, end = 6)),
         air_temperature = air_temperature - 273.15)

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
  
  #Build site level dataframe.  Note we are not forecasting chla
  site_forecast <- tibble(time = noaa_future_mean$time,
                          siteID = sites[i],
                          ensemble = noaa_future_mean$ensemble,
                          forecast = 1,
                          temperature = forecasted_temperature,
                          oxygen = forecasted_oxygen,
                          chla = NA)
  #Bind with other sites
  forecast <- bind_rows(forecast, site_forecast)
}

#Visualize forecast.  Is it reasonable?
forecast %>% 
  select(-chla) %>% 
  pivot_longer(cols = c("temperature","oxygen"), names_to = "variable", values_to = "values") %>% 
  ggplot(aes(x = time, y = values, group = ensemble)) +
  geom_line() +
  facet_grid(variable~siteID, scale ="free")

#Forecast output file name in standards requires for Challenge.  
# csv.gz means that it will be compressed
forecast_file <- paste0("aquatics","-",min(forecast$time),"-",team_name,".csv.gz")

#Write csv to disk
write_csv(forecast, forecast_file)

#Confirm that output file meets standard for Challenge
neon4cast::forecast_output_validator(forecast_file)


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
