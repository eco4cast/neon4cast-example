library(tidyverse)
library(neon4cast)
library(lubridate)
install.packages("rMR")
library(rMR)

dir.create("drivers", showWarnings = FALSE)

forecast_date <- Sys.Date()
noaa_date <- Sys.Date() - days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet

#Step 0: Define team name and team members 

model_id <- "air2waterSat"

team_list <- list(list(individualName = list(givenName = "Quinn", 
                                             surName = "Thomas"),
                       organizationName = "Virginia Tech",
                       electronicMailAddress = "rqthomas@vt.edu"))

#Step 1: Download latest target data and site description data

target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6)

site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(aquatics == 1)

#Step 2: Get drivers

df_past <- neon4cast::noaa_stage3()

df_future <- neon4cast::noaa_stage2()

sites <- unique(target$site_id)

#Step 3.0: Generate forecasts for each site

forecast <- NULL

for(i in 1:length(sites)){
  
  # Get site information for elevation
  site_info <- site_data %>% dplyr::filter(field_site_id == sites[i]) 
  
  noaa_past <- df_past |> 
    dplyr::filter(site_id == sites[i],
                  variable == "air_temperature") |> 
    dplyr::select(time, predicted, ensemble) |>
    dplyr::collect()
  
  noaa_future <- df_future |> 
    dplyr::filter(cycle == 0,
                  site_id == sites[i],
                  start_date == as.character(noaa_date),
                  time >= lubridate::as_datetime(forecast_date), 
                  variable == "air_temperature") |> 
    dplyr::select(time, predicted, ensemble) |>
    dplyr::collect()
  
  # Aggregate (to day) and convert units of drivers
  
  noaa_past_mean <- noaa_past %>% 
    mutate(date = as_date(time)) %>% 
    group_by(date) %>% 
    summarize(air_temperature = mean(predicted, na.rm = TRUE), .groups = "drop") %>% 
    rename(datetime = date) %>% 
    mutate(air_temperature = air_temperature - 273.15)
  
  noaa_future_site <- noaa_future %>% 
    mutate(datetime = as_date(time)) %>% 
    group_by(datetime, ensemble) |> 
    summarize(air_temperature = mean(predicted), .groups = "drop") |> 
    mutate(air_temperature = air_temperature - 273.15) |> 
    select(datetime, air_temperature, ensemble)
  
  #Merge in past NOAA data into the targets file, matching by date.
  site_target <- target |> 
    select(datetime, site_id, variable, observed) |> 
    dplyr::filter(variable %in% c("temperature", "oxygen"),
           site_id == sites[i]) |> 
    pivot_wider(names_from = "variable", values_from = "observed") |>
    left_join(noaa_past_mean, by = c("datetime"))
  
  #Check that temperature and oxygen are avialable at site
  if("temperature" %in% names(site_target) & "oxygen" %in% names(site_target)){
    
    if(length(which(!is.na(site_target$air_temperature) & !is.na(site_target$temperature))) > 0){
      
      #Fit linear model based on past data: water temperature = m * air temperature + b
      fit <- lm(site_target$temperature~site_target$air_temperature)
      
      #use linear regression to forecast water temperature for each ensemble member
      forecasted_temperature <- fit$coefficients[1] + fit$coefficients[2] * noaa_future_site$air_temperature
      
      #use forecasted temperature to predict oyxgen by assuming that oxygen is saturated.  
      forecasted_oxygen <- rMR::Eq.Ox.conc(forecasted_temperature, elevation.m = ,site_info$field_mean_elevation_m, 
                                           bar.press = NULL, 
                                           bar.units = NULL,
                                           out.DO.meas = "mg/L",
                                           salinity = 0, 
                                           salinity.units = "pp.thou")
      
      temperature <- tibble(datetime = noaa_future_site$datetime,
                            site_id = sites[i],
                            ensemble = noaa_future_site$ensemble,
                            predicted = forecasted_temperature,
                            variable = "temperature")
      
      oxygen <- tibble(datetime = noaa_future_site$datetime,
                       site_id = sites[i],
                       ensemble = noaa_future_site$ensemble,
                       predicted = forecasted_oxygen,
                       variable = "oxygen")
      
      
      #Build site level dataframe.  Note we are not forecasting chla
      forecast <- dplyr::bind_rows(forecast, temperature, oxygen)
    }
  }
}

forecast <- forecast |> 
  mutate(reference_datetime = forecast_date,
         family = "ensemble") |> 
  rename(parameter = ensemble) |> 
  select(datetime, reference_datetime, site_id, family, parameter, variable, predicted)

#Visualize forecast.  Is it reasonable?
#forecast %>% 
#  ggplot(aes(x = time, y = predicted, group = ensemble)) +
#  geom_line() +
#  facet_grid(variable~site_id, scale ="free")

#Forecast output file name in standards requires for Challenge.  
# csv.gz means that it will be compressed
forecast_file <- paste0("aquatics","-",min(forecast$datetime),"-",model_id,".csv.gz")

#Write csv to disk
write_csv(forecast, forecast_file)

# Step 4: Submit forecast!

neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)
