library(httr)
library(jsonlite)

get_weather_data <- function(latitude, longitude) {
  base_url <- "https://api.openweathermap.org/data/3.0/onecall"
  query_list <- list(lat = latitude, lon = longitude, appid = "5a71604f5c27fb3935eab8439f762fce", units = "imperial")
  
  response <- GET(url = base_url, query = query_list)
  
  if (response$status_code == 200) {
    weather_data <- fromJSON(rawToChar(response$content))
    return(weather_data)
  } 
  else {
    stop("Failed to fetch weather data. Status code: ", response$status_code)
  }
}

get_location_data <- function(city, state) {
  base_url <- "http://api.openweathermap.org/geo/1.0/direct"
  query_list <- list(q = paste(city, state, "US", sep = ","), appid = "5a71604f5c27fb3935eab8439f762fce")
  
  response <- GET(url = base_url, query = query_list)
  
  if (response$status_code == 200) {
    location_data <- fromJSON(rawToChar(response$content))
    if (length(location_data) == 0) {
      stop("No location data found for the specified city and state.")
    }
    return(location_data)
  } else {
    stop("Failed to fetch location data. Status code: ", response$status_code)
  }
}

main <- function() {
  latitude <- 42.514458
  longitude <- -83.014656
  
  weather_data <- get_weather_data(latitude, longitude)
  location_data <- get_location_data("warren", "MI")
  
  cat("The current temperature at the provided coordinates is", weather_data$current$temp, "°F\n")
  cat("The current latitude at the provided location is ", location_data$lat, "\n")
  cat("The current latitude at the provided location is ", location_data$lon)

}

main()
