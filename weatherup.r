library(httr)
library(jsonlite)

API_KEY <- "5a71604f5c27fb3935eab8439f762fce"
BASE_WEATHER <- "https://api.openweathermap.org/data/3.0/onecall"
BASE_GEO <- "http://api.openweathermap.org/geo/1.0/direct"

getWeatherData <- function(latitude, longitude) {
  query_list <- list(lat = latitude, lon = longitude, appid = API_KEY, units = "imperial")
  response <- GET(url = BASE_WEATHER, query = query_list)
  
  if (response$status_code == 200) {
    weather_data <- fromJSON(rawToChar(response$content))
    return(weather_data)
  } 
  else {
    stop("Failed to fetch weather data. Status code: ", response$status_code)
  }
}

getLocationData <- function(city, state) {
  query_list <- list(q = paste(city, state, "US", sep = ","), appid = API_KEY)
  response <- GET(url = BASE_GEO, query = query_list)
  
  if (response$status_code == 200) {
    location_data <- fromJSON(rawToChar(response$content))
    if (length(location_data) == 0) {
      stop("No location data found for the specified city and state.")
    }
    return(location_data)
  } 
  else {
    stop("Failed to fetch location data. Status code: ", response$status_code)
  }
}

main <- function() {
  latitude <- 42.514458
  longitude <- -83.014656
  
  weather_data <- getWeatherData(latitude, longitude)
  location_data <- getLocationData("Detroit", "MI")
  
  cat("The current temperature at the provided coordinates is", weather_data$current$temp, "°F\n")
  cat("The current latitude at the provided location is ", location_data$lat, "\n")
  cat("The current longitude at the provided location is ", location_data$lon)

}

main()
