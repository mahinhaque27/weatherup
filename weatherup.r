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

input <- function(prompt) {
  cat(prompt, "\n")  
  userInput <- readline()
  return(userInput)
}

main <- function() {
  city <- input("Enter the name of the US city: ")
  state <- input("Enter the name of the State the city is located in: ")
  cat("\n")
  locationData <- getLocationData(city, state)

  latFor <- format(locationData$lat, digits = 7, nsmall = 7)
  lonFor <- format(locationData$lon, digits = 7, nsmall = 7)
  
  cat("The city latitude: ", latFor, "\n")
  cat("The city longitude: ", lonFor, "\n\n")

  weatherData <- getWeatherData(latFor, lonFor)
  currentTemp <- round(weatherData$current$temp)
  currentHumidity <- round(weatherData$current$humidity)
  currentWind <- round(weatherData$current$wind_speed)

  cat("---------------Current Weather--------------------------\n")
  cat("The current temperature at",city,state, " is:",currentTemp, "°F\n")
  cat("The weather can be described as",weatherData$current$weather$main,"and",weatherData$current$weather$description,"\n")
  cat("The current humidity:",currentHumidity,"% \n")
  cat("The current wind speed:",currentWind,"mph \n\n")
  
  # Tomorrows weather data (start at 2 since 1 is todays data)
  day2Low <- round(weatherData$daily$temp$min[[2]])
  day2High <- round(weatherData$daily$temp$max[[2]])
  day2desc <- weatherData$daily$summary[[2]]

  # Weather data from two days from the current day
  day3Low <- round(weatherData$daily$temp$min[[3]])
  day3High <- round(weatherData$daily$temp$max[[3]])
  day3desc <- weatherData$daily$summary[[3]]

  # Weather data from three days from the current day
  day4Low <- round(weatherData$daily$temp$min[[4]])
  day4High <- round(weatherData$daily$temp$max[[4]])
  day4desc <- weatherData$daily$summary[[4]]

  # Weather data from four days from the current day
  day5Low <- round(weatherData$daily$temp$min[[5]])
  day5High <- round(weatherData$daily$temp$max[[5]])
  day5desc <- weatherData$daily$summary[[5]]

  # Weather data from five days from the current day
  day6Low <- round(weatherData$daily$temp$min[[6]])
  day6High <- round(weatherData$daily$temp$max[[6]])
  day6desc <- weatherData$daily$summary[[6]]

  # Weather data from six days from the current day
  day7Low <- round(weatherData$daily$temp$min[[7]])
  day7High <- round(weatherData$daily$temp$max[[7]])
  day7desc <- weatherData$daily$summary[[7]]

  # Weather data from 7 days from the current day
  day8Low <- round(weatherData$daily$temp$min[[8]])
  day8High <- round(weatherData$daily$temp$max[[8]])
  day8desc <- weatherData$daily$summary[[8]]

  cat("---------------7 Day Weather Forecast--------------------------\n")
  cat("Tomorrow: It will be a low of",day2Low,"°F and a high of ",day2High,"°F.",day2desc,"\n")
  cat("2 Days From Now: It will be a low of",day3Low,"°F and a high of ",day3High,"°F.",day3desc,"\n")
  cat("3 Days From Now: It will be a low of",day4Low,"°F and a high of ",day4High,"°F.",day4desc,"\n")
  cat("4 Days From Now: It will be a low of",day5Low,"°F and a high of ",day5High,"°F.",day5desc,"\n")
  cat("5 Days From Now: It will be a low of",day6Low,"°F and a high of ",day6High,"°F.",day6desc,"\n")
  cat("6 Days From Now: It will be a low of",day7Low,"°F and a high of ",day7High,"°F.",day7desc,"\n")
  cat("7 Days From Now: It will be a low of",day8Low,"°F and a high of ",day8High,"°F.",day8desc,"\n\n")
  
  cat("---------------Severe Weather Alerts--------------------------\n")
  cat(weatherData$alerts$description)
  cat("\n")


}

main()
