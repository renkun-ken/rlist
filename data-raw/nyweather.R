library("rlist")

dates <- seq(as.POSIXct("2013-01-01"), by = "day", length.out = 60L)

# need API key
nyweather <- list.load(
  sprintf(
    "https://api.openweathermap.org/data/2.5/history/city?q=New%20York&start=%s",
    as.integer(dates)
  )
)

names(nyweather) <- format(dates, "D%Y%m%d")
devtools::use_data(nyweather)
