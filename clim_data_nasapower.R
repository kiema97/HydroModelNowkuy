setwd("xxxxxxx")
Sys.setenv(TZ = "UTC")

library(nasapower)
library(zoo)

cli_params <- c("PRECTOTCORR", "T2M_MIN", "T2M_MAX", "WS2M")

start_date <- "1981-01-01"
end_date <- "2017-12-31"

SLEEP_TIME_BETWEEN_REQUESTS <- 10

stations_lon <- c(-2.1, 0.8, 0.9)           #definir les longitudes des stations
stations_lat <- c(11.5, 11.6, 11.7)         #definir les latitudes des stations
stations_names <- c("a","c","c")            #definir les noms des stations

stations <- data.frame(stations_names, stations_lon, stations_lat)
colnames(stations) <- c("stations", "lon", "lat")

df.out <- data.frame(matrix(nrow = 0, ncol = length(cli_params)+1))
df.names <- c("pr","tn", "tx", "ws")

for (i in 1:length(stations) {
  i <- 1
  s <- stations[i,"stations"]
  print(paste("Processing:", s))
  cli_df <- get_power(
    community = "ag",
    lonlat = c(stations[i,"lon"], stations[i,"lat"]),
    pars = cli_params,
    dates = c(start_date, end_date),
    temporal_api = "daily")
  
  df <- data.frame(cli_df[,7:ncol(cli_df)])
  colnames(df)[1] <- "Date"
  
  for (col in colnames(df)) {
    if (col != "Date") {
      if (is.na(df[1, col])) df[1, col] <- mean(df[,col], na.rm=T)
      if (is.na(df[nrow(df), col])) df[nrow(df), col] <- mean(df[,col], na.rm=T)
      df[,col] <- na.approx(df[,col], na.rm = T)
    }
  }
  
  df.out[nrow(df.out)+1,] <- stats
  
  Sys.sleep(SLEEP_TIME_BETWEEN_REQUESTS)
}

rownames(df.out) <- countries$Code_ISO
colnames(df.out) <- df.names

write.csv(df.out, file = "data_completion/climate_nasapower.csv", row.names = T)

print("finished")
