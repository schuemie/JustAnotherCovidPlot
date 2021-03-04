url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
download.file(url, "time_series_covid19_confirmed_global.csv")

data <- read.csv("time_series_covid19_confirmed_global.csv")
data <- data[, -(1:4)]

data <- data.frame(date = as.Date(gsub("X", "", colnames(data)), "%m.%d.%y"),
                   cumCount = apply(data, 2, sum))
data$count <- c(0, data$cumCount[-1] - data$cumCount[-nrow(data)])


startDate <- as.Date("2020-01-01")
endDate <- Sys.Date()


cutPoints <- seq(startDate, endDate, by = 7)
weekData <- data.frame(start = cutPoints[-length(cutPoints)],
                   end = cutPoints[-1] - 1,
                   count = 0)


for (i in 1:nrow(weekData)) {
  start <- weekData$start[i]
  end <- weekData$end[i]
  weekData$count[i] <- sum(data$count[data$date >= start & data$date <= end])
}

saveRDS(weekData, "Cases.rds")
