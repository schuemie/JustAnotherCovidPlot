papersData <- readRDS("PubMed.rds")
casesData <- readRDS("Cases.rds")

data <- rbind(data.frame(date = papersData$end,
                         count = papersData$count,
                         series = "Papers",
                         type = "Actual"),
              data.frame(date = casesData$end,
                         count = casesData$count,
                         series = "Cases",
                         type = "Actual"))

data$count[data$count < 1] <- 1

predictionCutoff <- Sys.Date() - 90
linearModelPapers <- lm(count ~ date, data = data[data$series == "Papers" & data$date > predictionCutoff, ])
linearModelCases <- lm(count ~ date, data = data[data$series == "Cases" & data$date > predictionCutoff, ])
predictDates <- c(unique(data$date[data$date > predictionCutoff]),
                  seq(Sys.Date(), Sys.Date() + 100, by = 7))

predictedData <- rbind(data.frame(date = predictDates,
                                  count = predict(linearModelPapers, list(date = predictDates)),
                                  series = "Papers",
                                  type = "Predicted"),
                       data.frame(date = predictDates,
                                  count = predict(linearModelCases, list(date = predictDates)),
                                  series = "Cases",
                                  type = "Predicted"))

predictedData$count[predictedData$count < 1] <- 1

library(ggplot2)
library(scales)
library(ggdark)

ggplot(data, aes(x = date, y = count, group = series, color = series)) +
  geom_point(alpha = 0.6) +
  geom_line(linetype = "dashed", alpha = 0.8, data = predictedData) +
  geom_vline(xintercept = Sys.Date(), alpha = 0.8) +
  scale_y_log10("Counts per week", labels = comma) +
  scale_x_date("Date", limits = c(min(data$date), max(predictedData$date))) +
  dark_mode() +
  theme(legend.title = element_blank())

ggsave("Plot.png", width = 7, height = 4)

