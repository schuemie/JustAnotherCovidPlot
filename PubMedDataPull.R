library(easyPubMed)

query <- "covid"
startDate <- as.Date("2020-01-01")
endDate <- Sys.Date()


cutPoints <- seq(startDate, endDate, by = 7)
data <- data.frame(start = cutPoints[-length(cutPoints)],
                   end = cutPoints[-1] - 1,
                   count = 0)

for (i in 1:nrow(data)) {
  start <- data$start[i]
  end <- data$end[i]
  
  # Start of year gets all papers with no specific date:
  if (format(start, "%m/%d") == "01/01") {
    start <- start + 1
  }
  periodQuery <- sprintf("%s AND (%s:%s[epdat])", query, format(start, "%Y/%m/%d"), format(end, "%Y/%m/%d"))
  message(sprintf("Running query '%s'", periodQuery))
  my_entrez_id <- get_pubmed_ids(periodQuery)
  count <- as.numeric(my_entrez_id$Count)
  days <- as.integer(end - start + 1)
  if (days != 7) {
    count <- round(count * 7 / days)
  }
  data$count[i] <- count
}

saveRDS(data, "PubMed.rds")
plot(data$count)
