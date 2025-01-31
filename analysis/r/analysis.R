library(dplyr)

df <- read.csv("../../data/data.csv")

df <- df[, c("DataValue", "LocationAbbr", "Topic", "YearEnd", "YearStart")]

df <- df[df$YearEnd %in% c(2016) & df$YearStart %in% c(2016), ]

df$DataValue <- as.numeric(df$DataValue)

df <- na.omit(df)

df <- df %>%
  group_by(LocationAbbr) %>%
  mutate(Percent = DataValue / sum(DataValue)) %>%
  ungroup()

write.csv(df, "../../data/csv/r.csv", row.names = FALSE)

write.csv(df, "../../visualizations/shiny/r.csv", row.names = FALSE)
