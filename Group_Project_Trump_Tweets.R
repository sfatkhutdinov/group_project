data <- read.csv('Donald-Tweets!.csv', as.is = F)

summary(data)
str(data)
DataExplorer::create_report(data)
