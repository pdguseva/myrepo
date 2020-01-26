### Everyday Smartphone Use (test data) ###

setwd("/Users/polinaguseva/Documents/Freie Universität Berlin/FUB Internship/ESU data")
getwd()

library("foreign")
library("car")

data <- read.csv("/Users/polinaguseva/Documents/Freie Universität Berlin/FUB Internship/ESU data/RealmUserEvent.csv",
                 header = TRUE, sep = ",")

## Ispect data ##

View(data)
dim(data)
summary(data)

table(is.na(data$timeStamp))
table(is.na(data$pkg))
table(is.na(data$eventType))
table(is.na(data$pkgName))
table(is.na(data$className))

class(data$timeStamp)
class(data$pkg)
class(data$eventType)
class(data$pkgName)
class(data$className)

table(data$timeStamp)
table(data$pkg)
table(data$eventType)
table(data$pkgName)
table(data$className)

# com.google.android.youtube - 602
# com.sec.android.app.camera - 331
# org.telegram.messenger - 475
# com.spotify.music - 915

## Remove duplicate data ##

library(tidyverse)
options(scipen = 999)

table(duplicated(data$timeStamp))
head(data, 10)

data_all <- data %>% distinct()
head(data_all, 10)
table(duplicated(data_all$timeStamp))
                   
data_time <- data %>% distinct(timeStamp, .keep_all = TRUE)
head(data_time, 10)
table(duplicated(data_time$timeStamp))

## Opening/closing and unlock/lock ##

data_ocul <- data_all[data_all$eventType < 3 | data_all$eventType > 16, ]
View(data_ocul)
table(data_ocul$eventType)

data_ocul_session <- 0
for(i in 2:nrow(data_ocul)) {
  if(data_ocul[i, "pkgName"] != data_ocul[i - 1, "pkgName"]) {
    data_ocul_session <- rbind(data_ocul_session, data_ocul[c(i - 1, i), ])
  }
}

View(data_ocul_session)

data_ocul_ses<- data_ocul_session %>% distinct()
View(data_ocul_ses)





## Opening/closing ##

data_opcl <- data_all[data_all$eventType < 3, ]
View(data_opcl)
table(data_opcl$eventType)

data_opcl_session <- 0
for(i in 2:nrow(data_opcl)) {
  if(data_opcl[i, "pkgName"] != data_opcl[i - 1, "pkgName"]) {
    data_opcl_session <- rbind(data_opcl_session, data_opcl[c(i - 1, i), ])
  }
}
View(data_opcl_session)

## Session duration ##

# data_opcl_session <- rbind(data_opcl_session, data_opcl[c(1), ])
# View(data_opcl_session)
# tail(data_opcl_session)

data_opcl_session <- data_opcl_session[-c(1, 2), ]
View(data_opcl_session)

data_opcl_session$dur <- 0
for(i in 1:nrow(data_opcl_session)) {
  data_opcl_session$dur[i] <- data_opcl_session$timeStamp[i + 1] - data_opcl_session$timeStamp[i]
}

View(data_opcl_session)
tail(data_opcl_session, 10)

data_opcl_duration <- data_opcl_session[data_opcl_session$eventType == 1, ]
View(data_opcl_duration)

data_opcl_duration$sec <- data_opcl_duration$dur/1000
data_opcl_duration$min <- data_opcl_duration$sec/60
data_opcl_duration$hou <- data_opcl_duration$min/60
View(data_opcl_duration)

## Timestamp to date ##

data_opcl_duration$date <- as.POSIXct((as.numeric(as.character(data_opcl_duration$timeStamp)) / 1000.0), origin = '1970-01-01', tz = 'GMT')
data_opcl_duration <- data_opcl_duration %>% separate(date, c("date", "time"), " ", extra = "merge")
data_opcl_duration$datetime <- as.POSIXct((as.numeric(as.character(data_opcl_duration$timeStamp)) / 1000.0), origin = '1970-01-01', tz = 'GMT')
View(data_opcl_duration)
table(data_opcl_duration$date)

## Telegram use ##

data_tg <- data_opcl_duration[data_opcl_duration$pkgName == "org.telegram.messenger", ]
head(data_tg, 10)
View(data_tg)

data_tg_use <- data_tg %>%
  group_by(date) %>%
  summarize(minpd = sum(min, na.rm = TRUE))
View(data_tg_use)

# Dot plot #

dotchart(data_tg_use$minpd,
         labels = row.names(date),
         cex = 0.9,
         main = "Telegram use", 
         xlab = "Duration, min")

data_tg_use %>%
  tail(10) %>%
  ggplot(aes(x = date, y = minpd)) +
  geom_line() +
  geom_point()

library(ggplot2)
library(dplyr)
library(hrbrthemes)
data_tg_use %>%
  tail(20) %>%
  ggplot(aes(x = date, y = minpd)) +
  geom_line(color = "black") +
  geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 9) +
  theme_ipsum() +
  ggtitle("Telegram use, minutes")

# Time series chart #

library(dygraphs)
library(xts)
library(tidyverse)
library(lubridate)

tg_use <- xts(x = data_tg$min, order.by = data_tg$datetime)

plot_tg_use <- dygraph(tg_use) %>%
  dyOptions(labelsUTC = TRUE, fillGraph = TRUE, fillAlpha = 0.1, drawGrid = FALSE, colors = "#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

plot_tg_use

library(htmlwidgets)

saveWidget(plot_tg_use, file = paste0(getwd(), "TelegramUsePlot.html"))

plot_tg_use2 <- dygraph(tg_use) %>%
  dyOptions(stepPlot = TRUE, labelsUTC = TRUE, fillGraph = TRUE, fillAlpha = 0.1, drawGrid = FALSE, colors = "#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

plot_tg_use2
