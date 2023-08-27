install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")
library(dplyr)
library(ggplot2)
library(gridExtra)

# Downloading and uploading data
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
data <- read.table(unz(temp, "household_power_consumption.txt"), sep = ";", header = TRUE)
unlink(temp)

# Adding class type to variables
data <- mutate(data, full_time = paste(data$Date, data$Time))
data$full_time <- as.POSIXct(data$full_time, "%d/%m/%Y %H:%M:%OS", tz = "")
data$Global_active_power <- as.numeric(data$Global_active_power)
data$Global_reactive_power <- as.numeric(data$Global_intensity)
data$Voltage <- as.numeric(data$Voltage)
data$Global_intensity <- as.numeric(data$Global_intensity)
data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)

# Filtering specific dates - 2007-02-01 and 2007-02-02

data <- data[(data$Date == "1/2/2007") | (data$Date == "2/2/2007"),] 
data <- data[!is.na(data$Date),]

hist(data$Global_active_power, xlab = "Global Active Power (kilowatts)",
     ylab = "Frequency", main = "Global Active Power", col = "red")

Sys.setlocale("LC_TIME", "en_US")
data <- mutate(data, weekday = weekdays(full_time))

plot3 <-        ggplot(data = data) + geom_line(mapping = aes(x = full_time , y = Voltage)) +
        theme_bw() +
        scale_x_datetime(name = "Datetime", date_breaks = "1 day", date_labels = "%a") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_y_continuous(name = "Voltage")

png("./figure/lot3.png", width = 480, units = "px")
print(plot3)
dev.off()