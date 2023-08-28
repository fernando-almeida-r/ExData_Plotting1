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

plot1 <-        ggplot(data = data) + geom_line(mapping = aes(x = full_time , y = Global_active_power)) +
        theme_bw() +
        scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
        theme(axis.title.x.bottom = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_y_continuous(name = "Global Active Power (kilowatts)")
png("./figure/plot1.png", width = 480, units = "px")
print(plot1)
dev.off()

plot2 <-        ggplot(data = data) + 
        geom_line(mapping = aes(x = full_time , 
                                y = Sub_metering_3, colour = "Sub_metering_3")) +
        geom_line(mapping = aes(x = full_time , 
                                y = Sub_metering_2, colour = "Sub_metering_2")) +
        geom_line(mapping = aes(x = full_time , 
                                y = Sub_metering_1, colour = "Sub_metering_1")) +
        theme_bw() +
        scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
        scale_color_manual(values = c("Sub_metering_3" = "blue",
                                      "Sub_metering_2" = "red",
                                      "Sub_metering_1" = "black"))+
        theme(axis.title.x.bottom = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.background = element_rect(fill = "white", color = "black"),
              legend.title = element_blank(),
              legend.position = c(1, 1),
              legend.justification = c("right", "top"),
              legend.box.just = "right") +
        scale_y_continuous(name = "Energy sub metering")
png("./figure/plot2.png", width = 480, units = "px")
print(plot2)
dev.off()

plot3 <-        ggplot(data = data) + geom_line(mapping = aes(x = full_time , y = Voltage)) +
        theme_bw() +
        scale_x_datetime(name = "Datetime", date_breaks = "1 day", date_labels = "%a") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_y_continuous(name = "Voltage")

png("./figure/plot3.png", width = 480, units = "px")
print(plot3)
dev.off()

plot4 <-        ggplot(data = data) + geom_line(mapping = aes(x = full_time , y = Global_reactive_power)) +
        theme_bw() +
        scale_x_datetime(name = "datetime", date_breaks = "1 day", date_labels = "%a") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_y_continuous(name = "Global_reactive_power")
png("./figure/plot4.png", width = 480, units = "px")
grid.arrange(plot1, plot3, plot2, plot4, nrow = 2)
dev.off()