# ----------------------------------------------------------------------------------------------------
# Coursera "Exploratory Data Analysis"
# Course Project 1
# author:   Marc Sol
# created:  Feb 5, 2015
# 
# This R program creates a plot as defined in the assignment for Course Project 1
# The code assumes that the data file is available already in the current working directory.
# The resulting plot png files will also be produced in the current working directory.
# ----------------------------------------------------------------------------------------------------

library(data.table)

fileName <- "household_power_consumption.txt"

# ----------------------------------------------------------------------------------------------------
# Data read and preparation
# ----------------------------------------------------------------------------------------------------
# The data in this file has 9 columns, separated by ; and using ? for missing values: 
# Date: Date in format dd/mm/yyyy
# Time: time in format hh:mm:ss
# Global_active_power: household global minute-averaged active power (in kilowatt)
# Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
# Voltage: minute-averaged voltage (in volt)
# Global_intensity: household global minute-averaged current intensity (in ampere)
# Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
# Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
# Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

all_data <- fread(fileName, sep=";", na.strings="?", colClasses=c( rep("character",9)))

# rename the Date and Time columns to highlight that they are in fact strings
setnames(all_data, "Date", "DateStr")
setnames(all_data, "Time", "TimeStr")

# add a column with the DateStr field converted to a real Date 
all_data[, Date:= as.Date(DateStr, format="%d/%m/%Y")]

# We will only be using data from the dates 2007-02-01 and 2007-02-02.
data <- all_data[ Date >= "2007-02-01" & Date <= "2007-02-02"]

# We have now obtained the data set of interest in a data.table object called "data"

# create a new column with Date/Time objects representing the concatenated DateStr and TimeStr columns
data[, DateTimeStr := paste( DateStr, TimeStr, sep=":")]

# Convert the numeric columns that we use.
# I tried this during the fread, but that failed due to some incompatibility with the ? missing values. I don't understand that issue really.
# Anyway, since data is only the small dataset of interest, the following conversions are fast
data$Global_active_power <- as.numeric( data$Global_active_power )
data$Sub_metering_1 <- as.numeric( data$Sub_metering_1 )
data$Sub_metering_2 <- as.numeric( data$Sub_metering_2 )
data$Sub_metering_3 <- as.numeric( data$Sub_metering_3 )
data$Voltage <- as.numeric( data$Voltage )
data$Global_reactive_power <- as.numeric( data$Global_reactive_power )

# unfortunately, here I need to move to data.frame in order to convert the date/times
# I would be very interested to learn a way to do this conversion while staying in the "data.table world"  
df_data <-as.data.frame.matrix(data) 
df_data$DateTime <- strptime(df_data[, 11], format="%d/%m/%Y:%H:%M:%S")

# The code above this line is identical for all 4 plot.R files. 

# ----------------------------------------------------------------------------------------------------
# Generating the plot
# ----------------------------------------------------------------------------------------------------

# plot4
png("plot4.png") # 480x480 pixels is default
par( mfrow=c(2,2))
# topleft
plot(df_data$DateTime, df_data$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
# topright
plot(df_data$DateTime, df_data$Voltage, type="l", xlab="datetime", ylab="Voltage")
# bottomleft
plot(df_data$DateTime, df_data$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")
points(df_data$DateTime, df_data$Sub_metering_1, type="l", col="black")
points(df_data$DateTime, df_data$Sub_metering_2, type="l", col="red")
points(df_data$DateTime, df_data$Sub_metering_3, type="l", col="blue")
legend("topright", bty="n", lty=c(1,1,1), col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
# bottomright
plot(df_data$DateTime, df_data$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
dev.off()

# ----------------------------------------------------------------------------------------------------
# End
# ----------------------------------------------------------------------------------------------------
