power <-read.table("household_power_consumption.txt", sep=";", header=TRUE)
attach(power)
names(power)
power$Date <- as.Date(power$Date, "%d/%m/%Y")

#select days one by one and join (something  I do not understand with date and time)
power1 <-subset(power, power$Date=="2007-02-01")
power2 <-subset(power, power$Date=="2007-02-02")
attach(power2)
attach(power1)
power3<-rbind(power1, power2)

#assign meaning to each factor
power3$Date <- as.Date(power3$Date, "%Y-%m-%d")
power3$Time <- strptime (power3$Time, "%H:%M:%S")
power3$tiempo<-strftime(power3$Time, format="%H:%M:%S")
power3$tiempo <- as.POSIXct(paste(power3$Date, power3$tiempo), format="%Y-%m-%d %H:%M:%S")

power3$Global_active_power <-as.numeric(power3$Global_active_power)
power3$Global_reactive_power <-as.numeric(power3$Global_reactive_power)
power3$Voltage <-as.numeric(power3$Voltage)
power3$Global_intensity <-as.numeric(power3$Global_intensity)
power3$Sub_metering_1 <-as.numeric(power3$Sub_metering_1)
power3$Sub_metering_2 <-as.numeric(power3$Sub_metering_2)
power3$Sub_metering_3 <-as.numeric(power3$Sub_metering_3)


names(power3)
attach(power3)

#Draw figures

#First figure
hist(power3$Global_active_power, main="Global active power", xlab="Global active power (kilowatts)",col="red")
dev.copy(png, file="globalhist.png")
dev.off()


#Second figure
plot(power3$tiempo, power3$Global_active_power, ann=FALSE,type="n" )
lines(power3$Global_active_power~power3$tiempo,lwd=1)
title(ylab="Global active power (kilowatts)")
axis(side = 2, lwd = 2)
dev.copy(png, file="globaltime.png")
dev.off()

#Third figure
plot(power3$tiempo, power3$Global_reactive_power, ann=FALSE,type="n" )
lines(power3$Global_reactive_power~power3$tiempo,lwd=1)
title(ylab="Global_reactive_power")
axis(side = 2, lwd = 2)
dev.copy(png, file="globalreactime.png")
dev.off()

#Fourth figure
plot(power3$tiempo, power3$Voltage, ann=FALSE,type="n" )
lines(power3$Voltage~power3$tiempo,lwd=1)
title(ylab="Voltage")
axis(side = 2, lwd = 2)
dev.copy(png, file="Voltage.png")
dev.off()


#Fifth figure
plot(power3$tiempo, power3$Sub_metering_1, ann=FALSE,type="n", ylim=c(0, 35) )
lines(power3$Sub_metering_1~power3$tiempo,lwd=0.5)
lines(power3$Sub_metering_2~power3$tiempo,lwd=0.5, col="red")
lines(power3$Sub_metering_3~power3$tiempo,lwd=0.5, col="blue")
legend("topright", pch = "_", col = c("black","blue", "red"), legend = c("Submetering 1", "Submetering 3", "Submetering 3"))
axis(side = 2, lwd = 2)
dev.copy(png, file="globaltime.png")
dev.off()



# Pannel
par(mfrow = c(2, 2))

plot(power3$tiempo, power3$Global_active_power, ann=FALSE,type="n" )
lines(power3$Global_active_power~power3$tiempo,lwd=1)
title(ylab="Global active power (kilowatts)")
axis(side = 2, lwd = 2)

plot(power3$tiempo, power3$Voltage, ann=FALSE,type="n" )
lines(power3$Voltage~power3$tiempo,lwd=1)
title(xlab="datetime", ylab="Voltage")

plot(power3$tiempo, power3$Sub_metering_1, ann=FALSE,type="n", ylim=c(0, 35) )
lines(power3$Sub_metering_1~power3$tiempo,lwd=0.5)
lines(power3$Sub_metering_2~power3$tiempo,lwd=0.5, col="red")
lines(power3$Sub_metering_3~power3$tiempo,lwd=0.5, col="blue")
title(ylab="Submetering")
legend("topright", pch = "_", col = c("black","blue", "red"), legend = c("Submetering 1", "Submetering 3", "Submetering 3"))
axis(side = 2, lwd = 2)


plot(power3$tiempo, power3$Global_reactive_power, ann=FALSE,type="n" )
lines(power3$Global_reactive_power~power3$tiempo,lwd=1)
title(xlab="datetime", ylab="Global_reactive_power")
axis(side = 2, lwd = 2)
dev.copy(png, file="globalreactime.png")
dev.off()