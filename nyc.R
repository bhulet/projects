#This analysis looked at the NYC "march" taxi cab data which can be found
#here : http://chriswhong.com/open-data/foil_nyc_taxi/
#
# All data for a given transaction was removed from analysis when:
# There were missing data points
# When Average Speed of taxi < 0 or Average Speed > 60
# When time == 0
# When ratio of (GPS 'striaght line distance' / Trip Distance) exceeded median +- (3*(Interquartile Range))
# When March Revenue exceeded median +- (3*(Interquartile Range))
# "NOC" or "No Charge" payments were removed for analysis of payments
#
#This prints values for specific aspects of Average_speed, payments, total revuene by driver, fare per mile,
#fare per minute, etc.
#
#program for finding distance using latitude and longitude is included at the beginning

nyc <- function(){
    earth.dist <- function (long1, lat1, long2, lat2){
        rad <- pi/180
	a1 <- lat1 * rad
	a2 <- long1 * rad
	b1 <- lat2 * rad
	b2 <- long2 * rad
	dlon <- b2 - a2
	dlat <- b1 - a1 
	a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
	c <- 2 * atan2(sqrt(a), sqrt(1-a))
	R <- 3963
        d <- R * c
	return(d)
	}
    set1 <- read.csv("trip_fare_3.csv")
    set1 <- set1[ ,c(5,6,7,8,9,10,11)]
    set2 <- read.csv("trip_data_3.csv")
    set2 <- set2[,c(2,6,7,9,10,11,12,13,14)]
    data <- data.frame()
    data <- cbind(set1, set2)
    rm("set1")
    rm("set2")
    data$pickup_longitude[which(data$pickup_longitude == 0)] = NA
    data$pickup_latitude[which(data$pickup_latitude == 0)] = NA
    data$dropoff_longitude[which(data$dropoff_longitude == 0)] = NA
    data$dropoff_latitude[which(data$dropoff_latitude == 0)] = NA
    dis <- data.frame()
    dis <- earth.dist(long1 = data$pickup_longitude, lat1 = data$pickup_latitude, long2 = data$dropoff_longitude, lat2 = data$dropoff_latitude)
    dis[which(dis > 100)] = NA
    data <- cbind(data, dis)
    rm("dis")
    minutes <- as.numeric(unlist(data$trip_time_in_secs))
    minutes <- minutes/60
    minutes[which(minutes == 0)] = NA
    data <- cbind(data, minutes)
    rm("minutes")   
    speed <- as.numeric(unlist((data$trip_distance + .0001 ) / (data$minutes / 60 )))
    speed[which(speed > 60 | speed < 0)] = NA
    data <- cbind(data, speed)
    rm("speed")
    ratio <- (data$dis +.0001) / (data$trip_distance + .0001)
    out <- median(ratio, na.rm = T) + ((quantile(ratio, p=.75, na.rm = T) - quantile(ratio, p=.25, na.rm = T)) * 3)
    ratio[which(ratio > out)] = NA
    data <- cbind(data, ratio)
    rm("ratio", "out")
    data <- subset(data, complete.cases(data))
    smaller <- subset(data[,c(1,7)], data$total_amount < 5)
    ans <- (nrow(subset(smaller, payment_type == "CRD"))) / (nrow(subset(smaller, payment_type != "NOC")))
    message("The fraction of payments under $5 which use a credit card is: ", round(ans, digits = 3))
    smaller <- subset(data[,c(1,7)], data$total_amount > 50)
    ans <- (nrow(subset(smaller, payment_type == "CRD"))) / (nrow(subset(smaller, payment_type != "NOC")))
    message("The fraction of payments over $50 which use a credit card is: ", round(ans, digits = 3))
    rm("smaller")
    rm("ans")
    ans <- mean(data$fare_amount / data$minutes)
    message("The mean fare per minute driven is: ", round(ans, digits = 2))
    rm("ans")
    ans <- median(as.numeric(unlist(data$fare)) / (as.numeric(unlist(data$trip_distance))))
    message("The median of the taxi's fare per mile driven is: ", round(ans, digits = 2))
    rm("ans")
    ans <- quantile(subset(data$speed, data$speed >= 1),p=.95)
    message("The 95th percentile of the taxi's average driving speed in miles per hour is: ", round(ans, digits = 0))
    rm("ans")
    ans <- mean(data$ratio)
    message("The average ratio of the distance between the pickup and dropoff divided by the distance driven is: ", round(ans, digits = 3))
    rm("ans")
    jfk_long <- -73.7789
    jfk_lat <- 40.6397
    jfk_distance <- earth.dist(long1 = jfk_long, lat1 = jfk_lat, long2= data$pickup_longitude, lat2 = data$pickup_latitude)
    data <- data[,c(1,2,3,4,5,6,7,8,10,11,12,17,18,19,20)]
    ans <- mean(subset(data$tip_amount, jfk_distance < 1 ))
    message("The average tip for rides from JFK is : ", round(ans, digits = 2))
    rm("jfk_distance", "earth.dist", "jfk_lat", "jfk_long", "ans")
    data$dropoff_datetime <- months(as.POSIXct(data$dropoff_datetime))
    by_hack <- subset(data[, c("hack_license" , "total_amount")], data$dropoff_datetime == "March")
    by <- tapply(by_hack$total_amount, by_hack$hack_license, sum)
    y <-numeric(nrow(by))
    for (i in 1:nrow(by)){
	y[i] <- by[[i]]
	}
    out <- median(y, na.rm = T) + ((quantile(y, p=.75, na.rm = T) - quantile(y, p=.25, na.rm = T)) * 3)
    y[which(y > out)] = NA
    ans <- median(y, na.rm = T)
    message("The median March revenue of a taxi driver is: ", round(ans, digits = 4))
    rm("ans")
}
