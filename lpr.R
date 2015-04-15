#Data is from, https://github.com/johnschrom/Minneapolis-ALPR-Data
#A repository containing data from Minneapolis' automatic license plate Recognition program
#This function shows initial investigation
#The percent of total records by device
#The percent of total license plates captured for a given hour of the day

lpr <- function(){
    data <- read.csv("data.csv")
    names(data) <- c("plate", "lat", "long", "date_time", "capture_id")
    data$long[which(data$long == 0)] = NA
    data$lat[which(data$lat == 0)] = NA
    data <- subset(data, complete.cases(data))
    data$date_time <- strptime(unlist(data$date_time), format = "%m/%d/%Y %H:%M")
    lev <- levels(data$capture_id)
    capture_names <- lev[2:9]
    capture <- numeric(length(capture_names))
    for (i in 1:length(capture)){
	capture[i] <- nrow(subset(data, capture_id == capture_names[i])) / nrow(data) *100
        }
    names(capture) <- capture_names
    message("There were ", length(capture), " plate recognition devices used in this dataset")
    message("The percent of total records by device is :")
    print(capture)
    time_of_capture <- numeric(24)
    names_time_of_capture <- c("1_am", "2_am", "3_am", "4_am", "5_am", "6_am", "7_am", 
	"8_am", "9_am", "10_am", "11_am", "noon", "1_pm", "2_pm", "3_pm", "4_pm", "5_pm",
	"6_pm", "7_pm", "8_pm", "9_pm", "10_pm", "11_pm", "midnight")
    for (i in 1:length(time_of_capture)){
	time_of_capture[i] <- round(nrow(subset(data, format(data$date_time, '%H') %in% i)) / nrow(data) * 100, digits = 1)
	}
    names(time_of_capture) <- names_time_of_capture
    message("The percent of total license plates captured for a given hour of the day, are :")
    print(time_of_capture)                   
}
