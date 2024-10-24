# Data pre processing
# Data Pre-processing

#read the external file
#assign the file into a variable named 'confirmed_cases'
global_data <- read.csv("WHO-COVID-19-global-data.csv",header=TRUE,sep=",")

#select country code 'Malaysia'
keyword <- "Thailand"

#select rows that contain the specific keyword
Thailand <- global_data[global_data$Country == keyword, ]

#check for missing values
#specify columns 5 to 8 to check for missing values
if (any(is.na(Thailand[, 5:8]))) {
  print("Selected rows contain missing values.")
} else {
  print("Selected rows do not contain missing values")
}

#There are no missing values in the number of cases

#Find the date of the first case
# 1. remove the records with '0'
cases <- Thailand[Thailand$New_cases != 0, ]

# 2. display the first row of the edited record
#this is the date of the first confirmed case
#first_line <- head(cases$Date_reported, n=1)

# find the number of row of the date in the Malaysia data frame
first_date <- which(Thailand == "2020-01-26", arr.ind=TRUE)[1,1]

# find the number of row of the last date in the Malaysia data frame
last_date <- which(Thailand == "2022-12-31", arr.ind=TRUE)[1,1]

# create a new data frame from first confirmed date until 31st dec 2022
confirmed_cases <- Thailand[first_date:last_date, ]

confirmed_cases <- confirmed_cases[,-2:-4]

confirmed_cases$New_cases <- ifelse(confirmed_cases$New_cases < 0, 0, confirmed_cases$New_cases)
confirmed_cases$New_deaths <- ifelse(confirmed_cases$New_deaths < 0, 0, confirmed_cases$New_deaths)
# write to csv
write.csv(confirmed_cases, "/Users/chairunnisanuramanda/RAMNDA/covid_thailand.csv", row.names=TRUE)
