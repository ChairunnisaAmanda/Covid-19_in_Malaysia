#################################################################
# Start from here
# install required packages
install.packages("ggplot2")
install.packages("lubridate")
install.packages("scales")

# Read csv file

covid_malaysia <- read.csv("covid_malaysia.csv",header=TRUE,sep=",")
covid_malaysia <- covid_malaysia[,-1]

# Convert the date column to Date format
library(lubridate)

covid_malaysia$Date_reported <- as.Date(covid_malaysia$Date_reported)


# Plot the graph using ggplot
library(ggplot2)
library(scales)

# new cases
plot_new_cases = ggplot(covid_malaysia, aes(x = Date_reported, y = New_cases)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Confirmed COVID-19 Cases per Day in Malaysia until 31 December 2022")+
  theme_classic()+
  scale_y_continuous(breaks=seq(0,40000,5000))+
  scale_x_date(labels = date_format("%b"), date_breaks = "3 month") +
  labs(x = "Month (Mar 2020 - Dec 2022)", y = "Number of Cases")
plot_new_cases

# new death
plot_new_deaths = ggplot(covid_malaysia, aes(x = Date_reported, y = New_deaths)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "COVID-19 Deaths per Day in Malaysia until 31 December 2022")+ 
         theme_classic()+
  scale_y_continuous(breaks=seq(0,600,100))+
  scale_x_date(labels = date_format("%b"), date_breaks = "3 month") +
  labs(x = "Month (Mar 2020 - Dec 2022)", y = "Number of Deaths")
plot_new_deaths

# cumulative cases
plot_cum_cases = ggplot(covid_malaysia, aes(x = Date_reported, y = Cumulative_cases)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Confirmed COVID-19 Cases in Malaysia until 31 December 2022")+ theme_classic()+
  coord_cartesian(ylim = c(0, 6000000)) +
  scale_y_continuous(labels = function(x) paste0(x / 1e6), breaks=seq(0,6000000,1000000))+ 
  scale_x_date(labels = date_format("%b"), date_breaks = "3 month") +
  labs(x = "Month (Mar 2020 - Dec 2022)", y = "Number of Cases (in million)")
plot_cum_cases

# cumulative death
plot_cum_deaths = ggplot(covid_malaysia, aes(x = Date_reported, y = Cumulative_deaths)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Total COVID-19 Deaths in Malaysia until 31 December 2022") + 
  theme_classic() +
  coord_cartesian(ylim = c(0,40000))+
  scale_y_continuous(breaks=seq(0,40000,10000))+
  scale_x_date(labels = date_format("%b"), date_breaks = "3 month") +
  labs(x = "Month (Mar 2020 - Dec 2022)", y = "Number of Deaths")
plot_cum_deaths


###################
# MCO Phases and plot of new daily cases

plot_new_cases_mco = ggplot(covid_malaysia, aes(x = Date_reported, y = New_cases)) +
  geom_rect(aes(xmin = ymd('2020-03-18'),
                xmax = ymd('2020-05-03'),
                ymin = -Inf,
                ymax = Inf, fill = "MCO"), alpha = 0.05) +
  geom_rect(aes(xmin = ymd('2020-05-04'),
                xmax = ymd('2020-06-09'),
                ymin = -Inf,
                ymax = Inf, fill = "CMCO"), alpha = 0.05) +
  geom_rect(aes(xmin = ymd('2020-06-10'),
                xmax = ymd('2021-01-11'),
                ymin = -Inf,
                ymax = Inf, fill = "RMCO"), alpha = 0.05) +
  geom_rect(aes(xmin = ymd('2021-01-11'),
                xmax = ymd('2021-03-31'),
                ymin = -Inf,
                ymax = Inf, fill = "MCO 2.0"), alpha = 0.05) +
  geom_rect(aes(xmin = ymd('2021-04-01'),
                xmax = ymd('2021-05-31'),
                ymin = -Inf,
                ymax = Inf, fill = "MCO by states"), alpha = 0.05) +
  geom_rect(aes(xmin = ymd('2021-06-01'),
                xmax = ymd('2021-12-31'),
                ymin = -Inf,
                ymax = Inf, fill = "NRP"), alpha = 0.05) +
  geom_line(color = 'black') +
  theme_classic()+
  labs(title = "Confirmed COVID-19 Cases per Day in Malaysia until 31 December 2022") +
  scale_y_continuous(breaks = seq(0, 40000, 5000)) +
  scale_x_date(labels = date_format("%b"), date_breaks = "3 months")+
  labs(x = "Month (Mar 2020 - Dec 2022)", y = "Number of Cases", fill = "Phases",color = "")

plot_new_cases_mco


###################
# Compare with thailand
covid_thailand <- read.csv("covid_thailand.csv",header=TRUE,sep=",")
covid_thailand <- covid_thailand[,-1]

# create a new dataframe to store new cases data in Malaysia and Thailand
new_cases_combined <- covid_malaysia[, c("Date_reported", "New_cases")]
new_cases_combined <- cbind(new_cases_combined, column3 = covid_thailand$New_cases)

colnames(new_cases_combined)[2] ="Malaysia"
colnames(new_cases_combined)[3] ="Thailand"

# Create the line graph
plot_case_MT <- ggplot(new_cases_combined, aes(x = Date_reported)) +
  geom_line(aes(y = Malaysia, color = "Malaysia")) +
  geom_line(aes(y = Thailand, color = "Thailand")) +
  labs(title = "COVID-19 Cases: Malaysia vs Thailand",
       x = "Month (Mar 2020 - Dec 2022)", y = "Number of Cases") +
  scale_color_manual(values = c("Malaysia" = "steelblue",
                                "Thailand" = "darkred")) +
  theme_classic()+
  scale_y_continuous(breaks=seq(0,40000,5000))+
  scale_x_date(labels = date_format("%b"), date_breaks = "3 month")
plot_case_MT

# create a new dataframe to store new deaths data in Malaysia and Thailand
new_deaths_combined <- covid_malaysia[, c("Date_reported", "New_deaths")]
new_deaths_combined <- cbind(new_deaths_combined, column3 = covid_thailand$New_deaths)

colnames(new_deaths_combined)[2] ="Malaysia"
colnames(new_deaths_combined)[3] ="Thailand"

# Create the line graph
plot_deaths_MT <- ggplot(new_deaths_combined, aes(x = Date_reported)) +
  geom_line(aes(y = Malaysia, color = "Malaysia")) +
  geom_line(aes(y = Thailand, color = "Thailand")) +
  labs(title = "COVID-19 Deaths: Malaysia vs Thailand",
       x = "Month (Mar 2020 - Dec 2022)", y = "Number of Deaths") +
  scale_color_manual(values = c("Malaysia" = "steelblue",
                                "Thailand" = "darkred")) +
  theme_classic()+
  scale_y_continuous(breaks=seq(0,600,100))+
  scale_x_date(labels = date_format("%b"), date_breaks = "3 month")

plot_deaths_MT



