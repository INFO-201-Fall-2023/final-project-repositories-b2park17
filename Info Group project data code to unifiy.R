library(dplyr)
library(stringr)


# our group data
Covid_19_data<-read.csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time_-_ARCHIVED.csv")
Average_Temp_data<-read.csv("2021_state_temp_data.csv")



selected_states<- c("MA","PA","CT","LA","FL","MS","MI","MN","WI","AZ","OK","NM","WA","UT","OR")
# Filter data for selected states and 2021
selected_states_2021 <- Covid_19_data[Covid_19_data$state %in% selected_states & 
                                        format(as.Date(Covid_19_data$submission_date, "%m/%d/%y"), "%Y") == "2021", ] 


selected_states_2021$month <- format(as.Date(selected_states_2021$submission_date, "%m/%d/%y"), "%m")

#summary of covid 19 data we want
monthly_totals <- aggregate(cbind(tot_cases, tot_death) ~ state + month, selected_states_2021, sum)

#adding 0 to single digit months
Average_Temp_data$Month <- sprintf("%02d", Average_Temp_data$Month)

#merging the data
combined_data <- merge(monthly_totals, Average_Temp_data, by.x = c("state","month"), by.y = c("State.abbreviation","Month"))

#fatality calculation
combined_data$M_Rate <- paste0(round(combined_data$tot_death/combined_data$tot_cases*100, 2), "%")

#sumamry of data to make categorical value
summary(combined_data$Average.temperature..)

combined_data$relative_temp[combined_data$Average.temperature.. >= 69.40] <- "high"
combined_data$relative_temp[combined_data$Average.temperature.. >= 40.77 & combined_data$Average.temperature.. < 69.40] <- "medium"
combined_data$relative_temp[combined_data$Average.temperature.. < 40.77] <- "low"


summary_data <- summary(combined_data)

write.csv(combined_data,"Combined Covid 19 and State Temperature.csv" )
