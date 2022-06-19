### Assignment #1
### To solve the problem of patients missing their scheduled appointment
### and possibly identify those risky patients who are missing out on their 
### appointments, which could lead to higher costs and underutilization of
### medical infrastructure.
###
### As this problem is a classification problem, the target variable

########################################################################
### To do
###
### 1 - Create a new feature/variable "HourOfTheDay" which will indicate the
###     hour of the day at which the appointment was booked




# Data Type
# Age - discreet - Age of patient
# Gender - categorical - Male or Female
# Appointment Registration - date - date of appointment registration
# Appointment Date - date - date of actual appointment
# Diabetes - binary - True / False
# Alcoholism - binary - True / False
# HyperTension - binary - True / False
# Handicap - binary - True / False
# Smokes - binary - True / False
# Scholarship - binary - True / False
# Tuberculosis - binary - True / False
# SMS Reminder - binary - True / False
# Status - Categorical - Show or no-show


## Import libraries
require(ggplot2)
require(ggmosaic)
require(tidyverse)
require(caret)
require(stats)
require(dplyr)


# prevent display of numbers in scientific notation
options(scipen = 999)


## user written function for creating descriptive statistics
mystats <- function(x) {
  nmiss <- sum(is.na(x))
  a <- x[!is.na(x)]
  med <- median(x)
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  # p1 <- quantile(a, 0.01)
  # p5 <- quantile(a, 0.05)
  # p10 <- quantile(a, 0.10)
  q1 <- quantile(a, 0.25)
  q2 <- quantile(a, 0.5)
  q3 <- quantile(a, 0.75)
  # p90 <- quantile(a, 0.90)
  # p95 <- quantile(a, 0.95)
  # p99 <- quantile(a, 0.99)
  max <- max(a)
  UC <- m + 3 * s  # upper fence 
  LC <- m - 3 * s  # lower fence
  outlier_flag <- max > UC | min < LC
  return(
    c(
      n = n,
      nmiss = nmiss,
      outlier_flag = outlier_flag,
      mean = m,
      med = med,
      stdev = s,
      min = min,
      # p1 = p1,
      # p5 = p5,
      # p10 = p10,
      q1 = q1,
      q2 = q2,
      q3 = q3,
      # p90 = p90,
      # p95 = p95,
      # p99 = p99,
      max = max,
      LC = LC,
      UC = UC
      )
  )
}




## Set working directory
working_dir <- choose.dir()  # prompt user for location of home directory
setwd(working_dir)  # setting the home directory
rm(working_dir)  # removing the variable from memeory space
getwd()  # verifying the home directory


# Reading the data
# Setting any text variable as a Factor, i.e., the Gender as well
# as the Status varaible
appts <- read.csv("Data/appointments.csv", stringsAsFactors = TRUE)


# getting to know the dataset
dim(appts)  # 300000 observations by 13 variables/features
names(appts)  # headers/names of the 13 variables/features
str(appts)  # querying the structure of the dataset
summary(appts)  # getting a summary of all 13 features
head(appts)  # show top 5 observations
tail(appts)  # show bottom 5 observations
appts[is.na(appts)]   # checking for any missing data = 0


# *****************************************************************
# *                   verify binary data 
# * Verify min and max values - should have min of 0 and max of 1
# *****************************************************************

## Status
print("-----------------------------------")
print("Number of Patients who Showed Up versus who Missed:")
print(summary(appts$Status))
table(appts$Status)
# display a pie chart of the Status feature
z <- prop.table(table(appts$Status)) * 100
pie(prop.table(table(appts$Status)) * 100, 
    labels = paste0(appts$Status, ': ', (format(round(z, 2), nsmall = 2)), '%'),
    main = "Breakdown of Appointment Status",
    col = rainbow(5),
    border = TRUE
    )

# SMS_Reminders
print("-----------------------------------")
print("Breakdown of SMS Reminders")
print(mystats(appts$Sms_Reminder))
print(table(appts$Sms_Reminder))
## Interestingly enough, the median "med" of this variable is at 1, while the
## mean is at 0.57417
# pie chart of sms reminders sent
z <- prop.table(table(appts$Sms_Reminder)) * 100
barchart(z,
         #labels = z,
         labels = paste0(unique(appts$Sms_Reminder), ': ', (format(round(z, 2), nsmall = 2)), '%'),
         # labels = paste0(appts$Sms_Reminder, ': ', (format(round(z, 2), nsmall = 2)), '%'),
         main = "Breakdown of SMS Reminders Sent",
         col = rainbow(5), 
         xlab = "Frequency in %",
         ylab = "Number of SMS Messages Sent"
         )

BP = barplot(z, 
             main = "Breakdown of SMS Reminders Sent", 
             ylab = "%", 
             ylim = c(0, 100), 
             las = 1,  
             col = SEQUENTIAL)
text(BP, 
     label = paste0(unique(appts$Sms_Reminder), ': ', (format(round(z, 2), nsmall = 2)), '%'), 
     # label = z,
     pos = 3, 
     cex = 0.8)


require(plotly)




counts <- prop.table(table(appts$Sms_Reminder)) * 100
b <- barplot(counts, main= "number of yes/no", xlab = "response", ylab = "number of occurrences", ylim=c(0,4),
text(x= b, y=counts,pos = 3, label = counts, cex = 0.8, col = "red"))



mystats(appts$Diabetes)
mystats(appts$Alcoholism)
mystats(appts$HyperTension)
mystats(appts$Handicap)  # has a max of 4
mystats(appts$Smokes)
mystats(appts$Tuberculosis)
mystats(appts$Sms_Reminder)  # has a max of 2
# ****************************************************************
# * SMS Reminder can have values of 0, 1 or 2, indicating number of
# * times SMS was sent to the patient
# *****************************************************************


# *****************************************************************
# 
# Breakdown of those who didn't turn up against the number of
# SMS reminders sent to patients
prop.table(xtabs(~ Status + Sms_Reminder, data = appts))
# The percentage of patients who turned up with 1 phone call is 39.7%!
# At least 29.9% of patients who received no reminder turned up, 
# while 30% did not turn up. Maybe if more patients received an SMS reminder
# or two, more patients would not miss their appointments.


# identify number of records with more than 1 type of handicaps
nrow(appts[appts$Handicap > 1,])   # 499 records had handicap value not set to 
prop.table(table(appts$Handicap))



# ****************************************************************
# * Normalize the data for Handicaps to just 1, so it is either yes or no
# ****************************************************************
appts$Handicap[appts$Handicap > 1] = 1
mystats(appts$Handicap)


# *****************************************************************
# * Breakdown of those who turned up or not, by gender
# *****************************************************************
status_by_gender <- prop.table(table(appts[, c(2, 13)]))
status_by_gender

plot(status_by_gender)




# ******************************************************************
# * Creating Show_Up and No_Show subsets of data
# ******************************************************************
Show_Up <- appts[appts$Status == "Show-Up", ]
No_Show <- appts[appts$Status == "No-Show", ]

#ggplot(appts, aes(Status)) +  
#  geom_col(main = "Bar Plot of patients that showed up and their gender breakdown")
  


# finding the difference in days between the date of registration and the
# actual appointment
# appts$dayDifference <- as.numeric(appts$AppointmentDate -
#                                     appts$AppointmentRegistration)
diff_in_days = as.numeric(difftime(appts$AppointmentDate,
                                   appts$AppointmentRegistration,
                                   units = "days")) 
mystats(diff_in_days)
# With difference in days, the maximum value is outrageously big, 398

diff_in_weeks = as.numeric(difftime(appts$AppointmentDate,
                                    appts$AppointmentRegistration,
                                    units = "weeks"))
mystats(diff_in_weeks)
# With difference in weeks, at least the maximum value is 57%, seems more manageable

diff_in_months = as.numeric(difftime(appts$AppointmentDate,
                                     appts$AppointmentRegistration,
                                     units = "days")) / (365.25 / 12)
mystats(diff_in_months)
# Although the difference in years seems to lower the value further, I will follow the
# practice used by our local healthcare provider, which is in weeks.

## Combining the appts data to the difference in weeks
apptDifference <- diff_in_weeks
## Removing redundant calculated variables
rm(diff_in_days)
rm(diff_in_weeks)
rm(diff_in_months)

## Combining the new vector, apptDifference to the appts dataframe
appts <- cbind(appts, apptDifference)
rm(apptDifference)


## Working on the determining the split ratio between ShowUp and NoShow
# 209269 records are of patients that showed up for their appointment
summary(appts[appts$Status == "Show-Up",])  
nrow(appts[appts$Status == "Show-Up",])

# 90732 records are of patients that did NOT show up for their
# scheduled appointment
summary(appts[appts$Status == "No-Show",])
(appts[appts$Status == "No-Show",])

ShowUp <- appts[appts$Status == "Show-Up",]
noShow <- appts[appts$Status == "No-Show", ]

# The ratio between NoShow and ShowUp is: 90731: 209269
nrow(No_Show) / nrow(appts)
nrow(Show_Up) / nrow(appts)

## Investigating the computed apptDifference variable

#histogram of apptDifference
ggplot(appts, aes(apptDifference)) +
  geom_histogram(binwidth = 1) 
mystats(appts$apptDifference)  
# based on the mystats result, the Upper Level value of this variable is 8.7
# or 9, when rounded up
nrow(appts[appts$apptDifference > 8.7, ])  #4908 records are considered outliers 
# for the apptDifference variable

## The data population has Female patients 
ggplot(appts, aes(y = Status, x = Gender, fill = Status)) +
  geom_bar(stat = "identity") + 
  labs(y = "Count of Status", title = "Breakdown of Gender by Status") +
  theme_classic()



## number of records with > 100 in age
nrow(appts[appts$Age > 100, ])  # 27 records
nrow(appts[appts$Age < 1, ])  # 10332 records
ggplot(appts, aes(Age, fill = Gender)) +
  geom_histogram(binwidth = 1) +
  labs(y = "Count of Population",
       title = "Histogram of Age by Gender") +
  theme_classic()


# getting statistical data about dayDifference
# the day difference ranges from 1 day to 398 days!! Or up to 1 year and 1 month!

# png("Output/ApptDifference.png")

hist(appts$apptDifference, xlim = c(0, 10), ks = 50, 
     main = "Histogram of Appointment Difference in Weeks",
     xlab = "Difference in Weeks", ylab = "Frequency Count",
     prob = TRUE)
# plotting probability distribution in Red
lines(density(appts$apptDifference), lwd = 2, col = "red")

# dev.off()


range(appts$apptDifference)  # the resulting range is 1 to 398 days
IQR(appts$apptDifference)  # Interquartile Range is 16
# finding the upper and lower fence values of dayDifference column
age_upper_fence <- 20 + (1.5 * IQR(appts$dayDifference))# 44
age_upper_fence
age_lower_fence <- 4 - (1.5 * IQR(appts$dayDifference))  # -20
age_lower_fence

# Looking at our data, as our minimum value for Age is 1, we can set this value as
# our lower fence

# displaying a histogram of the ageDifference column
hist(appts$dayDifference, main = "Histogram of dayDifference")

# subsetting the dataset to exclude outlier records
subset1 <- appts[appts$dayDifference < 45, 14]
subset2 <- appts[appts$dayDifference > 44, 14]
hist(subset1, main = "Histogram of subset")
hist(subset2, main = "Histogram of outliers")

## Even after filtering the outlier records, the histogram shows the
## records are still skewed to the right, with a long left tail.
## Looking at the histogram of the outliers, it appears to flatten off
## at the 150 day mark

subset3 <- appts[appts$dayDifference < 151, 14]
(length(subset3) / nrow(appts)) * 100
hist(subset3, main = "")

# what about cropping off at 125?
subset3 <- appts[appts$dayDifference < 156, 14]
(length(subset3) / nrow(appts)) * 100
hist(subset3, main = "")

(length(subset1) / nrow(appts)) * 100

## However, based on the range of the dayDifference variable, 

nrow(appts[appts$dayDifference > age_upper_fence, ])  # 11,148 records
(nrow(appts[appts$dayDifference > age_upper_fence, ]) / nrow(appts)) * 100
# percentage of records whose dayDifference is greater than the upper fence 
# is a mere 1.7%. We can decide to use dataset with and without outliers and
# compare later 

# Proportion of Male to Female is very lopsided, Female outnumber Male by 2 to 1!
prop.table(table(appts$Gender))

# 20% of Female does not show up 10% of Male does not show up. 
# For those that show up, 47% are Female while 23% are Male
prop.table(table(appts$Gender, appts$Status))


# proportion of those that turned up with or without SMS
table(appts$Status, appts$Sms_Reminder)
prop.table(table(appts$Status, appts$Sms_Reminder))



table(appts$Status, appts$Scholarship)
prop.table(table(appts$Status, appts$Scholarship))



# As the range is from 1 to 398, but the upper fence is only 44 days, we have
# outliers that are


summary(appts$Age)

appts %>% 
  ggplot(aes(y = Age)) +
  geom_boxplot()

nrow(appts[appts$Age < 1,])  # 10,332 records have Age recorded less than 1

median(appts$Age, na.rm = TRUE)  # 38 is median Age

appts['Age'][appts['Age'] < 1] <- median(appts$Age, na.rm = TRUE) 
# setting the median age to those records that have age less than 1

summary(appts$Age)

appts %>% 
  ggplot(aes(y = Age)) +
  geom_boxplot()

appts %>% 
  ggplot(aes(y = dayDifference)) +
  geom_boxplot() 


appts %>% 
  ggplot(aes(y = Age)) +
  geom_boxplot()



## Segmenting data between No-Show and Show-Up

showUp <- appts[appts$Status == "Show-Up",]
summary(showUp$dayDifference)

noShow <- appts[appts$Status == "No-Show", ]
summary(noShow$dayDifference)



mystats(appts$Age)

mystats(appts$dayDifference)

nrow(appts[appts$dateDiff > 61,])  # 4669 records above upper fence


## Correlation matrix
corr_mat <- round(cor(appts[,5:11]),2) 
corr_mat

typeof(corr_mat)

library(ggplot2)
library(reshape2)

melted_corr_mat <- melt(corr_mat)

ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile()
## NO CORRELATION


########################################################
mystats(appts$Diabetes)
mystats(appts$Alcoholism)
mystats(appts$HyperTension)
mystats(appts$Handicap)
mystats(appts$Smokes)
mystats(appts$Handicap)
mystats(appts$Scholarship)
mystats(appts$Tuberculosis)


nrow(appts[appts$Handicap > 4,])
499


Show-up <- appts[appts$Status == "Show-Up",]
No-Show <- appts[appts$Status == "No-Show",]

mystats(appts$Status)



num_appts = sapply(appts, is.numeric)


num_col = appts[num_appts]
names(num_col)

test <- apply(num_col, 2, mystats)
diag_stats <- t(data.frame(test))
View(diag_stats)
