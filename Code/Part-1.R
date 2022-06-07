### Assignment #1
### To solve the problem of patients missing their scheduled appointment
### and possibly identify those risky patients who are missing out on their 
### appointments, which could lead to higher costs and underutilization of
### medical infrastructure.
###
### As this problem is a classification problem, the target variable

## Import libraries
require(ggplot2)
require(tidyverse)

# prevent display of numbers in scientific notation
options(scipen = 999)


## user written function for creating descriptive statistics
mystats <- function(x) {
  nmiss <- sum(is.na(x))
  a <- x[!is.na(x)]
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
      UC = UC,
      LC = LC
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


## Working with the date columns and generating a third 
# type casting the date variables as type Date
appts$AppointmentDate <- as.Date(appts$AppointmentDate)
appts$AppointmentRegistration <- as.Date(appts$AppointmentRegistration)

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
# Show-up <- appts[appts$Status == "Show-Up",]

# 90732 records are of patients that did NOT show up for their
# scheduled appointment
summary(appts[appts$Status == "No-Show",])
nrow(appts[appts$Status == "No-Show",])


# noShow <- appts[appts$Status == "No-Show", ]

# The ratio between NoShow and ShowUp is: 90731: 209269


## Investigating the computed dayDifference variable

# scatter plot, Age by dayDifference
plot(appts$Age, appts$dayDifference)

# getting statistical data about dayDifference
# the day difference ranges from 1 day to 398 days!! Or up to 1 year and 1 month!
range(appts$dayDifference)  # the resulting range is 1 to 398 days
IQR(appts$dayDifference)  # Interquartile Range is 16
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
