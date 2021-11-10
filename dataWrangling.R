library(tidyverse)

data <- read_csv("CFBeattendance.csv")
str(data)

#'To do:
#'1.Turn date and 'time' into time variables
#'2.Clean opponent string. Add new column for "In Conference"
#'3.Turn rank into factor ?
#'4.Clean up site string, additional column for "big game" data
#'5.Clean up result string, split into different columns
#'
