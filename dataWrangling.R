library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)

data <- read_csv("CFBeattendance.csv",)
str(data)

#'To do:
#'1.Turn date and 'time' into time variables (Completed)
#'2.Clean opponent string. Add new column for "In Conference" (Completed)
#'3.Turn rank into factor ?
#'4.Clean up site string, additional column for "big game" data (Completed)
#'5.Clean up result string, split into different columns
#'

data %<>% mutate(Date = strptime(Date, format = "%m/%d/%Y"))

data %<>% mutate(Opponent = stri_enc_toutf8(Opponent))

data %<>% mutate(Opponent = gsub("No\\..*[0-9] ", "", Opponent)) %>% 
  mutate(Opponent = gsub("\\(.*\\) ", "", Opponent))

data %<>% mutate(NonConference = ifelse(str_sub(data$Opponent, -1, -1) == "*", 1, 0)) %>%
  mutate(Opponent = gsub("\\*", "", Opponent))


data %<>% separate(col= Site,
                     into = c("Site", "CollegeGameDay"),
                     sep = "\\(College GameDay\\)",
                     fill = "right") %>%
  mutate(CollegeGameDay = ifelse(is.na(CollegeGameDay), 0, 1))

data %<>% separate(col= Site,
                   into = c("Site", "BigGameName"),
                   sep = " \\(",
                   fill = "right") %>%
  mutate(BigGame = ifelse(is.na(BigGameName), 0, 1))

