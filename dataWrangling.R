library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)

data <- read_csv("CFBeattendance.csv")
str(data)

data %<>% mutate(Time = strptime(Time, format = "%I:%M %p"))
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
  mutate(BigGame = ifelse(is.na(BigGameName), 0, 1)) %>%
  mutate(BigGameName = str_sub(BigGameName, 1, -2))

names(data)<-make.names(names(data),unique = TRUE)

data %<>% mutate(GamesPlayed = Current.Wins + Current.Losses)

data %<>% mutate(Record = ifelse(Current.Wins + Current.Losses ==0, 0, Current.Wins/(GamesPlayed)))



needs_switch <- which(data$TMAX <= data$TMIN)
for (row in needs_switch){
  temp <- data[row,21]
  data[row,21] <- data[row,22]
  data[row,22] <- temp
}


#################################################################
#Visualization




