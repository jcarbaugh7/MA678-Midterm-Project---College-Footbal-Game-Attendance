# library(tidyverse)
# library(magrittr)
# library(stringr)
# library(stringi)
# # library(arm)
# # library(rstan)
# library(rstanarm)
# library(lattice)
# library(coefplot)
# 
# library(lme4)

data <- read_csv("CFBeattendance.csv")

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

data %<>% mutate(Record = ifelse(Current.Wins + Current.Losses ==0, 1, Current.Wins/(GamesPlayed)))

data %<>% mutate(Opponent_Rank = factor(Opponent_Rank, levels = c("1", "2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","NR")))
data %<>% mutate(Rank = factor(Rank, levels = c("1", "2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","NR")))

data %<>% mutate(Month_f = factor(Month, levels = c(8,9,10,11,12,1,4)))

data %<>% mutate(New.Coach = as.integer(New.Coach), Tailgating = as.integer(Tailgating))
data %<>% mutate(did_rain = ifelse(PRCP >0, 1,0), did_snow = ifelse(SNOW >0, 1,0))


needs_switch <- which(data$TMAX <= data$TMIN)
for (row in needs_switch){
  temp <- data[row,21]
  data[row,21] <- data[row,22]
  data[row,22] <- temp
}

scale <- 1/(max(data$Fill.Rate) - min(data$Fill.Rate)) - .001
test <- scale * data$Fill.Rate
test <- test - min(test) + .001
data_t <- mutate(data,Fill_Fixed = test)


#################################################################
#Model Selection

fit5 <- lmer(qlogis(Fill_Fixed)~ BigGame + Tailgating * Record + GamesPlayed * TMIN + NonConference + PRCP + (1 + Record|Team),data_t)

s <- summary(fit5)
p <-  plot(fit5)
q <- qqmath(fit5)

f <- fixef(fit5)
r <- ranef(fit5)



