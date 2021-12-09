library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)
library(arm)
library(rstan)
library(rstanarm)
library(lattice)

library(lme4)

#https://geoffboeing.com/2016/09/college-football-stadium-attendance/

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

data %<>% mutate(Opponent_Rank = factor(Opponent_Rank, levels = c("1", "2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","NR")))
data %<>% mutate(Rank = factor(Rank, levels = c("1", "2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","NR")))

data %<>% mutate(New.Coach = as.integer(New.Coach), Tailgating = as.integer(Tailgating))

needs_switch <- which(data$TMAX <= data$TMIN)
for (row in needs_switch){
  temp <- data[row,21]
  data[row,21] <- data[row,22]
  data[row,22] <- temp
}


#################################################################
#Visualization(EDA)

data_bc <- filter(data, Team == "Notre Dame")
plot(x = data_bc$Date, y = data_bc$Fill.Rate)
ggplot(data) + geom_line(mapping = aes(x=as.Date(Date), y=Fill.Rate, color = Team), show.legend = FALSE)

ggplot(data, aes(x = PRCP, y = Fill.Rate)) + geom_point()

ggplot(data, aes(x = SNOW, y = Fill.Rate)) + geom_point()

ggplot(data_bc, aes(x = TMAX, y = Fill.Rate)) + geom_point()

ggplot(data, aes(x = Rank, y = Fill.Rate)) + geom_point()

ggplot(data, aes(x = Record, y = Fill.Rate)) + geom_point()
ggplot(data_bc, aes(x = GamesPlayed, y = Fill.Rate)) + geom_point()

mean(filter(data, BigGame == 0)$Fill.Rate)
mean(filter(data, BigGame == 1)$Fill.Rate)

mean(filter(data, NonConference == 0)$Fill.Rate)
mean(filter(data, NonConference == 1)$Fill.Rate)

mean(filter(data, CollegeGameDay == 0)$Fill.Rate)
mean(filter(data, CollegeGameDay == 1)$Fill.Rate)

mean(filter(data, Tailgating == 0)$Fill.Rate)
mean(filter(data, Tailgating == 1)$Fill.Rate)

mean(filter(data, New.Coach == 0)$Fill.Rate)
mean(filter(data, New.Coach == 1)$Fill.Rate)

length(filter(data, Team == "Clemson")$Team)

ggplot(data)+geom_density(alpha=0.3)+
  aes(x=qlogis(Fill.Rate),color=Team)+facet_wrap(~Team)+theme(legend.position="none")+geom_rug()+
  xlab("Stadium Fill Rate (Percentage")+geom_vline(xintercept=mean(data$Fill.Rate),color="red",lty=2)

ggplot(data)+geom_line()+
  aes(x=as.Date(Date), y = Fill.Rate, color=Team)+facet_wrap(~Team)+theme(legend.position="none")+
  xlab("Stadium Fill Rate (Percentage")

ggplot(data)+geom_histogram()+
  aes(x = Fill.Rate, fill=Team, color = Team)+facet_wrap(~Team)+theme(legend.position="none")+
  xlab("Stadium Fill Rate (Percentage")

ggplot(data)+geom_histogram()+
  aes(x = Fill.Rate, fill=Conference, color = Conference)+facet_wrap(~Conference)+theme(legend.position="none")+
  xlab("Stadium Fill Rate (Percentage")

ggplot(data)+geom_jitter()+
  aes(x=BigGame, y = Fill.Rate, color=Team)+facet_wrap(~Team)+theme(legend.position="none")+
  xlab("Stadium Fill Rate (Percentage")

ggplot(data)+geom_jitter()+
  aes(x=NonConference, y = Fill.Rate, color=Team)+facet_wrap(~Team)+theme(legend.position="none")+
  xlab("Stadium Fill Rate (Percentage")


mean_fillrates <- data %>% group_by(Team) %>%
  summarise(Average_FillRate = mean(Fill.Rate, na.rm = TRUE)) %>%
  arrange(desc(Average_FillRate))



ggplot(mean_fillrates) +
  geom_bar(stat = "identity", mapping = aes(x = reorder(Team, -Average_FillRate), y = Average_FillRate, fill = Team), show.legend = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


hist(data$Fill.Rate)

# hist(qlogis(data$Fill.Rate-min(data$Fill.Rate) + .01))
# qqnorm(qlogis(data$Fill.Rate-min(data$Fill.Rate) + .01), pch = 1, frame = FALSE)
# qqline(qlogis(data$Fill.Rate-min(data$Fill.Rate) + .01), col = "steelblue", lwd = 2)
# 
# scale <- 1/(max(data$Fill.Rate) - min(data$Fill.Rate)) - .001
# test <- scale * data$Fill.Rate
# range(test)
# test <- test - min(test) + .001
# range(test)
# hist(test)
# hist(log(-log(test)))
# qqnorm(log(-log(test)), pch = 1, frame = FALSE)
# qqline(log(-log(test)), col = "steelblue", lwd = 2)

ggplot(data)+
  aes(TMAX, Fill.Rate)+
  geom_point(aes(color = Team),alpha = 0.1)+
  labs(title="Maximum Temperature vs. Stadium Fill Rate, Stratified over Tailgating Quality",x="Max Temperature (F)",y="Fill Rate (%)")+
  geom_smooth(aes(color = Team),method = "lm",se = FALSE)+theme(legend.position="none")+
  facet_wrap(~Tailgating)


#################################################################
#Model Selection

lin_fit <- stan_glm(Fill.Rate ~ Tailgating + PRCP + TMIN + NonConference + BigGame + Record, data = data, refresh = 0)
rbind(lin_fit$coefficients,lin_fit$ses)

multi_fit <- lmer(Fill.Rate ~ Tailgating + PRCP + TMIN + NonConference + BigGame + Record + (1+TMIN|Team), data)


scale <- 1/(max(data$Fill.Rate) - min(data$Fill.Rate)) - .001
test <- scale * data$Fill.Rate
range(test)
test <- test - min(test) + .001
data_t <- mutate(data,Fill_Fixed = test)
hist(qlogis(test))

hist(data$GamesPlayed)

fit4 <- lmer(qlogis(Fill_Fixed)~TMAX+GamesPlayed+Tailgating + BigGame + (1+TMAX|Team)+(1+Tailgating|Team),data_t)
fit5 <- lmer(qlogis(Fill_Fixed)~TMAX+Tailgating + BigGame + Month + (1+TMAX|Team)+(1+Record|Team)+(1+Tailgating|Team) +(1+Month|Team) ,data_t)

summary(fit5)
plot(fit4)
plot(fit5)
qqmath(fit4)
qqmath(fit5)
