

scatter_line <- function(data){
  names <- c(`0` = "Not Rated Top 25 in Tailgating", `1` = "Rated Top 25 in Tailgating")
  
  ggplot(data)+
    aes(TMIN, Fill.Rate)+
    geom_point(aes(color = Team),alpha = 0.1)+
    labs(title="Minimum Temperature vs. Stadium Fill Rate, Stratified over Tailgating",x="Min Temperature (F)",y="Fill Rate (%)")+
    geom_smooth(aes(color = Team),method = "lm",se = FALSE)+theme(legend.position="none")+
    facet_wrap(~Tailgating, labeller = as_labeller(names))
}


record_bars <- function(data){
  mean_record <- data %>% group_by(Team) %>%
    summarise(Average_Record = mean(Record, na.rm = TRUE)) %>%
    ungroup()
  
  tg <- select(data, Team, Tailgating)
  tg <- tg[!duplicated(tg), ]
  rec_tg <- inner_join(mean_record,tg) %>% mutate(Tailgating = as.factor(Tailgating))

  ggplot(rec_tg) +
    geom_bar(stat = "identity", mapping = aes(x = reorder(Team, -Average_Record), y = Average_Record, fill = Tailgating,), show.legend = TRUE) +
    theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1))+ 
    geom_hline(yintercept = mean(rec_tg$Average_Record))+ 
    annotate("text", 55, mean(rec_tg$Average_Record), vjust = -1, label = "Overall Mean")+
    labs(title="Average Team Records from 2000 to 2018",x="Team", y="Average Record (%)")
}


fixed <- function(fit){
  coefplot(fit)
}

rand <- function(fit){
  plot_model(fit, type="re",
             vline.color="#A9A9A9", dot.size=1.5, value.offset=.2, sort.est = "Record")+
    theme(axis.text.y = element_text(size = 6))
}

distr <- function(data){
  ggplot(data)+geom_histogram()+
    aes(x = Fill.Rate, fill=Team, color = Team)+facet_wrap(~Team)+theme(legend.position="none")+
    xlab("Stadium Fill Rate (Percentage)")
}

distr_c <- function(data){
  ggplot(data)+geom_histogram()+
    aes(x = Fill.Rate, fill=Conference, color = Conference)+facet_wrap(~Conference)+theme(legend.position="none")+
    xlab("Stadium Fill Rate (Percentage)")
}


