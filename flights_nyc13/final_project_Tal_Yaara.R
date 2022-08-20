install.packages("nycflights13")
library(nycflights13)
library(dplyr)
library(tidyverse)
library(ggplot2)
flights<-flights
airlines<-airlines
airports<-airports
planes<-planes
weather<-weather

#install.packages("anyflights")
#library(anyflights)
#anyflights<-anyflights
#pdxflights19 <- anyflights("PDX", 2019, 6)

# comparison in dep delay between origin airport
num_flights<-as.matrix(table(flights$origin))
colnames(num_flights)<-"counts"

flights<-flights %>% mutate(dep_delay_norm =
                     case_when(origin == "EWR" ~ dep_delay/num_flights["EWR","counts"], 
                               origin == "JFK" ~ dep_delay/num_flights["JFK","counts"],
                               origin == "LGA" ~ dep_delay/num_flights["LGA","counts"]))

summarize_delay_origin<- flights %>% group_by(origin) %>% summarize(mean_delay=mean(dep_delay_norm, na.rm=TRUE), sd_delay= sd(dep_delay_norm, na.rm=TRUE), sum_delay=sum(dep_delay_norm, na.rm=TRUE))

#total mean, SD and sum delay
ggbar_mean_origin<-ggplot(data=summarize_delay_origin, aes(origin, mean_delay, fill=origin)) + geom_bar(stat="identity")+
  ggtitle('Mean of normalized departure delays per origin airport') + labs(fill='Origin', y='Mean of normalized departure delay [min]')+
  scale_fill_manual(values=c('#660000','#993333', '#CC6666'))+ 
  theme(plot.title = element_text(face = "bold",hjust=0.5))+
  geom_errorbar(aes(x=origin, ymin=mean_delay-sd_delay, ymax=mean_delay+sd_delay))
ggbar_mean_origin

ggbar_sd_origin<-ggplot(data=summarize_delay_origin, aes(origin, sd_delay, fill=origin)) + geom_bar(stat="identity")+ 
  ggtitle('SD of normalized departure delays per origin airport') + labs(fill='Origin', y='SD of normalized departure delay [min]')+
  scale_fill_manual(values=c('#336600', '#66CC33', '#66FF00'))+ 
  theme(plot.title = element_text(face = "bold",hjust=0.5))
ggbar_sd_origin

ggbar_sum_total_origin<-ggplot(data=summarize_delay_origin, aes(origin, sum_delay, fill=origin)) + geom_bar(stat="identity")+ 
  ggtitle('Total normalized time of departure delays per origin airport') + labs(fill='Origin', y='Total normalized time of departure delay [min]')+
  scale_fill_manual(values=c('#FF6600', '#FF9900', '#FFCC00'))+  
  theme(plot.title = element_text(face = "bold",hjust=0.5))
ggbar_sum_total_origin

#positive delay: mean, SD, sum
summarize_pos_delay_origin<-flights%>% filter(dep_delay_norm>0) %>% group_by(origin)%>%summarize(mean_delay= mean(dep_delay_norm, na.rm=TRUE),sd_delay= sd(dep_delay_norm, na.rm=TRUE), sum_delay=sum(dep_delay_norm, na.rm = TRUE))
ggbar_mean_pos_origin<-ggplot(data=summarize_pos_delay_origin, aes(origin, mean_delay, fill=origin)) + geom_bar(stat="identity")+
  ggtitle('Mean of positive normalized departure delays per origin airport') + labs(fill='Origin', y='Mean of positive normalized departure delay [min]')+
  scale_fill_manual(values=c('#330066','#663399', '#9966CC'))+ 
  theme(plot.title = element_text(face = "bold",hjust=0.5))
  #geom_errorbar(aes(x=origin, ymin=mean_delay-sd_delay, ymax=mean_delay+sd_delay))
ggbar_mean_pos_origin

ggbar_sd_pos_origin<-ggplot(data=summarize_pos_delay_origin, aes(origin, sd_delay, fill=origin)) + geom_bar(stat="identity")+ 
  ggtitle('SD of positive normalized departure delays per origin airport') + labs(fill='Origin', y='SD of positive normalized departure delay [min]')+
  scale_fill_manual(values=c('#009966', '#33CC66', '#66FF99'))+ 
  theme(plot.title = element_text(face = "bold",hjust=0.5))
ggbar_sd_pos_origin

ggbar_sum_total_pos_origin<-ggplot(data=summarize_pos_delay_origin, aes(origin, sum_delay, fill=origin)) + geom_bar(stat="identity")+ 
  ggtitle('Total normalized time of positive departure delays per origin airport') + labs(fill='Origin', y='Total normalized time of positive departure delay [min]')+
  scale_fill_manual(values=c('#003366', '#0066CC', '#3399FF'))+  
  theme(plot.title = element_text(face = "bold",hjust=0.5 , size=11), axis.title.y = element_text(size = 10))
ggbar_sum_total_pos_origin

#negative delay: mean, SD, sum
summarize_neg_delay_origin<-flights%>% filter(dep_delay_norm<0) %>% group_by(origin)%>%summarize(mean_delay= mean(dep_delay_norm, na.rm=TRUE),sd_delay= sd(dep_delay_norm, na.rm=TRUE), sum_delay=sum(dep_delay_norm, na.rm = TRUE))
summarize_neg_delay_origin$abs_mean_delay<-abs(summarize_neg_delay_origin$mean_delay)
summarize_neg_delay_origin$abs_sum_delay<-abs(summarize_neg_delay_origin$sum_delay)
neg_delay_flights<-flights%>% filter(dep_delay_norm<0) 
neg_delay_flights$dep_delay_norm<-abs(neg_delay_flights$dep_delay_norm)

box_neg_dep_delay<-ggplot(neg_delay_flights, aes(x=origin, y=dep_delay_norm, fill=origin)) +
  geom_boxplot() +stat_summary(fun=mean, shape=15,size=0.2) +
  labs(title="Boxplot of normalized negative departure delays per origin airport",x="Origin", y = "Time of normalized negative delay [min]")+
  theme(plot.title = element_text(face = "bold",hjust=0.5, size=12))
box_neg_dep_delay

#ggplot(summarize_neg_delay_origin,aes(x=origin))+geom_boxplot(aes(lower=abs_mean_delay-sd_delay,upper=abs_mean_delay+sd_delay, middle=abs_mean_delay,ymin=abs_mean_delay-min(sd_delay), ymax=abs_mean_delay+max(sd_delay)),stat="identity")

ggbar_sum_total_neg<-ggplot(data=summarize_neg_delay_origin, aes(origin, abs_sum_delay, fill=origin)) + geom_bar(stat="identity")+ 
  ggtitle('Total normalized time of negative departure delays per origin airport') + labs(fill='Origin', y='Total normalized time of negative departure delay [min]')+
  scale_fill_manual(values=c('#663399', '#9966CC', '#CC99FF'))+  
  theme(plot.title = element_text(face = "bold",hjust=0.5, size=12), axis.title.y = element_text(size = 9))
ggbar_sum_total_neg


# comparison in dep delay between carrier
summarize_delay_carrier<-flights%>%group_by(carrier)%>%summarize(mean_delay= mean(dep_delay_norm, na.rm=TRUE),sd_delay= sd(dep_delay, na.rm=TRUE), sum_delay=sum(dep_delay, na.rm = TRUE))

#total mean, SD and sum delay - carrier 
ggbar_mean_carrier<-ggplot(data=summarize_delay_carrier, aes(carrier, mean_delay, fill=carrier)) + geom_bar(stat="identity")+
  ggtitle('Mean normalized departure delays per carrier') + labs(fill='Carrier', y='Mean of normalized departure delay [min]')+
  #scale_fill_manual(values=c('#660000','#993333', '#CC6666'))+ 
  theme(plot.title = element_text(face = "bold",hjust=0.5), legend.key.size = unit(0.4, 'cm'))
  #geom_errorbar(aes(x=carrier, ymin=mean_delay-sd_delay, ymax=mean_delay+sd_delay))
ggbar_mean_carrier

ggbar_sd_carrier<-ggplot(data=summarize_delay_carrier, aes(carrier, sd_delay, fill=carrier)) + geom_bar(stat="identity")+ 
  ggtitle('SD normalized departure delays per carrier') + labs(fill='Carrier', y='SD of normalized departure delay [min]')+
  #scale_fill_manual(values=c('#336600', '#66CC33', '#66FF00'))+ 
  theme(plot.title = element_text(face = "bold",hjust=0.5), legend.key.size = unit(0.4, 'cm'))
ggbar_sd_carrier

ggbar_sum_total_carrier<-ggplot(data=summarize_delay_carrier, aes(carrier, sum_delay, fill=carrier)) + geom_bar(stat="identity")+ 
  ggtitle('Total normalized time of departure delays per carrier') + labs(fill='Carrier', y='Total normalized time of departure delay [min]')+
  #scale_fill_manual(values=c('#FF6600', '#FF9900', '#FFCC00'))+  
  theme(plot.title = element_text(face = "bold",hjust=0.5), legend.key.size = unit(0.4, 'cm'))
ggbar_sum_total_carrier

#positive delay - carrier: mean, SD, sum
summarize_pos_delay_carrier<-flights%>% filter(dep_delay_norm>0) %>% group_by(carrier)%>%summarize(mean_delay= mean(dep_delay_norm, na.rm=TRUE),sd_delay= sd(dep_delay_norm, na.rm=TRUE), sum_delay=sum(dep_delay_norm, na.rm = TRUE))

ggbar_mean_pos_carrier<-ggplot(data=summarize_pos_delay_carrier, aes(carrier, mean_delay, fill=carrier)) + geom_bar(stat="identity")+
  ggtitle('Mean normalized positive departure delays per carrier') + labs(fill='Carrier', y='Mean normalized positive departure delay [min]')+
  #scale_fill_manual(values=c('#330066','#663399', '#9966CC'))+ 
  theme(plot.title = element_text(face = "bold",hjust=0.5))
ggbar_mean_pos_carrier

ggbar_sd_pos_carrier<-ggplot(data=summarize_pos_delay_carrier, aes(carrier, sd_delay, fill=carrier)) + geom_bar(stat="identity")+ 
  ggtitle('SD normalized positive departure delays of each carrier') + labs(fill='Carrier', y='SD of normalized positive departure delay [min]')+
  #scale_fill_manual(values=c('#009966', '#33CC66', '#66FF99'))+ 
  theme(plot.title = element_text(face = "bold",hjust=0.5), legend.key.size = unit(0.4, 'cm'))
ggbar_sd_pos_carrier

ggbar_sum_total_pos_carrier<-ggplot(data=summarize_pos_delay_carrier, aes(carrier, sum_delay, fill=carrier)) + geom_bar(stat="identity")+ 
  ggtitle('Total time of normalized positive departure delays per carrier') + labs(fill='Carrier', y='Total time of normalized positive departure delay [min]')+
  #scale_fill_manual(values=c('#003366', '#0066CC', '#3399FF'))+  
  theme(plot.title = element_text(face = "bold",hjust=0.5), axis.title.y = element_text(size = 10), legend.key.size = unit(0.4, 'cm'))
ggbar_sum_total_pos_carrier

#negative delay: mean, SD, sum
summarize_neg_delay_carrier<-flights%>% filter(dep_delay_norm<0) %>% group_by(carrier)%>%summarize(mean_delay= mean(dep_delay_norm, na.rm=TRUE),sd_delay= sd(dep_delay_norm, na.rm=TRUE), sum_delay=sum(dep_delay_norm, na.rm = TRUE))
summarize_neg_delay_carrier$abs_mean_delay<-abs(summarize_neg_delay_carrier$mean_delay)
summarize_neg_delay_carrier$abs_sum_delay<-abs(summarize_neg_delay_carrier$sum_delay)
#neg_delay_flights<-flights%>% filter(dep_delay<0) already exists
#neg_delay_flights$dep_delay<-abs(neg_delay_flights$dep_delay)

box_neg_dep_delay_carrier<-ggplot(neg_delay_flights, aes(x=carrier, y=dep_delay_norm, fill=carrier)) +
  geom_boxplot() +stat_summary(fun=mean, shape=15,size=0.2) +
  labs(title="Boxplot of normalized negative departure delays per carrier",x="Origin", y = "Time of normalized negative departure delay [min]")+
  theme(plot.title = element_text(face = "bold",hjust=0.5, size=12), legend.key.size = unit(0.5, 'cm'))
box_neg_dep_delay_carrier

mean_sd_bp_neg_dep_delay_carrier<-ggplot(summarize_neg_delay_carrier,aes(x=carrier , fill=carrier))+geom_boxplot(aes(lower=abs_mean_delay-sd_delay,upper=abs_mean_delay+sd_delay, middle=abs_mean_delay,ymin=abs_mean_delay-min(sd_delay), ymax=abs_mean_delay+max(sd_delay)),stat="identity")+
  labs(title="Mean & SD of normalized negative departure delays per carrier",x="Origin", y = "Time of normalized negative departure delay [min]")+
  theme(plot.title = element_text(face = "bold",hjust=0.5, size=12), legend.key.size = unit(0.4, 'cm'))
mean_sd_bp_neg_dep_delay_carrier

ggbar_mean_neg_carrier<-ggplot(data=summarize_neg_delay_carrier, aes(carrier, abs_mean_delay, fill=carrier)) + geom_bar(stat="identity")+
  ggtitle('Mean normalized negative departure delays per carrier') + labs(fill='Carrier', y='Mean normalized negative departure delay [min]')+
  #scale_fill_manual(values=c('#330066','#663399', '#9966CC'))+ 
  theme(plot.title = element_text(face = "bold",hjust=0.5, size=12), legend.key.size = unit(0.4, 'cm'))
ggbar_mean_neg_carrier


ggbar_sum_total_neg_carrier<-ggplot(data=summarize_neg_delay_carrier, aes(carrier, abs_sum_delay, fill=carrier)) + geom_bar(stat="identity")+ 
  ggtitle('Total time of negative departure delays per carrier') + labs(fill='Carrier', y='Total time of negative departure delay [min]')+
  #scale_fill_manual(values=c('#663399', '#9966CC', '#CC99FF'))+  
  theme(plot.title = element_text(face = "bold",hjust=0.5), legend.key.size = unit(0.4, 'cm'))
ggbar_sum_total_neg_carrier

#anova
aov.total_mean_orgin <- aov(dep_delay_norm ~ origin, data = flights)
# Summary of the analysis
summary(aov.total_mean_orgin)
kruskal.test(dep_delay_norm ~ origin, data = flights)


#positive
pos_flights<-flights%>% filter(dep_delay_norm>0)
aov.pos_mean_orgin <- aov(dep_delay_norm ~ origin, data = pos_flights)
summary(aov.pos_mean_orgin)
kruskal.test(dep_delay_norm ~ origin, data = pos_flights)


#negative
neg_flights<-flights%>% filter(dep_delay_norm<0)
aov.neg_mean_orgin <- aov(dep_delay_norm ~ origin, data = neg_flights)
summary(aov.neg_mean_orgin)
TukeyHSD(aov.neg_mean_orgin)
library(car)
leveneTest(dep_delay_norm ~ origin, data = neg_flights) #no homogeneity 
oneway.test(dep_delay_norm ~ origin, data = neg_flights) #ANOVA test with no assumption of equal variances
plot(aov.neg_mean_orgin, 2) #not normal
qqnorm(neg_flights$dep_delay_norm, pch = 1, frame = FALSE) #not normal
qqline(neg_flights$dep_delay_norm, col = "steelblue", lwd = 2) #not normal
kruskal.test(dep_delay_norm ~ origin, data = neg_flights)


#################
flights_delayed <- filter(flights,dep_delay>0)
by_time_hour_airport = group_by(flights_delayed,origin,time_hour)
sum_delay_count = summarize(by_time_hour_airport,totaldelay = mean(dep_delay),
                            count = n())

combine_df = merge(sum_delay_count, weather,by=c("origin","time_hour"))
by_visib = group_by(combine_df,visib)
avg_delay_v = summarize(by_visib,avg_dep_delay_time = mean(totaldelay))
number_of_delay_per_visib = summarize(by_visib,Avg_Delay_Count_Per_Visib = mean(count))

##############

#histogram of dep_delay with thresholds
q_dep_delay<-quantile(flights$dep_delay, na.rm=TRUE)
q_dep_delay
pos_flights<-flights %>% filter(dep_delay>0) 
q_dep_delay_pos<-quantile(pos_flights$dep_delay)
q_dep_delay_pos
hist_dep_delay<-ggplot(pos_flights, aes(x=dep_delay))+
  geom_histogram(color="darkblue", fill="lightblue") +
  labs(title="Histogram of positive departure delay",x="Positive departure delay (minutes)")+
  geom_vline(aes(xintercept=0, color="short_delay"))+
  geom_vline(aes(xintercept=10, color="middle_delay"))+
  geom_vline(aes(xintercept=50, color="long_delay"))+
  scale_color_manual(name = "Thresholds", values = c("short_delay"= "#FFFF00", "middle_delay"="#FF66FF", "long_delay"= "#FF0000"))+
  theme(plot.title= element_text(face="bold.italic",hjust=0.5))
hist_dep_delay

#panel.grid.major = element_line(colour="grey", size=0.5), panel.grid.minor = element_line(colour="white", size=0.5), legend.spacing.y = unit(1,"cm")

hist_zoom_dep_delay<-ggplot(pos_flights, aes(x=dep_delay))+
  geom_histogram(color="darkblue", fill="lightblue") +
  xlim(0,150)+
  labs(title="Histogram of positive departure delay",x="Positive departure delay (minutes)")+
  geom_vline(aes(xintercept=0, color="short_delay"))+
  geom_vline(aes(xintercept=10, color="middle_delay"))+
  geom_vline(aes(xintercept=50, color="long_delay"))+
  scale_color_manual(name = "Thresholds", values = c("short_delay"= "#FFFF00", "middle_delay"="#FF66FF", "long_delay"= "#FF0000"))+
  theme(plot.title= element_text(face="bold.italic",hjust=0.5))
hist_zoom_dep_delay

hist_zoom_changed_thr<-ggplot(pos_flights, aes(x=dep_delay))+
  geom_histogram(color="darkblue", fill="lightblue") +
  xlim(0,120)+
  labs(title="Histogram of positive departure delay",x="Positive departure delay (minutes)")+
  geom_vline(aes(xintercept=0, color="short_bf=0"))+
  geom_vline(aes(xintercept=10, color="middle_bf=10"))+
  geom_vline(aes(xintercept=50, color="long_bf=50"))+
  geom_vline(aes(xintercept=7, color="middle_after=7"))+
  geom_vline(aes(xintercept=40, color="long_after=40"))+
  geom_vline(aes(xintercept=15, color="middle_after=15"))+
  geom_vline(aes(xintercept=60, color="long_after=60"))+
  scale_color_manual(name = "Thresholds", values = c("short_bf=0"= "#FF00FF", "middle_bf=10"="#FF00FF", "long_bf=50"= "#FF00FF","middle_after=7"="#33CC33", "long_after=40"="#33CC33", "middle_after=15"="#FF6600", "long_after=60"="#FF6600"))+
  theme(plot.title= element_text(face="bold.italic",hjust=0.5))
hist_zoom_changed_thr

#divide dep_delay to groups accorgding to thresholds
short_thresh <- 10 #minutes
middle_thresh <- 50
flights <- flights %>% mutate(
  dep_delay_cat = case_when(
    as.numeric(dep_delay) > 0 & as.numeric(dep_delay) <= short_thresh ~1 ,
    as.numeric(dep_delay) > short_thresh & as.numeric(dep_delay) <= middle_thresh ~ 2,
    as.numeric(dep_delay) > middle_thresh ~3,
    TRUE ~ 0
  ))
flights <- flights %>% mutate(
  dep_delay_cat = case_when(
    as.numeric(dep_delay) > 0 & as.numeric(dep_delay) <= short_thresh ~1 ,
    as.numeric(dep_delay) > short_thresh & as.numeric(dep_delay) <= middle_thresh ~ 2,
    as.numeric(dep_delay) > middle_thresh ~3,
    TRUE ~ 0
  ))
flights$dep_delay_cat<- as.factor(flights$dep_delay_cat)

#pos_flights_dep_perc <- pos_flights %>% group_by(dep_delay_cat) %>% mutate(dep_perc=(dep_delay/sum(dep_delay))) %>% ungroup()
#check<-pos_flights_dep_perc %>% filter(dep_delay_cat==1)
#check<-pos_flights %>% group_by(dep_delay_cat) %>% mutate(percent = prop.table(dep_delay))

count_cat<-flights %>% group_by(origin,dep_delay_cat) %>% summarize(count=n()) #the result shown on bar_delay_counts

#stacked bar plot of dep_delay counts per origin
bar_counts_delay_origin<-ggplot(flights, aes(fill=dep_delay_cat, x=origin)) + 
  geom_bar(position="stack", stat="count")+
  scale_fill_brewer(palette="RdPu")+
  geom_text(aes(label = ..count..), stat="count", position=position_stack(vjust=0.5), colour= "black", size=3)+
  labs(title= "Counts of delays per delay category and origin", fill="dep_delay category")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5))
bar_counts_delay_origin

#percentage bar plot of dep_delay counts per origin
count_cat_perc<-count_cat %>% group_by(origin) %>% mutate(percent = prop.table(count))
bar_percent_delay_origin<-ggplot(count_cat_perc, aes(fill=dep_delay_cat, x=origin, y=percent)) + 
  geom_col()+
  scale_fill_brewer(palette="RdPu")+
  geom_text(aes(label = scales::percent(percent)), position=position_stack(vjust=0.5), colour= "black", size=3)+
  scale_y_continuous(label = scales::percent)+
  labs(title= "Percentage of delays per delay category and origin", fill="dep_delay category")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5))
bar_percent_delay_origin

#stacked bar plot of dep_delay counts per carrier
count_origin_carrier<-flights %>% group_by(origin, carrier) %>% summarize(count=n(), mean=mean(dep_delay, na.rm=TRUE))
count_cat_origin_carrier<- flights%>% group_by(origin,carrier,dep_delay_cat) %>%summarize(count=n())
count_cat_origin_carrier<-count_cat_origin_carrier %>% group_by(origin, carrier) %>% mutate(percent=count/sum(count))

bar_perc_delay_origin_carrier<-ggplot(data = count_cat_origin_carrier, mapping = aes(x = carrier, y = count, fill = dep_delay_cat)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +
  geom_text(aes(label = count), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Carrier", y = "Counts of dearture delays",
       title = "Counts of departure delays per carrier and origin")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 9))
bar_perc_delay_origin_carrier

#bar plot mean per carrier and origin
bar_mean_origin_carrier<-ggplot(data = count_origin_carrier, mapping = aes(x = carrier, y = mean, fill = carrier)) +
  geom_col()+
  facet_wrap(~ origin, ncol = 1) +
  geom_text(aes(label = sprintf("%0.2f", round(mean, digits = 2))), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Carrier", y = "Mean of dearture delay time (min)",
       title = "Mean of departure delay time per carrier and origin")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size=8),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=5))
bar_mean_origin_carrier


#bar plot with counts of dep_delay_cat per carrier in all 3 airport
bar_count_delay_origin_carrier<-ggplot(data = count_cat_origin_carrier, mapping = aes(x = carrier, y = count, fill = dep_delay_cat)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +
  scale_fill_manual(values=c('#660000','#993333', '#CC6666', "#FFCCCC"))+ 
  geom_text(aes(label = count), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Carrier", y = "Count",
       title = "Count of delay category per carrier and origin")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 8), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=5),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=3))
bar_count_delay_origin_carrier

#bar plot with counts of dep_delay_cat without 0 per carrier in all 3 airport
count_cat_origin_carrier_0<- flights%>% filter(dep_delay>0)%>% group_by(origin,carrier,dep_delay_cat) %>%summarize(count=n())%>% mutate(percent=count/sum(count))

bar_count_delay_origin_carrier_0<-ggplot(data = count_cat_origin_carrier_0, mapping = aes(x = carrier, y = count, fill = dep_delay_cat)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +
  scale_fill_manual(values=c('#660000','#993333', '#CC6666'))+ 
  geom_text(aes(label = count), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Carrier", y = "Count",
       title = "Count of delay category per carrier and origin")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 8), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=5),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=3))
bar_count_delay_origin_carrier_0


#bar_counts_delay_carrier<-ggplot(pos_flights, aes(fill=dep_delay_cat, x=carrier)) + 
#  geom_bar(position="stack", stat="count")+
#  scale_fill_manual(values=c('#660000','#993333', '#CC6666'))+ 
#  geom_text(aes(label = ..count..), stat="count", position=position_stack(vjust=0.5), colour= "white", size=1.5)+
#  labs(title= "Counts of positive delays per delay category and origin", fill="dep_delay category")+
#  theme(plot.title = element_text(face="bold.italic", hjust=0.5))
#bar_counts_delay_carrier


#prepare data for stacked bar plot of dep_delay counts according to tailnum
flights_tailnum= merge(flights, planes, by="tailnum")
count_cat_origin_manu<-flights_tailnum%>% group_by(origin, manufacturer, dep_delay_cat)%>%summarize(count=n())
count_cat_origin_manu<-count_cat_origin_manu %>% group_by(origin, manufacturer) %>% mutate(percent=count/sum(count))

bar_perc_delay_origin_manu<-ggplot(data = count_cat_origin_manu, mapping = aes(x = manufacturer, y = percent, fill = dep_delay_cat)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +
  geom_text(aes(label = sprintf("%0.2f", round(percent, digits = 2))), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Manufacturer", y = "Prevalence [num of delay / total num of flights per manufacturer]",
       title = "Prevalence of delay category per manufacturer and origin")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 8), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=5),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=3))
bar_perc_delay_origin_manu


#bar plot percent per manufacturer without 0 delay cat 
flights_tailnum_0<-flights_tailnum%>% filter(dep_delay_cat!=0)
count_cat_origin_manu_0<-flights_tailnum%>% group_by(origin, manufacturer, dep_delay_cat)%>%summarize(count=n())
count_cat_origin_manu_0<-count_cat_origin_manu_0 %>% group_by(origin, manufacturer) %>% mutate(percent=count/sum(count))

bar_perc_delay_origin_manu_0<-ggplot(data =count_cat_origin_manu_0, mapping = aes(x = manufacturer, y = percent, fill = dep_delay_cat)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +
  geom_text(aes(label = sprintf("%0.2f", round(percent, digits = 2))), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Manufacturer", y = "Prevalence [num of delay / total num of positive delay per manufacturer]",
       title = "Prevalence of delay category per manufacturer and origin")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 8), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=5),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=3))
bar_perc_delay_origin_manu_0

#bar plot with counts of dep_delay per manufacturer in all 3 airport
bar_count_delay_origin_manu<-ggplot(data = count_cat_origin_manu, mapping = aes(x = manufacturer, y = count, fill = dep_delay_cat)) +
  geom_col() +
  facet_wrap(~ origin, ncol = 1) +
  geom_text(aes(label = count), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Manufacturer", y = "Count",
       title = "Count of delay category per manufacturer and origin")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 8), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=5),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=3))
bar_count_delay_origin_manu

#bar plot With counts of dep_delay per manufacturer in EWR airport.
#already exists - flights_tailnum_0<-flights_tailnum%>% filter(dep_delay_cat!=0)
#already exists -count_cat_origin_manu_0<-flights_tailnum_0%>%  group_by(origin, manufacturer, dep_delay_cat)%>%summarize(count=n())
count_cat_origin_manu_0_EWR<-count_cat_origin_manu_0%>%filter(origin=='EWR')
bar_count_manu_0_EWR<-ggplot(data = count_cat_origin_manu_0_EWR, mapping = aes(x = manufacturer, y = count, fill = dep_delay_cat)) +
  geom_col() +
  geom_text(aes(label = count), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Manufacturer", y = "Count",
       title = "Count of delay category per manufacturer in EWR")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 8), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=5),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=3))
bar_count_manu_0_EWR

#bar plot With counts of dep_delay per manufacturer in JFK airport.
count_cat_origin_manu_0_JFK<-count_cat_origin_manu_0%>%filter(origin=='JFK')
bar_count_manu_0_JFK<-ggplot(data = count_cat_origin_manu_0_JFK, mapping = aes(x = manufacturer, y = count, fill = dep_delay_cat)) +
  geom_col() +
  geom_text(aes(label = count), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Manufacturer", y = "Count",
       title = "Count of delay category per manufacturer in JFK")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 8), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=5),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=3))
bar_count_manu_0_JFK

#bar plot With counts of dep_delay per manufacturer in LGA airport.
count_cat_origin_manu_0_LGA<-count_cat_origin_manu_0%>%filter(origin=='LGA')
bar_count_manu_0_LGA<-ggplot(data = count_cat_origin_manu_0_LGA, mapping = aes(x = manufacturer, y = count, fill = dep_delay_cat)) +
  geom_col() +
  geom_text(aes(label = count), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Manufacturer", y = "Count",
       title = "Count of delay category per manufacturer in LGA")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 8), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=5),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=3))
bar_count_manu_0_LGA

#stacked bar plot of dep_delay counts per number of seats
flights_tailnum_seats<-flights_tailnum%>%arrange(seats)
count_cat_origin_seats<-flights_tailnum_seats%>% group_by(origin, seats, dep_delay_cat)%>%summarize(count=n())
count_cat_origin_seats<-count_cat_origin_seats %>% group_by(origin, seats) %>% mutate(percent=count/sum(count))
count_cat_origin_seats$seats<-as.factor(count_cat_origin_seats$seats)

#bar count seats - "EWR"
bar_count_seats_EWR<-ggplot(data = count_cat_origin_seats%>%filter(origin=="EWR"), mapping = aes(x = seats, y = count, fill = dep_delay_cat)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#CC99FF",'#9966CC','#663399','#330066'))+ 
  geom_text(aes(label = ifelse(count>1000, count,"")), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Number of seats", y = "Count",
       title = "Count of delay category per num of seats in EWR")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 8), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=5),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=3))
bar_count_seats_EWR

#bar count seats - "JFK"
bar_count_seats_JFK<-ggplot(data = count_cat_origin_seats%>%filter(origin=="JFK"), mapping = aes(x = seats, y = count, fill = dep_delay_cat)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#CC99FF",'#9966CC','#663399','#330066'))+ 
  geom_text(aes(label = ifelse(count>1000, count,"")), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Number of seats", y = "Count",
       title = "Count of delay category per num of seats in JFK")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 8), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=5),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=3))
bar_count_seats_JFK

#bar count seats - "LGA"
bar_count_seats_LGA<-ggplot(data = count_cat_origin_seats%>%filter(origin=="LGA"), mapping = aes(x = seats, y = count, fill = dep_delay_cat)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#CC99FF",'#9966CC','#663399','#330066'))+ 
  geom_text(aes(label = ifelse(count>600, count,"")), position=position_stack(vjust=0.5), colour= "black", size=1)+
  labs(x = "Number of seats", y = "Count",
       title = "Count of delay category per num of seats in LGA")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5), axis.title.y = element_text(size = 8), legend.key.size = unit(0.3, 'cm'), legend.title = element_text(size=5),  
        legend.text = element_text(size=5), axis.text.x = element_text(angle = 45, hjust=1,size=3))
bar_count_seats_LGA

#scatter plot number of seat and mean of dep_delay in each airport
pos_flights_summ_mean<-flights_tailnum_0%>%group_by(origin,seats)%>%summarise(mean=mean(dep_delay))
pos_flights_summ_mean$seats<-as.factor(pos_flights_summ_mean$seats)
ggplot(pos_flights_summ_mean, aes(x=seats, y=mean)) + 
  geom_point()+
  facet_wrap(~ origin, ncol = 1)+
  labs(x = "Number of seats", y = "Mean of departur delay",
       title = "Mean of departure delay per num of seats and origin")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5),  
        axis.text.x = element_text(angle = 45, hjust=1,size=5))

#histograam of dep delay in each airport
EWR<-flights%>%filter(origin=="EWR")
EWR_hist<-ggplot(EWR, aes(x=dep_delay))+
  geom_histogram(color="darkblue", fill="lightblue", bins=100) +
  labs(title="Histogram of departure delays in EWR",x="Departure delay (minutes)")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5))
EWR_hist  
Q_EWR<-quantile(EWR$dep_delay, na.rm=TRUE)
min_EWR<-min(EWR$dep_delay, na.rm=TRUE)

LGA<-flights%>%filter(origin=="LGA")
LGA_hist<-ggplot(LGA, aes(x=dep_delay))+
  geom_histogram(color="darkblue", fill="lightblue", bins=100) +
  labs(title="Histogram of departure delays in LGA",x="Departure delay (minutes)")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5))
LGA_hist  
Q_LGA<-quantile(LGA$dep_delay, na.rm=TRUE)
min_LGA<-min(LGA$dep_delay, na.rm=TRUE)

JFK<-flights%>%filter(origin=="JFK")
JFK_hist<-ggplot(JFK, aes(x=dep_delay))+
  geom_histogram(color="darkblue", fill="lightblue", bins=100) +
  labs(title="Histogram of departure delays in JFK",x="Departure delay (minutes)")+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5))
JFK_hist  
Q_JFK<-quantile(JFK$dep_delay, na.rm=TRUE)
min_JFK<-min(JFK$dep_delay, na.rm=TRUE)

library(hrbrthemes)
ggplot(flights, aes(x=dep_delay, fill=origin)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins=100) +
  #scale_fill_manual(values=c("#69b3a2", "#404080", "#CC6600")) +
  #theme_ipsum() +
  labs(title="Histogram of departure delays per origin airport",x="Departure delay (minutes)",fill="")+
  guides(fill=guide_legend(title="Origin"))+
  theme(plot.title = element_text(face="bold.italic", hjust=0.5))

########################################################################################################
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot") 
library(rpart.plot)
library(dplyr)

new_flights <- select(flights, -c(arr_delay, minute))
new_weather<- select(weather, -c(hour, year))
combined_df<-merge(new_flights, planes, by="tailnum")
combined_df<-merge(combined_df,new_weather,by=c("origin","time_hour"))
combined_df <- select(combined_df, -c(time_hour))
str(combined_df)
# Make dependent variable as a factor (categorical)
combined_df$origin = as.factor(combined_df$origin)
combined_df$tailnum = as.factor(combined_df$tailnum)
combined_df$carrier = as.factor(combined_df$carrier)
combined_df$dest = as.factor(combined_df$dest)
combined_df$type = as.factor(combined_df$type)
combined_df$manufacturer = as.factor(combined_df$manufacturer)
combined_df$model = as.factor(combined_df$model)
combined_df$engine = as.factor(combined_df$engine)

combined_df<-combined_df%>% drop_na(dep_delay)
combined_df<-combined_df%>% drop_na(dep_delay)
#combined_df<-na.omit(combined_df)

# combined_df_trn<-combined_df_trn%>% drop_na(dep_delay)
# combined_df_tst<-combined_df_tst%>% drop_na(dep_delay)

#train-test split
set.seed(42)
combined_df_idx   = sample(nrow(combined_df), (nrow(combined_df))*0.8)
combined_df_trn = combined_df[combined_df_idx, ]
combined_df_tst =combined_df[-combined_df_idx, ]

model <- rpart(dep_delay ~ origin+carrier, data=combined_df_trn, method = 'anova', control= list(maxdepth=20, rpart.control(cp=.00001)))
#view results
rpart.plot(model)
#view results
printcp(model)  

#identify best cp value to use
best <- model$cptable[which.min(model$cptable[,"xerror"]),"CP"]
#produce a pruned tree based on the best cp value
pruned_tree <- prune(model, cp=best)

#plot the pruned tree
prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output


#use pruned tree to predict salary of this player
predict(pruned_tree, newdata=combined_df_tst)

model_cp <- rpart(dep_delay ~ origin+carrier+wind_speed + wind_dir, data=combined_df_trn, method = 'anova', control=rpart.control(cp=.00001))
model_cp <- rpart(dep_delay ~ ., data=combined_df_trn, method = 'anova', control=rpart.control(cp=.0001))
#view results
rpart.plot(model_cp)
printcp(model_cp)
plotcp(model_cp)
best <- model_cp$cptable[which.min(model_cp$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(model_cp, cp=best)
rpart.plot(pruned_tree)

model_no_cp$

combined_df
library(party)
fit <- ctree(dep_delay ~ origin+carrier+wind_speed + wind_dir, data=combined_df_trn)
plot(fit, main="Conditional Inference Tree for Kyphosis")
