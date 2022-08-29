# dep delay 2 categories 

#packages and libraries
list.of.packages <-
  c("dataPreparation",
    "nycflights13",
    "dplyr",
    "tidyverse",
    "RColorBrewer",
    "ggplot2")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  install.packages(new.packages)
}


library(dataPreparation)
library(nycflights13)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)

flights <- flights
airlines <- airlines
airports <- airports
planes <- planes
weather <- weather


#merged flights + weather + planes + airports(tzone)
flights_planes <- merge(flights, planes, by = "tailnum")
flights_planes_weather <-
  merge(flights_planes, weather, by = c("origin", "time_hour"))
flights_full <-
  merge(flights_planes_weather,
        airports,
        by.x = c("dest"),
        by.y = c("faa"))

#before processing (drop na and outliers)
flights_full_old <- flights_full


summary(flights_full)

#suspected columns to have mostly NAs
length(which(is.na(flights_full$speed)))
length(which(is.na(flights_full$wind_gust)))


######

## cyclic features
#flights_full$hour_sin = sin(flights_full$hour.x * (2 * pi / 24))
#flights_full$hour_cos = cos(flights_full$hour.x * (2 * pi / 24))
#flights_full$month_sin = sin((flights_full$month.x - 1) * (2 * pi / 12))
#flights_full$month_cos = cos((flights_full$month.x - 1) * (2 * pi / 12))

#convert hours to minutes
flights_full <-
  flights_full %>% mutate(
    sched_dep_time = hour.x * 60 + minute,
    sched_arr_time = floor(sched_arr_time / 100) *
      60 + sched_arr_time %% 100
  )


#remove NA from dep_delay
flights_full <- flights_full %>% drop_na(dep_delay)
flights_full <- flights_full %>% drop_na(tzone)


str(flights_full)


# Make dependent variable as a factor (categorical)
flights_full <- transform(
  flights_full,
  origin = as.factor(origin),
  carrier = as.factor(carrier),
  tzone = as.factor(tzone),
  type = as.factor(type),
  model = as.factor(model),
  engine = as.factor(engine),
  hour.y = as.numeric(hour.y),
  hour.x = as.numeric(hour.x),
  manufacturer = as.factor(manufacturer),
  month.x = as.factor(month.x)
)

#identical columns
identical(flights_full$hour.x, flights_full$hour.y)
identical(flights_full$month.x, as.factor(flights_full$month.y))
identical(flights_full$day.x, flights_full$day.y)


#remove identical and constant columns
flights_full <- fast_filter_variables(
  flights_full,
  level = 2,
  keep_cols = NULL,
  verbose = TRUE
)


#remove irrelevant columns
flights_full <-
  select(
    flights_full,-c(
      time_hour,
      arr_delay,
      flight,
      tailnum,
      arr_time,
      dep_time,
      name,
      lat,
      lon,
      alt,
      tz,
      dst,
      speed,
      wind_gust,
      dewp,
      hour.x,
      minute
    )
  )
different_columns <-
  colnames(flights_full_old)[!(colnames(flights_full_old) %in% colnames(flights_full))]


#remove sd outliers
flights_full <-
  remove_sd_outlier(flights_full,
                    cols = "dep_delay",
                    n_sigmas = 7,
                    verbose = TRUE)


#display all flights_full
str(flights_full)

#divide the flights into 2 groups according to their dep_delay - flights above 20 min delay, and flights above -10 & until 20 min delay
flights_full_new_dep_delay <- flights_full[which(flights_full$dep_delay>-10),]
flights_full_arranged <- flights_full_new_dep_delay %>% arrange(dep_delay)

# plot histogram of original dep_delay before changing it into 2 categories with the threshold
ggplot(flights_full_arranged, aes(x = dep_delay)) +
  geom_histogram(color = "black", fill = "white", bins = 40) +
  geom_vline(aes(xintercept = 20, color = "delay > 20 min"),
             linetype = "dashed",
             size = 1.3) +
  scale_color_manual(name = "Treshold delay time", values = c("delay > 20 min" = "red")) +
  labs(
    x = "Departure delay time [min]",
    y = "counts of flights",
    title = paste('Histogram of departure delay time')
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 19, face = "bold"))

# percentage of dep_delay=0
(length(which(flights_full_arranged$dep_delay<(20))))/nrow(flights_full_arranged) 
# percentage of dep_delay=1
(length(which(flights_full_arranged$dep_delay>=(20))))/nrow(flights_full_arranged)

#change dep_delay column into categories (0 / 1)
flights_full_arranged <-
  flights_full_arranged %>% mutate(
    dep_delay = case_when(
      dep_delay < 20 ~ 0,
      dep_delay >= 20 ~1
    )
  )

#convert dep_delay to factor column
flights_full_arranged$dep_delay<- as.factor(flights_full_arranged$dep_delay)

#plot flights counts per 2 dep_delay categories
ggplot(flights_full_arranged, aes(dep_delay, fill = dep_delay)) + geom_bar(fill =
                                                                             c('#CC6666','#FFCCCC')) +     #'#660000', '#993333', '#CC6666'"#FFCCCC"
  labs(title = "Flights counts per departure delay category", x = "Departure delay categories") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  

# merge manufacturer and model columns into one column
manu_model <-
  paste(flights_full_arranged$manufacturer,
        flights_full_arranged$model,
        sep = "_")
flights_full_arranged$manu_model <- manu_model


# permutation function - skip it because it is already done 
levels_model<-levels(factor(flights_full_arranged$model)) #levels of model
levels_manufacturer<-levels(factor(flights_full_arranged$manufacturer)) #levels of manufacturer
levels_manu_model <- levels(factor(flights_full_arranged$manu_model)) #levels of manu_model
levels_dest<-levels(factor(flights_full_arranged$dest)) #levels of destinations
levels_seats<-levels(factor(flights_full_arranged$seats)) #levels of seats

# origin_num_delay_var<-function(var_name,levels_var) {
#   origin_delay_vec<-sapply(levels_var, simplify = TRUE, FUN=function(one_level){
#     df_summarize<-flights_full_arranged %>% filter((!! sym(var_name))==one_level) %>% group_by(dep_delay, .drop=FALSE) %>% tally
#     number_delay_in_level<-df_summarize$n[df_summarize$dep_delay==1]
#     number_delay_in_level
#   })
#   origin_delay_vec
# }
# 
# origin_num_delay_model<-origin_num_delay_var('model',levels_model)
# origin_num_delay_manufacturer<-origin_num_delay_var('manufacturer',levels_manufacturer)
# origin_num_delay_manu_model<-origin_num_delay_var('manu_model',levels_manu_model)
# origin_num_delay_dest<-origin_num_delay_var('dest',levels_dest)
# origin_num_delay_seats<-origin_num_delay_var('seats',levels_seats)
# 
# 
# perm_vec_var<-function(levels_var_model, origin_num_delay_var){
#   perm_vec<-rep(1, length(levels_var_model))
#   perm_vec<-setNames(perm_vec, names(origin_num_delay_var))  
# }
# 
# perm_ndelay_vec_model<-perm_vec_var(levels_model, origin_num_delay_model)
# perm_ndelay_vec_manufacturer<-perm_vec_var(levels_manufacturer, origin_num_delay_manufacturer)
# perm_ndelay_vec_manu_model<-perm_vec_var(levels_manu_model, origin_num_delay_manu_model)
# perm_ndelay_vec_dest<-perm_vec_var(levels_dest, origin_num_delay_dest)
# perm_ndelay_vec_seats<-perm_vec_var(levels_seats, origin_num_delay_seats)
# 
# create_perm_ndelay_vec_var<-function(flights_full_perm, var_name,levels_var, origin_num_delay_var, perm_vec_var) {
#   perm_num_delay_var <- sapply(levels_var, simplify = TRUE, FUN=function(one_level){
#     df_summarize<-flights_full_perm %>% filter((!! sym(var_name))==one_level) %>% group_by(dep_delay, .drop=FALSE) %>% tally
#     number_delay_in_level<-df_summarize$n[df_summarize$dep_delay==1]
#     number_delay_in_level
#   })
#   ind_greater<-which(perm_num_delay_var>=origin_num_delay_var)
#   perm_vec_var[ind_greater]<-perm_vec_var[ind_greater]+1
#   perm_vec_var
# }
# 
# num_perm<-2000
# for (iter in 1:num_perm){
#   print(iter)
#   flights_full_perm<-transform(flights_full_arranged, dep_delay = sample(dep_delay))
#   perm_ndelay_vec_model<-create_perm_ndelay_vec_var(flights_full_perm, 'model', levels_model, origin_num_delay_model, perm_ndelay_vec_model)
#   perm_ndelay_vec_manufacturer<-create_perm_ndelay_vec_var(flights_full_perm, 'manufacturer', levels_manufacturer, origin_num_delay_manufacturer, perm_ndelay_vec_manufacturer)
#   perm_ndelay_vec_manu_model<-create_perm_ndelay_vec_var(flights_full_perm, 'manu_model', levels_manu_model, origin_num_delay_manu_model, perm_ndelay_vec_manu_model)
#   perm_ndelay_vec_dest<-create_perm_ndelay_vec_var(flights_full_perm, 'dest', levels_dest, origin_num_delay_dest, perm_ndelay_vec_dest)
#   perm_ndelay_vec_seats<-create_perm_ndelay_vec_var(flights_full_perm, 'seats', levels_seats, origin_num_delay_seats, perm_ndelay_vec_seats)
# }

# save perm_ndelay_vec_var output from permutations to csv (as tibble data frmae)
#perm_ndelay_vec_model_df = tibble(name = names(perm_ndelay_vec_model), value = perm_ndelay_vec_model)
#write.table(perm_ndelay_vec_model_df , file = "G:/My Drive/University/Msc/Big_Data_Gur/final_project/perm_ndelay_vec_model_df.csv",  sep=",",  row.names=FALSE)
#perm_ndelay_vec_manufacturer_df = tibble(name = names(perm_ndelay_vec_manufacturer), value = perm_ndelay_vec_manufacturer)
#write.table(perm_ndelay_vec_manufacturer_df , file = "G:/My Drive/University/Msc/Big_Data_Gur/final_project/perm_ndelay_vec_manufacturer_df.csv",  sep=",",  row.names=FALSE)
#perm_ndelay_vec_manu_model_df = tibble(name = names(perm_ndelay_vec_manu_model), value = perm_ndelay_vec_manu_model)
#write.table(perm_ndelay_vec_manu_model_df , file = "G:/My Drive/University/Msc/Big_Data_Gur/final_project/perm_ndelay_vec_manu_model_df.csv",  sep=",",  row.names=FALSE)
#perm_ndelay_vec_dest_df = tibble(name = names(perm_ndelay_vec_dest), value = perm_ndelay_vec_dest)
#write.table(perm_ndelay_vec_dest_df , file = "G:/My Drive/University/Msc/Big_Data_Gur/final_project/perm_ndelay_vec_dest_df.csv",  sep=",",  row.names=FALSE)
#perm_ndelay_vec_seats_df = tibble(name = names(perm_ndelay_vec_seats), value = perm_ndelay_vec_seats)
#write.table(perm_ndelay_vec_seats_df , file = "G:/My Drive/University/Msc/Big_Data_Gur/final_project/perm_ndelay_vec_seats_df.csv",  sep=",",  row.names=FALSE)



#display plots of p-val for each level and flights count for each level
num_perm<-2000
perm_result_var_df<-function(var_name) {
  perm_ndelay_var_df <- read.table(file=paste("G:/My Drive/University/Msc/Big_Data_Gur/final_project/perm_ndelay_merged_vec_",var_name,"_df.csv", sep=""),  sep=",", header=TRUE)
  colnames(perm_ndelay_var_df)[1]<-var_name
  perm_ndelay_var_df$p_val<- perm_ndelay_var_df$value/num_perm
  perm_ndelay_var_df$p_adj<-p.adjust(perm_ndelay_var_df$p_val, method = "BH")

  flights_counts_var_df <-
     flights_full_arranged %>% group_by((!! sym(var_name))) %>% summarise(total_counts =
                                                                   n())
  perm_ndelay_var_df <-
    merge(perm_ndelay_var_df, flights_counts_var_df, by = var_name) #add flights counts per level
  
  perm_ndelay_var_df<- perm_ndelay_var_df%>% arrange(p_adj)

   p_val_plot<-ggplot(perm_ndelay_var_df, aes(x = reorder((!! sym(var_name)), p_adj) , y = p_adj)) + geom_bar(stat ="identity") +
     labs(title = paste("p-value adjusted per", var_name, "level"), x = var_name, y ="p-value adjusted") +
     theme(plot.title = element_text(hjust = 0.5, size = 19, face = "bold"),
       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6)) +
     geom_hline(aes(yintercept = 0.05, color="0.05"),linetype = "dashed",size = 1.5)+
     scale_color_manual(name = "FDR cutoff", values =c("0.05"="red"))

   flights_counts_plot<-ggplot(perm_ndelay_var_df,  aes(x = reorder((!! sym(var_name)), p_adj) , y = total_counts)) +
     geom_bar(stat ="identity") + labs(title =paste("flights counts per", var_name, "level"), x = var_name, y ="flights counts") +
     theme(plot.title = element_text(hjust = 0.5, size = 19, face = "bold"),
           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6))

  return(list(perm_ndelay_var_df, p_val_plot, flights_counts_plot))
}

#permutation - model
perm_model_df<-perm_result_var_df("model")
perm_ndelay_model_df<-perm_model_df[[1]]
perm_model_df[[2]]
perm_model_df[[3]]


#permutation - manufacturer
perm_manufacturer_df<-perm_result_var_df("manufacturer")
perm_ndelay_manufacturer_df<-perm_manufacturer_df[[1]]
perm_manufacturer_df[[2]]
perm_manufacturer_df[[3]]

#permutation - manu_model
perm_manu_model_df<-perm_result_var_df("manu_model")
perm_ndelay_manu_model_df<-perm_manu_model_df[[1]]
perm_manu_model_df[[2]]
perm_manu_model_df[[3]]

#permutation - dest
perm_dest_df<-perm_result_var_df("dest")
perm_ndelay_dest_df<-perm_dest_df[[1]]
perm_dest_df[[2]]
perm_dest_df[[3]]

#permutation - seats
perm_seats_df<-perm_result_var_df("seats")
perm_ndelay_seats_df<-perm_seats_df[[1]]
perm_seats_df[[2]]
perm_seats_df[[3]]


#change model levels in flights_full_arranged
flights_full_arranged$model<-as.character(flights_full_arranged$model)
models_to_change<-perm_ndelay_model_df$model[which(perm_ndelay_model_df$p_adj<=0.05)] #filtered levels (were kept)
models_to_change
flights_full_arranged <-
  flights_full_arranged %>% mutate(model = replace(model, !(flights_full_arranged$model %in% models_to_change), 'other_model'))

flights_full_arranged$model<-as.factor(flights_full_arranged$model) #convert back to factor variable
levels(flights_full_arranged$model)

#change manufacturer levels in flights_full_arranged
flights_full_arranged$manufacturer<-as.character(flights_full_arranged$manufacturer)
manufacturers_to_change<-perm_ndelay_manufacturer_df$manufacturer[which(perm_ndelay_manufacturer_df$p_adj<=0.05)] #filtered levels (were kept)
manufacturers_to_change
flights_full_arranged <-
  flights_full_arranged %>% mutate(manufacturer = replace(manufacturer, !(flights_full_arranged$manufacturer %in% manufacturers_to_change), 'other_manufacturer'))

flights_full_arranged$manufacturer<-as.factor(flights_full_arranged$manufacturer) #convert back to factor variable
levels(flights_full_arranged$manufacturer)

#change manu_model levels in flights_full_arranged
flights_full_arranged$manu_model<-as.character(flights_full_arranged$manu_model)
manu_models_to_change<-perm_ndelay_manu_model_df$manu_model[which(perm_ndelay_manu_model_df$p_adj<=0.05)] #filtered levels (were kept)
manu_models_to_change
flights_full_arranged <-
    flights_full_arranged %>% mutate(manu_model = replace(manu_model, !(flights_full_arranged$manu_model %in% manu_models_to_change), 'other_manu_model'))

flights_full_arranged$manu_model<-as.factor(flights_full_arranged$manu_model) #convert back to factor variable
levels(flights_full_arranged$manu_model)

#change destination levels in flights_full_arranged
flights_full_arranged$dest<-as.character(flights_full_arranged$dest)
dests_to_change<-perm_ndelay_dest_df$dest[which(perm_ndelay_dest_df$p_adj<=0.05)] #filtered levels (were kept)
dests_to_change
flights_full_arranged <-
  flights_full_arranged %>% mutate(dest = replace(dest, !(flights_full_arranged$dest %in% dests_to_change), 'other_dest'))

flights_full_arranged$dest<-as.factor(flights_full_arranged$dest) #convert back to factor variable
levels(flights_full_arranged$dest)

#change seats levels in flights_full_arranged
flights_full_arranged$seats<-as.character(flights_full_arranged$seats)
seats_to_change<-perm_ndelay_seats_df$seats[which(perm_ndelay_seats_df$p_adj<=0.05)] #filtered levels (were kept)
seats_to_change
flights_full_arranged <-
  flights_full_arranged %>% mutate(seats = replace(seats, !(flights_full_arranged$seats %in% seats_to_change), 'other_seats'))

flights_full_arranged$seats<-as.factor(flights_full_arranged$seats) #convert back to factor variable
levels(flights_full_arranged$seats)



