# EDA

#flights_full_arranged_old<-flights_full_arranged

# grouped bar chart for model
# model
count_df_model<- flights_full_arranged %>% group_by(model) %>% summarize(total_count=n())
df_model<-flights_full_arranged %>% group_by(model, dep_delay) %>% tally() %>% filter(dep_delay==1) 
df_model<-merge(df_model, count_df_model, by= "model")
df_model <- df_model %>% mutate(relative_count_1=n/total_count)










#########
counts_delay_per_var<-function(df, var_name, convert_x){
  g <- ggplot(df, aes_string(fill = ("dep_delay"), x = var_name)) +
    geom_bar(position = "dodge", stat = "count") +
    labs(
      title = paste("flights counts in each dep_delay category per", var_name, "category"),
      x = var_name,
      y = "flights counts"
    ) 
    if (convert_x==TRUE){
      g+ theme(
        plot.title = element_text(hjust = 0.5,
                                  size = 19,
                                  face = "bold"),
        axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1,
          size = 6
        ))
    } else {
      g + theme(
      plot.title = element_text(hjust = 0.5,
                                size = 19,
                                face = "bold"))}

}

counts_delay_per_var("model", convert_x = TRUE)
counts_delay_per_var("origin", convert_x = FALSE)

#  +theme(
#    plot.title = element_text(hjust = 0.5,
#                              size = 19,
#                              face = "bold"),
#    axis.text.x = element_text(
#      angle = 45,
#      vjust = 0.5,
#      hjust = 1,
#      size = 6
#    ))
# g
#####################

flights_full_arranged_factors_all<-data.frame(lapply(flights_full_arranged,factor))
str(flights_full_arranged_factors_all)

gplot_var<-function(var_name, convert_x, palette_a=FALSE){
  count_df_var<- flights_full_arranged_factors_all %>% group_by(!!(sym(var_name))) %>% summarize(total_count=n())
  df_var<-flights_full_arranged_factors_all %>% group_by(!!(sym(var_name)), dep_delay) %>% tally() %>% filter(dep_delay==1) 
  df_var<-merge(df_var, count_df_var, by= var_name)
  df_var <- df_var %>% mutate(relative_count_1=n/total_count)
  varX<-paste(var_name,".x", sep="")
  colnames(df_var)[which(colnames(df_var)==varX)]<-var_name
  g<-ggplot(data = df_var, mapping = aes_string(x = var_name, y = "relative_count_1", fill = var_name))+ geom_bar(stat="identity", show.legend = FALSE)+
   labs(fill=var_name, x=var_name, y='Normalized count delayed flights', title= paste("Normalized count delayed flights per category in", var_name))
  if (convert_x==TRUE){
    t<- theme(
      plot.title = element_text(hjust = 0.5,
                                size = 19,
                                face = "bold"),
      axis.text.x = element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        size = 6
      ))
  } else {
    t <- theme(
      plot.title = element_text(hjust = 0.5,
                                size = 19,
                                face = "bold"),
      axis.text.x = element_text(
        size = 6))}
  if (!(palette_a==FALSE)){
    color<-scale_fill_brewer(palette = palette_a)
    g+t+color
  } else {
    g+t
  }
}

# plot normalized delayed flights in destination variable
dest_plot<-gplot_var("dest", convert_x=TRUE)                                                                        
dest_plot


# plot normalized delayed flights in model variable
origin_plot<-gplot_var("origin", convert_x=FALSE, palette_a="PuRd")                                                                        
origin_plot

# plot normalized delayed flights in month variable
colnames(flights_full_arranged_factors_all)[which(colnames(flights_full_arranged_factors_all)=="month.x")]<-"month"
month_plot<-gplot_var("month", convert_x=FALSE)                                                                        
month_plot

# plot normalized delayed flights in carrier variable
carrier_plot<-gplot_var("carrier", convert_x=FALSE)                                                                        
carrier_plot

# plot normalized delayed flights in year variable
colnames(flights_full_arranged_factors_all)[which(colnames(flights_full_arranged_factors_all)=="year.y")]<-"year"
year_plot<-gplot_var("year", convert_x=TRUE)                                                                        
year_plot

# plot normalized delayed flights in type variable
type_plot<-gplot_var("type", convert_x=FALSE, palette_a="Oranges")                                                                        
type_plot

# plot normalized delayed flights in manufacturer variable
manufacturer_plot<-gplot_var("manufacturer", convert_x=TRUE)                                                                        
manufacturer_plot

# plot normalized delayed flights in model variable
model_plot<-gplot_var("model", convert_x=TRUE)                                                                        
model_plot

# plot normalized delayed flights in engines variable
engines_plot<-gplot_var("engines", convert_x=FALSE, palette_a="BuGn")                                                                        
engines_plot

# plot normalized delayed flights in visib variable
visib_plot<-gplot_var("visib", convert_x=FALSE)                                                                        
visib_plot

# plot normalized delayed flights in tzone variable
tzone_plot<-gplot_var("tzone", convert_x=TRUE, palette_a="RdPu")                                                                        
tzone_plot

# plot normalized delayed flights in w_day variable
w_day_plot<-gplot_var("w_day", convert_x=FALSE, palette_a="YlGn")                                                                        
w_day_plot

# plot normalized delayed flights in week_num variable
week_num_plot<-gplot_var("week_num", convert_x=FALSE)                                                                        
week_num_plot

# plot normalized delayed flights in manu_model variable
manu_model_plot<-gplot_var("manu_model", convert_x=TRUE)                                                                        
manu_model_plot




count_df_var<- flights_full_arranged %>% group_by(origin) %>% summarize(total_count=n())
#head(count_df_var)
df_var<-flights_full_arranged %>% group_by(origin, dep_delay) %>% tally() %>% filter(dep_delay==1) 
df_var<-merge(df_var, count_df_var, by= origin)
#df_var <- df_var %>% mutate(relative_count_1=n/total_count)
#ggplot(data = df, mapping = aes(x = origin.x, y = relative_count_1, fill = origin.x))+ geom_bar(stat="identity")+
#  labs(fill='origin', x="origin", y='Normalized count delayed flights', title= "Normalized count delayed flights per category in origin")+
#  scale_fill_manual(values=c(palette = "Greens"))+ 
#  theme(plot.title = element_text(face = "bold",hjust=0.5, size=12), legend.key.size = unit(0.4, 'cm'))


#330066','#663399', '#9966CC'
FUN = function(one_level) {
  df_summarize <-
    flights_full_perm %>% filter(!!(enquo(var_name)) == one_level) %>% group_by(dep_delay, .drop =
                                                                                FALSE) %>% tally
  number_delay_in_level <-
    df_summarize$n[df_summarize$dep_delay == 1]
  number_delay_in_level
}

