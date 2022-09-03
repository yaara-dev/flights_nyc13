# EDA

#flights_full_arranged_old<-flights_full_arranged

# grouped bar chart for model

counts_delay_per_var<-function(var_name, convert_x){
  g <- ggplot(flights_full_arranged, aes_string(fill = ("dep_delay"), x = var_name)) +
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
          angle = 90,
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


#####################

#function(var_name, pallete )
count_df<- flights_full_arranged %>% group_by(origin) %>% summarize(total_count=n())
df<-flights_full_arranged %>% group_by(origin, dep_delay) %>% tally() %>% filter(dep_delay==1) 
df<-merge(df, count_df, by=origin)
df <- df %>% mutate(relative_count_1=n/total_count)
ggplot(data = df, mapping = aes(x = origin.x, y = relative_count_1, fill = origin.x))+ geom_bar(stat="identity")+
  labs(fill='origin', x="origin", y='Normalized count delayed flights', title= "Normalized count delayed flights per category in origin")+
  scale_fill_manual(values=c(palette = "Greens"))+ 
  theme(plot.title = element_text(face = "bold",hjust=0.5, size=12), legend.key.size = unit(0.4, 'cm'))


                                                                        
                                                                        
'#330066','#663399', '#9966CC'
FUN = function(one_level) {
  df_summarize <-
    flights_full_perm %>% filter((!!sym(var_name)) == one_level) %>% group_by(dep_delay, .drop =
                                                                                FALSE) %>% tally
  number_delay_in_level <-
    df_summarize$n[df_summarize$dep_delay == 1]
  number_delay_in_level
}

