create_heatmap<-function(dataset)
{
  YEAR<-dataset$YEAR
  MONTH<-dataset$MONTH
  ggplot(dataset, aes(YEAR, MONTH)) +
    geom_tile(aes(fill = COUNT), color = "black") +
    scale_fill_gradient(low = "white", high = "red") +
    ylab("Month ") +
    xlab("Year") +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 12),
          plot.title = element_text(size=16),
          axis.title=element_text(size=14,face="bold"),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(fill = "COUNT") + scale_y_discrete(limits = month.abb) + scale_x_discrete(limits = YEAR) 
  
}

aggregate_heatmap<-function(dataset, variable)
{

aggregated_dataset<-subset(dataset, variable != 0)
aggregated_dataset <- aggregate(variable ~ YEAR + MONTH, data = dataset, FUN = sum)
names(aggregated_dataset) <- c("YEAR", "MONTH", "COUNT")
aggregated_dataset$MONTH <- as.factor(month.abb[aggregated_dataset$MONTH]) 
return(aggregated_dataset)
}

create_cause_dataset<-function(dataset,cause)
{

causeLocation<-dataset[apply(dataset[,c("CONTRIBUTING.FACTOR.VEHICLE.1","CONTRIBUTING.FACTOR.VEHICLE.2","CONTRIBUTING.FACTOR.VEHICLE.3","CONTRIBUTING.FACTOR.VEHICLE.4","CONTRIBUTING.FACTOR.VEHICLE.5")],1,function(x) any(x %in% cause)),]

causeLocation<-causeLocation[apply(causeLocation[,c("CONTRIBUTING.FACTOR.VEHICLE.1","CONTRIBUTING.FACTOR.VEHICLE.2","CONTRIBUTING.FACTOR.VEHICLE.3","CONTRIBUTING.FACTOR.VEHICLE.4","CONTRIBUTING.FACTOR.VEHICLE.5")],1,function(x) any(!x %in% "Unspecified")),]

causeLocation<-causeLocation[apply(causeLocation[,c("CONTRIBUTING.FACTOR.VEHICLE.1","CONTRIBUTING.FACTOR.VEHICLE.2","CONTRIBUTING.FACTOR.VEHICLE.3","CONTRIBUTING.FACTOR.VEHICLE.4","CONTRIBUTING.FACTOR.VEHICLE.5")],1,function(x) any(!x %in% "")),]  

return(causeLocation)
}
 


cause_plot<-function(dataset)
{
  
  plot<-ggplot(data = dataset, aes(x = CAUSE, y = log(TOTAL), fill = TYPE)) +
  geom_bar(data = subset(dataset, TYPE=="PERSONS.INJURED"), stat = "identity") +geom_bar(data = subset(dataset, TYPE=="PERSONS.KILLED"),
stat = "identity",position = "identity",mapping = aes(y = -log(TOTAL))) +scale_y_continuous(labels = abs) +coord_flip()+ggtitle('Causes of Accidents') 
  return(plot)
  
}

geo_plot<-function(map,dataset,type)
{
  plot <- ggmap(map)+geom_point(data=subset(dataset,TYPE==type), 
                                             aes(x=LONGITUDE, y=LATITUDE, colour=TOTAL),size=1,alpha=0.2)+scale_color_continuous(low = "red",  high = "black")
  return(plot)
}

geo_plot_2<-function(map,dataset,type)
{
  plot <- ggmap(map)+geom_point(data=subset(dataset,TYPE==type), 
                                aes(x=LONGITUDE, y=LATITUDE, colour=TOTAL),size=5,alpha=0.2)+scale_color_continuous(low = "red",  high = "black")
  return(plot)
}