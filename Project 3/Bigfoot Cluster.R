# import required libraries
library(tidyverse)
library(ggthemes)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(leaderCluster)

#read in 
bigfoot <- read.csv(file = "bfro_report_locations.csv")
head(bigfoot)

#modify date format so we can plot by date
bigfoot.date<-bigfoot %>% mutate(SightMonth=month(timestamp,label = T,abbr = T),SightYear=year(timestamp))

#plot sightings by month
ggplot(bigfoot.date,aes(x=SightMonth))+geom_bar(stat="count",color="black",fill="steelblue2")+
  geom_label(stat = "count",aes(label=paste(round(..count../sum(..count..),digits = 4)*100,"%",sep = "")))+
  labs(title="Distribution of Bigfoot Sightings by Month",x="Month",y="Sightings")+theme_bw(base_size = 12)+
  theme(axis.text = element_text(color="black"))

#generate a map that shows the different class of sightings

Class.A<-bigfoot.date %>% filter(classification=="Class A")
Class.B<-bigfoot.date %>% filter(classification=="Class B")
Class.C<-bigfoot.date %>% filter(classification=="Class C")

map_m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(-98.5795,39.8283,zoom=2) %>% 
  addCircleMarkers(lng=Class.A$longitude,lat=Class.A$latitude,radius = 4,opacity = .8,fillOpacity = .8,fill=T,color = "red",popup = paste(Class.A$SightMonth,Class.A$SightYear,"|",Class.A$title),group = "All Class A") %>% 
  addCircleMarkers(lng=Class.B$longitude,lat=Class.B$latitude,radius = 4,opacity = .8,fillOpacity = .8,fill=T,color = "orange",popup = paste(Class.B$SightMonth,Class.B$SightYear,"|",Class.B$title),group = "All Class B") %>% 
  addCircleMarkers(lng=Class.C$longitude,lat=Class.C$latitude,radius = 4,opacity = .8,fillOpacity = .8,fill=T,color = "blue",popup = paste(Class.C$SightMonth,Class.C$SightYear,"|",Class.C$title),group = "All Class C") %>% 
  addLegend("bottomleft",title = "Bigfoot Sighting Classes",labels = c("Class A","Class B","Class C"),colors = c("red","orange","blue"),opacity = .85) %>% 
  addLayersControl(options = layersControlOptions(collapsed = F),overlayGroups = c("All Class A","All Class B","All Class C"),position = "topright")

map_m
# Map is too difficult to read.  Let's create a cluster with a 175 mile radius

set.seed(31232)
BF.cluster<-leaderCluster(bigfoot.date[,5:6],radius = 175,distance = "haversine",max_iter =25)
Clust.BF<-cbind(ClusterID=BF.cluster$cluster_id,bigfoot.date)
Clust.AGG<-Clust.BF %>% group_by(ClusterID) %>% summarise(Count=n()) %>% mutate(percent.sight=Count/3810) %>% arrange(desc(Count)) 
Top.5.Clust<-head(Clust.AGG,5)
knitr::kable(Top.5.Clust)

round(sum(Top.5.Clust$percent.sight),digits=3)*100

# [1] 30.8 percent chance

# Create maps of top cluster ids
Top.5.Clust.Dat<-inner_join(Top.5.Clust,Clust.BF,by="ClusterID")
Top.5.Clust.Dat$ClusterID<-as.factor(Top.5.Clust.Dat$ClusterID)
clust.47<-Top.5.Clust.Dat %>% filter(ClusterID=="47")
clust.28<-Top.5.Clust.Dat %>% filter(ClusterID=="28")
clust.14<-Top.5.Clust.Dat %>% filter(ClusterID=="14")
clust.59<-Top.5.Clust.Dat %>% filter(ClusterID=="59")
clust.9<-Top.5.Clust.Dat %>% filter(ClusterID=="9")

# Top cluster that has 11.2 percent of sightings in Washingston and Oregan
clust.47.map <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(mean(clust.47$longitude),mean(clust.47$latitude),zoom=6) %>% 
  addCircleMarkers(lng=clust.47$longitude,clust.47$latitude,radius = 2.5,opacity = .8,fillOpacity = .2,fill=T,color = "red",popup = paste(clust.47$SightMonth,clust.47$SightYear,"|",clust.47$title),group = "Sightings") %>% 
  addHeatmap(data=clust.47,lng = ~clust.47$longitude,lat = ~clust.47$latitude,blur =20,max =.5,radius =10 ,group = "Sighting Heat Map") %>% 
  addLayersControl(options = layersControlOptions(collapsed = F),overlayGroups = c("Sightings","Sighting Heat Map"),position = "topright")%>%hideGroup(c("Sightings"))

clust.47.map

# Map of 5.7 percent sightings happen is Ohio and Pen Valley

clust.28.map <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(mean(clust.28$longitude),mean(clust.28$latitude),zoom=6) %>% 
  addCircleMarkers(lng=clust.28$longitude,clust.28$latitude,radius = 2.5,opacity = .8,fillOpacity = .2,fill=T,color = "red",popup = paste(clust.28$SightMonth,clust.28$SightYear,"|",clust.28$title),group = "Sightings") %>% 
  addHeatmap(data=clust.28,lng = ~clust.28$longitude,lat = ~clust.28$latitude,blur =20,max =.5,radius =10 ,group = "Sighting Heat Map") %>% 
  addLayersControl(options = layersControlOptions(collapsed = F),overlayGroups = c("Sightings","Sighting Heat Map"),position = "topright")%>%hideGroup(c("Sightings"))

clust.28.map

# Map of 4.69 percent sightings in Florida

clust.14.map <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(mean(clust.14$longitude),mean(clust.14$latitude),zoom=6) %>% 
  addCircleMarkers(lng=clust.14$longitude,clust.14$latitude,radius = 2.5,opacity = .8,fillOpacity = .2,fill=T,color = "red",popup = paste(clust.14$SightMonth,clust.14$SightYear,"|",clust.14$title),group = "Sightings") %>% 
  addHeatmap(data=clust.14,lng = ~clust.14$longitude,lat = ~clust.14$latitude,blur =20,max =.5,radius =10 ,group = "Sighting Heat Map") %>% 
  addLayersControl(options = layersControlOptions(collapsed = F),overlayGroups = c("Sightings","Sighting Heat Map"),position = "topright")%>%hideGroup(c("Sightings"))

clust.14.map

# Map of 4.61 percent sightings on Canada border.

clust.59.map <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(mean(clust.59$longitude),mean(clust.59$latitude),zoom=6) %>% 
  addCircleMarkers(lng=clust.59$longitude,clust.59$latitude,radius = 2.5,opacity = .8,fillOpacity = .2,fill=T,color = "red",popup = paste(clust.59$SightMonth,clust.59$SightYear,"|",clust.59$title),group = "Sightings") %>% 
  addHeatmap(data=clust.59,lng = ~clust.59$longitude,lat = ~clust.59$latitude,blur =20,max =.5,radius =10 ,group = "Sighting Heat Map") %>% 
  addLayersControl(options = layersControlOptions(collapsed = F),overlayGroups = c("Sightings","Sighting Heat Map"),position = "topright")%>%hideGroup(c("Sightings"))

clust.59.map

# Map of 4.4 percent sightings in Eastern Kentucky.

clust.9.map <- leaflet() %>%
  addProviderTiles(providers$Stamen.Terrain) %>%
  setView(mean(clust.9$longitude),mean(clust.9$latitude),zoom=6) %>% 
  addCircleMarkers(lng=clust.9$longitude,clust.9$latitude,radius = 2.5,opacity = .8,fillOpacity = .2,fill=T,color = "red",popup = paste(clust.9$SightMonth,clust.9$SightYear,"|",clust.9$title),group = "Sightings") %>% 
  addHeatmap(data=clust.9,lng = ~clust.9$longitude,lat = ~clust.9$latitude,blur =20,max =.5,radius =10 ,group = "Sighting Heat Map") %>% 
  addLayersControl(options = layersControlOptions(collapsed = F),overlayGroups = c("Sightings","Sighting Heat Map"),position = "topright")%>%hideGroup(c("Sightings"))

clust.9.map

