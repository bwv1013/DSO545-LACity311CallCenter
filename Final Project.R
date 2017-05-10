data = read.csv("MyLA311_Service_Request_Data_2016 copy.csv")

library(lubridate)

data$CreatedDate=mdy_hms(data$CreatedDate)
data$UpdatedDate=mdy_hms(data$UpdatedDate)

data$DayofWeek = wday(data$CreatedDate, label=T, abbr=F)
data$Hour = hour(data$CreatedDate)
data$Year = year(data$CreatedDate)
data$Month = month(data$CreatedDate, label = T)
data$YM =  factor(paste(data$Month, data$Year))
data = data %>%
  filter(!YM == "Dec 2016") %>%
  droplevels()
data$YM = factor(data$YM, levels(data$YM)[c(2,15, 13, 11, 4, 6, 5, 9, 1, 10, 8, 7, 3, 16, 14, 12)])

save(data, file = "311FinalData.rda")
load(file = "311FinalData.rda")


load("311FinalData.rda")


library(ggplot2)
library(dplyr)


# Trend of Total Request
data %>%
  group_by(YM) %>% 
  summarise(Count = n()) %>%
  ggplot(aes(x = YM, y = Count, group = 1)) +
  geom_line() +
  ggtitle("Trend of Total Requests") +
  labs(x = "Date") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, size = 9))

# Trend of Count by Request Type

data %>%
  group_by(YM, RequestType) %>% 
  summarise(Count = n()) %>%
  ggplot(aes(x = YM, y = Count, group = RequestType, color = RequestType)) +
  geom_line() +
  ggtitle("Trend of Count by Request Type") +
  labs(x = "Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, size = 9),
        legend.position="bottom",
        legend.title = element_text(size=10),
        legend.text=element_text(size=9))

# Trend of Count by Minor Request Type
data %>%
  group_by(YM, RequestType) %>% 
  filter(!RequestType %in% c("Bulky Items" , "Graffiti Removal")) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = YM, y = Count,  group = RequestType, color = RequestType)) +
  geom_line() +
  ggtitle("Trend of Count by Minor Request Type") +
  labs(x = "Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30),
        legend.position="bottom",
        legend.title = element_text(size=10),
        legend.text=element_text(size=9)) +
  guides(fill=guide_legend(nrow = 3))


# Alternative

data %>%
  group_by(YM, RequestType) %>% 
  filter(!RequestType %in% c("Bulky Items" , "Graffiti Removal")) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = YM, y = Count, group = RequestType)) +
  geom_line() +
  facet_wrap(~RequestType, ncol = 2, scales = "free_y") +
  ggtitle("Trend of Count by Minor Request Type") +
  labs(x = "Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, size = 9))


# Trend of Total Request Exclude Graffiti Removal & Other

data %>%
  filter(!RequestType %in% c("Graffiti Removal","Other")) %>%
  droplevels() %>%
  group_by(YM) %>% 
  summarise(Count = n()) %>%
  ggplot(aes(x = YM, y = Count, group = 1)) +
  geom_line() +
  ggtitle("Trend of Total Requests Exclude Graffiti Removal & Other") +
  labs(x = "Date") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30)) +
  scale_y_continuous(breaks = seq(0, 70000, 10000))

# Request Perentage
data %>%
  group_by(YM, RequestType) %>% 
  summarise(count = n()) %>% 
  mutate(Percentage = count / sum(count)) %>%
  ggplot(aes(x = YM, y = Percentage, fill = RequestType)) +
  geom_bar(stat = "identity") +
  ggtitle("Request Perentage") +
  labs(x = "Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, size = 9), 
        legend.text=element_text(size=9),
        legend.position="bottom",
        legend.title = element_text(size=10))

# Requests Distribution by Owner Dept.
data %>% 
  group_by(Owner, RequestType) %>% 
  summarise(Count=n()) %>%
  filter(!Owner == "") %>%
  ggplot(aes(x=reorder(Owner,Count / 1000),y = Count / 1000))+ 
  geom_bar(stat = "identity", fill = "lightblue")+
  xlab('Owner Department')+
  ylab('Number of Requests (in Thousand)')+
  ggtitle('Requests Distribution by Owner Dept.') +
  theme_minimal()

# Distribution of Request Amount for Each Area Planning Commissions
data %>%
  filter(!APC == "") %>%
  ggplot(aes(APC)) +
  geom_bar(fill = "lightblue")+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust = 0.8)) +
  scale_y_continuous(breaks = seq(0,300000,50000)) +
  labs(x = "Area Planning Commissions", y = "Count", 
       title = "Distribution of Request Amount for Each Area Planning Commissions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30)) 



library(stringr)
a = str_locate(data$APC, "APC")[,1]-1

APC = str_sub(data$APC, end = a)
data$APC1 = str_replace(data$APC1, pattern = "Los Angeles", replacement = "LA")
data$APC1 = str_replace(data$APC1, pattern = "West", replacement = "W.")
data$APC1 = str_replace(data$APC1, pattern = "East", replacement = "E.")
data$APC1 = str_replace(data$APC1, pattern = "South", replacement = "S.")
data$APC1 = str_replace(data$APC1, pattern = "North", replacement = "N.")

# Distribution of Request Amount for Each APC by Request Type
data %>%
  filter(!APC1 == "") %>%
  ggplot(aes(APC1, fill = RequestType == "Report Water Waste")) +
  geom_bar() +
  facet_wrap(~RequestType, scales = "free_y", ncol = 3) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust = 0.8)) +
  labs(x = "Area Planning Commissions", y = "Count", 
       title = "Distribution of Request Amount for Each APC by Request Type") +
  scale_fill_manual(values = c("lightblue", "pink"), guide = F) +
  theme_minimal()

# Distribution of Request Amount for Each Council District
data %>%
  filter(!is.na(CD)) %>%
  ggplot(aes(factor(CD))) +
  geom_bar(fill = "lightblue")+
  labs(x = "Council District", y = "Count", 
       title = "Distribution of Request Amount for Each Council District") +
  theme_minimal()


# Distribution of Request Amount for #8/9/10/13/14/15 Council District
data %>%
  filter(!is.na(CD) & CD %in% c(8,9,10,13,14,15)) %>%
  ggplot(aes(RequestType)) +
  geom_bar(fill = "lightblue")+
  labs(x = "Request Type", y = "Count", 
       title = "Distribution of Request Amount for #8/9/10/13/14/15 Council District ") +
  theme_minimal() +
  facet_wrap(~CD, ncol = 2) +
  coord_flip()
#theme(axis.text.x = element_text(angle = 30)) +
scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

######## Analysis by Zip Code area ########
### 1. Bulky item per househol by zipcode
### 2. Call & app volume by zipcode


### 1. Bulky Item per Household
bulky=
  data1 %>%
  filter(RequestType=="Bulky Items")

bulkyzip=
  bulky %>%
  group_by(ZipCode) %>%
  summarise(count= n())

bulkyzip= merge(bulkyzip, pop, by.x="ZipCode", by.y="Zip.Code")

bulkyzip=
  bulkyzip %>%
  mutate(ReqPerHouse= count/Total.Households) %>%
  mutate(ReqPerPerson= count/Total.Population)

## read shapefile
library(maptools)
zipshp <- readShapeSpatial("geo_export_3d86418b-6004-4d7b-8595-a1666788d0b5.shp")
print(zipshp$objectid)
dfshp= zipshp@data
plotbulky= merge(dfshp, bulkyzip, by.x="zipcode", by.y="ZipCode")
groupdata= fortify(zipshp)
merge.bulky<-merge(groupdata, plotbulky, by.x="id", by.y="objectid")
final.merge.bulky= arrange(merge.bulky, group, order)

### Plot the map
la=qmap("Los Angeles", maptype="roadmap", color="bw")
install.packages("scales")
library(ggplot2)
library(scales)

final.merge.bulky.filter= filter(final.merge.bulky, zipcode!=90071 & zipcode!=91330)

la +
  geom_polygon(data= final.merge.bulky.filter, 
               aes(x = long, y = lat, group = group, fill = ReqPerHouse), alpha=0.6) +
  scale_fill_gradient(name= "value", 
                      low= "white", high="#660099") +
  ggtitle("Bulky Items per Household")

test1=arrange(bulkyzip, -ReqPerHouse)


### CALL & APP by ZIPCODE ###

## CALL
levels(data1$RequestSource)
call=filter(data1, RequestSource== "Call")
callzip=
  call %>%
  group_by(ZipCode) %>%
  summarise(count= n())
#plot the map
plotcall= merge(dfshp, callzip, by.x="zipcode", by.y="ZipCode")
groupdata= fortify(zipshp)
merge.call<-merge(groupdata, plotcall, by.x="id", by.y="objectid")
final.merge.call= arrange(merge.call, group, order)

la +
  geom_polygon(data= final.merge.call, 
               aes(x = long, y = lat, group = group, fill = count), alpha=0.6) +
  scale_fill_gradient(name= "Volume", 
                      low= "white", high="#006666") +
  ggtitle("Call Volume by Zip Code")

## APP
levels(data1$RequestSource)
mobile=filter(data1, RequestSource== "Mobile App")
mobzip=
  mobile %>%
  group_by(ZipCode) %>%
  summarise(count= n())
#plot the map
plotmob= merge(dfshp, mobzip, by.x="zipcode", by.y="ZipCode")
groupdata= fortify(zipshp)
merge.mob<-merge(groupdata, plotmob, by.x="id", by.y="objectid")
final.merge.mob= arrange(merge.mob, group, order)

la +
  geom_polygon(data= final.merge.mob, 
               aes(x = long, y = lat, group = group, fill = count), alpha=0.6) +
  scale_fill_gradient(name= "Volume", 
                      low= "white", high="#660066") +
  ggtitle("App Volume by Zip Code")



########## CLUSTERING ANALYSIS ##########
## Used K-means clustering to segment zip code areas into different clusters that share simialr request patterns

load("datazip.rda")

zipagg=
  data1 %>%
  group_by(ZipCode, RequestType) %>%
  summarise(count = n())

write.csv(zipagg, "zipagg.csv")
### use excel to format dataset for clusering, read.csv


cluster= read.csv("cleanzip_cluster.csv")
colnames(cluster)[1]="zipcode"
cluster[is.na(cluster)] <- 0


library(stats)


wss <- (nrow(cluster)-1)*sum(apply(cluster,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cluster,
                                     centers=i)$withinss)


?kmeans
set.seed(20)
zipcluster <- kmeans(cluster[, 2:13], 5, nstart = 20)
zipcluster
zipcluster$cluster
chorodata= data.frame(cluster$zipcode, zipcluster$cluster)


chorodata= arrange(chorodata, zipcluster.cluster)
test=merge(chorodata, cluster, by.x="cluster.zipcode", by.y="zipcode")
test=arrange(test,zipcluster.cluster)


### using shapefile
?readShapeSpatial
library(maptools)
zipshp <- readShapeSpatial("geo_export_bbeedb4e-66f7-4e6e-b8b3-e8aae113305c.shp")
names(zipshp)
print(zipshp$name)
dfshp= zipshp@data
plotdata= merge(dfshp, chorodata, by.x="zipcode", by.y="cluster.zipcode")
groupdata= fortify(zipshp)
merge<-merge(groupdata, plotdata, by.x="id", by.y="objectid")
final.merge= arrange(merge, group, order)
final.merge$zipcluster.cluster= as.factor(final.merge$zipcluster.cluster)

library(ggplot2)


loc= c(-119, 33.7, -117.5, 34.5)
la=qmap(loc, maptype="roadmap", color="bw")

la +
  geom_polygon(data = final.merge, 
               aes(x = long, y = lat, group = group, 
                   fill = zipcluster.cluster), color="black", alpha= 0.6) +
  scale_fill_manual(name="cluster",values=c('#a6cee3','#1f78b4','#b2df8a',
                                            '#33a02c','#fb9a99','#e31a1c')) +
  ggtitle("Clusters by Request Type")


sum= data.frame(zipcluster[[2]])
sum=t(sum)
sum=rbind(c(4, 46, 29, 10, 50), sum)
rownames(sum)[1]="size"


######## Analysis by geographic area ######## 
CA <- "Los Angeles"

hmap <- qmap(CA, zoom = 12, color = "bw")
gmap <- qmap(CA, zoom = 10, color = "bw")
```



# Distribution of Calls on Map of Zoom = 12
par(mfrow = c(1,2))
hmap +
  stat_density2d(data = data, aes(x = Longitude, y = Latitude,
                                  fill = ..level.., 
                                  alpha = ..level..),
                 geom = "polygon") +
  scale_alpha(guide = "none") +
  ggtitle("Distribution of Service Request on Map") +
  scale_fill_gradient(low = "black", high = "red", guide = FALSE) +
  theme(plot.title = element_text(size = 17))

# Distribution of Calls on Map of Zoom = 10

gmap +
  stat_density2d(data = data, aes(x = Longitude, y = Latitude,
                                  fill = ..level.., 
                                  alpha = ..level..),
                 geom = "polygon") +
  scale_alpha(guide = "none") +
  ggtitle("Distribution of Service Request on Map") +
  scale_fill_gradient(low = "black", high = "red", guide = FALSE) +
  theme(plot.title = element_text(size = 17))



data$DayofWeek <- factor(data$DayofWeek, ordered = F, 
                         levels = c("Monday", "Tuesday", "Wednesday",
                                    "Thursday", "Friday", "Saturday",
                                    "Sunday"))

# Distribution of Calls on Map of Each Day with Zoom = 12

hmap +
  stat_density2d(data = data, aes(x = Longitude, y = Latitude,
                                  fill = ..level..,
                                  alpha = ..level..),
                 geom = "polygon") +
  facet_wrap(~DayofWeek) +
  scale_alpha(guide = "none") +
  ggtitle("Distribution of Service Request on Map on Each Day") +
  scale_fill_gradient(low = "black", high = "red", guide = FALSE) +
  theme(plot.title = element_text(size = 17))


# Distribution of Calls on Map on Each day with Zoom = 10

gmap +
  stat_density2d(data = data, aes(x = Longitude, y = Latitude,
                                  fill = ..level..,
                                  alpha = ..level..),
                 geom = "polygon") +
  facet_wrap(~DayofWeek) +
  scale_alpha(guide = "none") +
  ggtitle("Distribution of Service Request on Map on Each Day") +
  scale_fill_gradient(low = "black", high = "red", guide = FALSE) +
  theme(plot.title = element_text(size = 17))



## Distribution of Request Type on Map with Zoom = 12
hmap +
  stat_density2d(data = data, aes(x = Longitude, y = Latitude,
                                  fill = ..level..,
                                  alpha = ..level..),
                 geom = "polygon") +
  facet_wrap(~RequestType, ncol = 4) +
  scale_alpha(guide = "none") +
  ggtitle("Distribution of Request Type on Map") +
  scale_fill_gradient(low = "white", high = "red", guide = FALSE) +
  theme(plot.title = element_text(size = 17))

## Distribution of Request Type on Map with Zoom = 10

gmap +
  stat_density2d(data = data, aes(x = Longitude, y = Latitude,
                                  fill = ..level..,
                                  alpha = ..level..),
                 geom = "polygon") +
  facet_wrap(~RequestType, ncol = 4) +
  scale_alpha(guide = "none") +
  ggtitle("Distribution of Request Type on Map") +
  scale_fill_gradient(low = "white", high = "red", guide = FALSE) +
  theme(plot.title = element_text(size = 17))



# Distribution of Bulky Items on Map with Zoom = 12
library(dplyr)
bulky <- data %>%
  filter(RequestType == "Bulky Items")

hmap +
  stat_density2d(data = bulky, aes(x = Longitude, y = Latitude,
                                   fill = ..level..,
                                   alpha = ..level..),
                 geom = "polygon") +
  scale_alpha(guide = "none") +
  ggtitle("Distribution of Bulky Items on Map") +
  scale_fill_gradient(low = "white", high = "darkblue", guide = FALSE) +
  theme(plot.title = element_text(size = 17))



# Distribution of Graffiti Removal on Map
Graffiti_Removal <- data %>%
  filter(RequestType == "Graffiti Removal")

hmap +
  stat_density2d(data = Graffiti_Removal, aes(x = Longitude, y = Latitude,
                                              fill = ..level..,
                                              alpha = ..level..),
                 geom = "polygon") +
  scale_alpha(guide = "none") +
  ggtitle("Distribution of Graffiti Removal on Map") +
  scale_fill_gradient(low = "green", high = "darkblue", guide = FALSE) +
  theme(plot.title = element_text(size = 17))


crime <- read.csv("Crime.csv")
streetlight <- subset(data, RequestType == "Single Streetlight Issue" |
                        RequestType == "Multiple Streetlight Issue")

streetlight$RequestType <- "Streetlight Issue"
crime <- crime[ ,-1:-8]

crime$Location.1 <- as.character(crime$Location.1)
library(stringr)
str_locate(crime$Location.1, ",")
crime$lat <- str_sub(crime$Location.1, start = 2, end = 8)
crime$long <- str_sub(crime$Location.1, start = 10, end = 18)
crime$lat <- as.numeric(crime$lat)
crime$long <- as.numeric(crime$long)


hmap +
  stat_density2d(data = crime, aes(x = long, y = lat,
                                   fill = "green", alpha = ..level..),
                 geom = "polygon") +
  stat_density2d(data = streetlight, aes(x = Longitude, y = Latitude, 
                                         fill = "purple", alpha = ..level..), geom = "polygon") +
  ggtitle("Crime vs. Streetlight Issue") +
  theme(plot.title = element_text(size = 17)) +
  scale_fill_manual(values = c("green", "purple"))



new_data <- subset(data, RequestType != "Single Streetlight Issue" &
                     RequestType != "Multiple Streetlight Issue")

data1 <- rbind(new_data, streetlight)

#
hmap +
  stat_density2d(data = data, aes(x = Longitude, y = Latitude,
                                  fill = ..level..,
                                  alpha = ..level..),
                 geom = "polygon") +
  facet_wrap(~RequestType, ncol = 4) +
  scale_alpha(guide = "none") +
  ggtitle("Distribution of Request Type on Map") +
  scale_fill_gradient(low = "green", high = "darkblue", guide = FALSE) +
  theme(plot.title = element_text(size = 17))



explore <- subset(data, Month == "Jun" | Month == "Jul" | Month == "Aug" |
                    Month == "Sep")

explore <- subset(explore, RequestType == "Bulky Items")

bulky <- subset(data, RequestType == "Bulky Items")

gmap +
  stat_density2d(data = bulky, aes(x = Longitude, y = Latitude,
                                   fill = ..level..,
                                   alpha = ..level..), geom = "polygon") +
  facet_wrap(~Month) +
  scale_alpha(guide = "none") +
  ggtitle("Distribution of Bulky Item from June to September") +
  scale_fill_gradient(low = "green", high = "darkblue", guide = FALSE) +
  theme(plot.title = element_text(size = 17))



######## Current Request Solving Status, Internal Job Distribution and Working Efficiency ########
### 1. Requests distribution by Owner Dept. and AssignTo Group
### 2. ActionTaken & Status
### 3. Working efficiency measured by Processing Time 

### 1. Requests distribution by Owner Dept. and AssignTo Group
#Pre-process date data
library(lubridate)
library(stringr)
data$Year = year(data$CreatedDate)
data$Month = month(data$CreatedDate, label = T)
data$YM =  factor(paste(data$Month, str_sub(data$Year)))
levels(data$YM)
data$YM = factor(data$YM, levels(data$YM)[c(2,15, 13, 11, 4, 6, 5, 9, 1, 10, 8, 7, 3, 16, 14, 12)])

#Requests distribution by owner types through time
data %>%
  group_by(YM, Owner) %>% 
  summarise(count = n()) %>% 
  mutate(Percentage = count / sum(count)) %>%
  ggplot(aes(x = YM, y = Percentage, fill = Owner)) +
  geom_bar(stat = "identity") +
  ggtitle("Requests distribution by Owner Type") +
  labs(x = "Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, size = 8.5, vjust = 2), 
        legend.text=element_text(size=9),
        legend.position="bottom",
        legend.title = element_text(size=10))

#Requests distribution by owner vs request types 
data%>%
  group_by(Owner, RequestType)%>%
  summarise(numofrequests=n())%>%
  filter(Owner!="")%>%
  mutate(perc=numofrequests/sum(numofrequests))%>%
  ggplot(aes(x=Owner, y=perc,fill=RequestType))+
  geom_bar(stat="identity")+
  labs(x="Owner Dept.",y="Number of Requests",title="Request Type distribution among Owner Dept.")+
  theme(axis.text.x = element_text(size = 8.5, vjust = 0), 
        legend.text=element_text(size=9),
        legend.position="bottom",
        legend.title = element_text(size=10))

#Overall requests distribution by AssignTo
data%>%
  group_by(AssignTo)%>%
  summarise(numofrequests=n())%>%
  ggplot(aes(x=reorder(AssignTo,-numofrequests),y=numofrequests))+
  geom_bar(stat='identity')+
  xlab('Assign To Group')+
  ylab('Number of Requests')+
  ggtitle('Requests distributed by AssignTo Group')+
  coord_flip()

#As there are too many AssignTo groups, we select top 10 of AssignTo by request volumn
top10=data%>%
  group_by(AssignTo)%>%
  summarise(numofrequests=n())%>%
  arrange(-numofrequests)%>%
  slice(1:10)

data%>%
  group_by(YM,AssignTo)%>%
  summarise(numofrequests=n())%>%
  filter(AssignTo%in%c("NC","EV","SLA","WV","WLA","SLABT","HB","GAP","GAPBH","CCAC"))%>%
  ggplot(aes(x=YM,y=numofrequests,color=AssignTo,group=AssignTo))+
  geom_line()+
  xlab('Top10 Assign To Group')+
  ylab('Number of Requests')+
  ggtitle('Requests distributed by AssignTo Group')+
  theme(axis.text.x = element_text(angle = 30, size = 8.5, vjust = 0.5), 
        legend.text=element_text(size=9),
        legend.position="bottom",
        legend.title = element_text(size=10))

#To see how job is differentiate among groups, either by RequestType or by Region, we draw below graphs
data%>%
  group_by(AssignTo, RequestType)%>%
  summarise(numofrequests=n())%>%
  filter(AssignTo%in%c("EV","SLA","NC"))%>%
  ggplot(aes(x=AssignTo, y=numofrequests,fill=RequestType))+
  geom_bar(stat="identity")+
  labs(x="AssignTo Group",y="Number of Requests",title="Requests Type distribution among Top 3 'Busy' Groups")+
  theme(axis.text.x = element_text(size = 8.5, vjust = 0), 
        legend.text=element_text(size=9),
        legend.position="bottom",
        legend.title = element_text(size=10))

data%>%
  group_by(AssignTo, ZipCode)%>%
  summarise(numofrequests=n())%>%
  filter(AssignTo%in%c("EV","SLA","NC"))%>%
  ggplot(aes(x=AssignTo, y=numofrequests,fill=ZipCode))+
  geom_bar(stat="identity")+
  labs(x="AssignTo Group",y="Number of Requests",title="Area distribution among Top 3 'Busy' Groups")+
  theme(axis.text.x = element_text(size = 8.5, vjust = 0), 
        legend.text=element_text(size=9),
        legend.title = element_text(size=10))
#Obviously, groups are assigned by region, which makes sense




### 2. Request Status & ActionTaken

#See how requests are solved through time 
data %>%
  group_by(YM, Status) %>% 
  summarise(count = n()) %>% 
  mutate(Percentage = count / sum(count)) %>%
  ggplot(aes(x = YM, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  ggtitle("Requests solved through time") +
  labs(x = "Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, size = 8.5, vjust = 2), 
        legend.text=element_text(size=9),
        legend.position="bottom",
        legend.title = element_text(size=10)) 

#Only look at those not closed cases
data %>%
  group_by(YM, Status) %>% 
  summarise(count = n()) %>% 
  filter(Status!="Closed")%>%
  mutate(Percentage = count / sum(count)) %>%
  ggplot(aes(x = YM, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  ggtitle("Requests solved through time(not closed case)") +
  labs(x = "Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, size = 8.5, vjust = 2), 
        legend.text=element_text(size=9),
        legend.position="bottom",
        legend.title = element_text(size=10))

#What are those open cases back in Aug 2015?
openAug2015=data %>%
  group_by(YM,Status,RequestType)%>%
  summarise(count=n())%>%
  filter(YM=="Aug 2015")%>%
  filter(Status=="Open")
openAug2015


# We can see that most cases are closed. However, what about those not closed case, 
# especially Open and Pending,which type are they?

#Heatmap status vs. request types
data %>%
  group_by(Status, RequestType) %>% 
  summarise(count = n()) %>% 
  filter(Status%in%c("Open","Pending"))%>%
  ggplot(aes(x=Status,y=RequestType,fill=count))+
  geom_tile()+
  scale_fill_gradient(low='white',high='blue',guide=FALSE)+
  ggtitle('Heatmap of RequestType vs. Status(Open and Pending)')+
  xlab('Request Status')+
  ylab('Request Type')+
  theme_classic()+
  theme(axis.text.x = element_text(size = 9.5))



### 3. Working efficiency measured by Processing Time 
#Calculate processing time=UpdatedDate-CreatedDate
data$protime<-with(data,difftime(UpdatedDate,CreatedDate,units="hours"))
data$protime=as.numeric(round(data$protime,digit=1))
class(data$protime)

#However, we can see that there are many processing time <= 0, 
#which is probably due to incorrect recording, so we will
#exclude those 0 processing time

#Processing Time by each Owner Dept. through time
data%>%
  group_by(YM,Owner)%>%
  filter(protime>0)%>%
  summarise(avgprot=mean(protime))%>%
  ggplot(aes(x=YM,y=avgprot,color=Owner,group=Owner))+
  geom_line()+
  xlab('Time')+
  ylab('Hours')+
  ggtitle('Average Processing Time by Owner Dept.')+
  theme(axis.text.x = element_text(angle = 30, size = 8.5, vjust = 0), 
        legend.text=element_text(size=9),
        legend.position="bottom",
        legend.title = element_text(size=10))

#Seems BSS has abnormal high average processing time before Jan 2016, why is that?
#BSS
data%>%
  group_by(Owner)%>%
  filter(Owner=="BSS")%>%
  filter(year(CreatedDate)<2016)%>%
  group_by(RequestType)%>%
  summarize(count=n())%>%
  ggplot(data=BSStype,aes(x=reorder(RequestType,-count),y=count))+
  geom_bar(stat="identity")+
  labs(x="Request Type",y="Count",title="Request numbers for BSS by RequestType")

data%>%
  group_by(Owner)%>%
  filter(Owner=="BSS")%>%
  filter(year(CreatedDate)<2016)%>%
  group_by(RequestType)%>%
  summarize(avgprot=mean(protime))%>%
  ggplot(aes(x=reorder(RequestType,-avgprot),y=avgprot))+
  geom_bar(stat="identity")+
  labs(x="Request Type",y="Average Processing Time",title="Processing Time for BSS by RequestType")

#Both indicates that BSS is mainly in charge of Homeless issue, both in large quantity and took longer time.

#Processing Time by each AssignTo Group(top 10 by requests number) through time
#We have known the top 10 groups in previous graphs("NC","EV","SLA","WV","WLA","SLABT","HB","GAP","GAPBH","CCAC")

data%>%
  group_by(YM,AssignTo)%>%
  summarise(avgprot=mean(protime))%>%
  filter(AssignTo%in%c("NC","EV","SLA","WV","WLA","SLABT","HB","GAP","GAPBH","CCAC"))%>%
  ggplot(aes(x=factor(YM),y=avgprot))+
  geom_bar(stat="identity",fill="lightblue")+
  facet_wrap(~AssignTo,ncol=2,scales="free_y")+
  xlab("Date")+
  ylab("Average Processing Time(Hours)")+
  ggtitle("Average Processing Time by Top10 AssignTo Group")+
  theme(axis.text.x = element_text(angle = 30, size = 7.5, vjust = 0), 
        legend.text=element_text(size=7),
        legend.position="bottom",
        legend.title = element_text(size=10))

##Which type of Request need longer time to process?
data%>%
  group_by(YM,RequestType)%>%
  summarise(avgprot=mean(protime))%>%
  ggplot(aes(x=factor(YM),y=avgprot,fill=avgprot<72))+
  guides(fill=guide_legend(title=NULL))+
  scale_fill_discrete(labels=c("Processing Time>72 Hrs","Processing Time<=72Hrs"))+
  geom_bar(stat="identity")+
  facet_wrap(~RequestType,ncol=2,scales="free_y")+
  xlab("Date")+
  ylab("Average Processing Time(Hours)")+
  ggtitle("Average Processing Time by RequestType")+
  theme(axis.text.x = element_text(angle = 30, size = 8.5, vjust = 0), 
        legend.text=element_text(size=9),
        legend.position="bottom",
        legend.title = element_text(size=10))


######## Arima Forecasting Model ########  

data <- data[ , -1]

data$ServiceDate <- mdy_hms(data$ServiceDate)
data$ClosedDate <- mdy_hms(data$ClosedDate)

str(data$CreatedDate)
data$CreatedDate <- as.character(data$CreatedDate)
str(data$CreatedDate)
data$CreatedTime <- str_sub(data$CreatedDate, start = 11)
data$CreatedDate <- str_sub(data$CreatedDate, end = 10)


forecast_data <- data %>%
  group_by(CreatedDate) %>%
  summarise(count = n())

Arima_data <- forecast_data[,2]
Arima_data <- ts(Arima_data, frequency = 7)
tsdisplay(Arima_data,lag.max = 100)
TotalNumbefofServiceRequest <- forecast_data[ ,2]
TotalNumbefofServiceRequest <- ts(TotalNumbefofServiceRequest, frequency = 7)
tsdisplay(TotalNumbefofServiceRequest, lag.max = 100)
## Based on the ACF graph, we can see that there is weekly pattern of service request.

## So, the lag is seven here
WeeklyDifferenceData <- diff(Arima_data, lag = 7)

# The new data set is stationary as volatility is reduced substantially. 
tsdisplay(WeeklyDifferenceData,lag.max = 100)
# Stationarity is also confirmed by the following tests
kpss.test(WeeklyDifferenceData)
adf.test(WeeklyDifferenceData)
ndiffs(WeeklyDifferenceData)

# Testing data set
TestingDataSet <- head(Arima_data, 380)
TestingDataSet <- ts(TestingDataSet, frequency = 7) 


# I selected for models based on ACF and PACF graphs
M1 <- Arima(Arima_data, order = c(0,0,1), seasonal = c(0,1,0))
M2 <- Arima(Arima_data, order = c(0,0,1), seasonal = c(0,1,1))
M3 <- Arima(Arima_data, order = c(0,0,0), seasonal = c(0,1,1))
M4 <- auto.arima(Arima_data)

# This model is based on testing dataset, while the above models are based on the entire dataset
M5 <- Arima(TestingDataSet, order = c(0,0,1), seasonal = c(0,1,0))

## M1 is the best among the four. 
tsdisplay(residuals(M1))
tsdisplay(residuals(M2))
tsdisplay(residuals(M3))
tsdisplay(residuals(M4))


M1F <- forecast(M1, h = 70)
M5F <- forecast(M5, h = 140)
M1
M5

a <- forecast_data
a <- data.frame(a)
a$weekofday <- wday(a$CreatedDate, label = TRUE)
## The blue line is the predictive values based on the testing dataset.The overlapped area between the red line and the blue line is the validation dataset. As far as the graph shows, it does a decent job in predicting values (black line) before the predictive values (blue line). It overestimated the values in the predictive intervals

plot(M5F)
plot(M1F)

lines(Arima_data, col = 2)
M5
residuals(M5)
Arima_data
M5F

#The following Arima model is to predict the daily service request of bulky item 
# since it is the largest portion of the total service request. 
#Same idea and process as above. Even comments are almost same

forecast_data <- data %>%
  group_by(CreatedDate, RequestType) %>%
  summarise(count = n())

aaa <- subset(data, RequestType == "Bulky Items")
bulky <- subset(forecast_data, RequestType == "Bulky Items")
bulky <- bulky[ , 3]

bulky <- ts(bulky, frequency = 7)

tsdisplay(bulky)
test_bulky <- head(bulky, 380)
test_bulky <- ts(test_bulky, frequency = 7)
WeeklyDifferenceData <- diff(bulky, lag = 7)
tsdisplay(WeeklyDifferenceData, lag.max = 100)
kpss.test(WeeklyDifferenceData)
adf.test(WeeklyDifferenceData)
ndiffs(WeeklyDifferenceData)

M1 <- Arima(bulky, order = c(0,0,1), seasonal = c(0,1,0))
M2 <- Arima(bulky, order = c(0,0,1), seasonal = c(0,1,1))
M3 <- Arima(bulky, order = c(0,0,0), seasonal = c(0,1,1))
M4 <- auto.arima(bulky)

M5 <- Arima(test_bulky, order = c(0,0,1), seasonal = c(0,1,0))

tsdisplay(residuals(M1))
tsdisplay(residuals(M2))
tsdisplay(residuals(M3))
tsdisplay(residuals(M4))
Box.test(residuals(M1))

tsdisplay(residuals(M5))


M1F <- forecast(M1, h = 70)
M5F <- forecast(M5, h = 100)

par(mfrow = c(1,1))
plot(M5F)
lines(fitted(M1), col = 2)
M5F


aaa <- subset(data, RequestType == "Graffiti Removal")
g <- subset(forecast_data, RequestType == "Graffiti Removal")
g <- g[ , 3]
g <- g[-1: -12, ]

g <- ts(g, frequency = 7)

tsdisplay(g)
test_bulky <- head(g, 300)
test_bulky <- ts(test_bulky, frequency = 7)
WeeklyDifferenceData <- diff(bulky, lag = 7)
tsdisplay(WeeklyDifferenceData, lag.max = 100)
kpss.test(WeeklyDifferenceData)
adf.test(WeeklyDifferenceData)
ndiffs(WeeklyDifferenceData)

M1 <- Arima(g, order = c(0,0,1), seasonal = c(0,1,0))
M2 <- Arima(g, order = c(0,0,1), seasonal = c(0,1,1))
M3 <- Arima(g, order = c(0,0,0), seasonal = c(0,1,1))
M4 <- auto.arima(g)

M5 <- Arima(test_bulky, order = c(0,0,1), seasonal = c(0,1,0))

tsdisplay(residuals(M1))
tsdisplay(residuals(M2))
tsdisplay(residuals(M3))
tsdisplay(residuals(M4))
Box.test(residuals(M1))

tsdisplay(residuals(M5))


M1F <- forecast(M1, h = 70)
M5F <- forecast(M5, h = 100)

par(mfrow = c(1,1))
plot(M5F)
lines(fitted(M1), col = 2)
M5F




forecast_data <- data %>%
  group_by(CreatedDate, RequestSource) %>%
  summarise(count = n())

call <- forecast_data %>%
  filter(RequestSource == "Call")

call <- call[ ,3]
call <- ts(call, frequency = 7)
tsdisplay(call)
test_call <- head(call, 380)
test_call <- ts(test_call, frequency = 7)
WeeklyDifferenceData <- diff(call, lag = 7)
tsdisplay(WeeklyDifferenceData)

M1 <- Arima(call, seasonal = c(0,1,1))
M1F <- forecast(M1, h = 70)
M5 <- Arima(test_call, seasonal = c(0,1,1))
M5F <- forecast(M5, h = 140)
M6 <- Arima(test_call, order = c(0,0,1), seasonal = c(0,1,1))
M6F <- forecast(M6, h = 140)
M5$aicc
M6$aicc

plot(M6F)
lines(call, col = 2)
M6F

