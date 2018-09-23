#Lab1 Part2
#Name : Vigneshwaran Vasanthakumar UBID#: 50248708
#Team Member's name: Siddharth Selvaraj UBID#:5024
#Lab1_Part3  Twitter Application Development
# Collecting tweets
library (twitteR) 
consumer_key <- "UztRpM22FN9TwttMeOcnN2ZEx"
consumer_secret <- "0pfEscDYrp92dN3orQqJGJSpvbfCcEYpMvDJHh04hKonBQTkGr"
access_token <- "4431681614-93f0MxilzqIIaqLuSMBoUsirj9uofjV2VsdSExv"
access_secret <- "2CeDw53zeV8pzntYE9dKB33T2Qeq9RfzRO9pMN9OHQctH"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets <- searchTwitter("flu OR #flu",geocode = '39.0119,-98.48425,2500mi' ,n = 2800, lang = "en")
tweets.df <-twListToDF(tweets)

# Getting users from tweets
users <-tweets.df$screenName
temp_df <- twListToDF(lookupUsers(users))
temp_df

# Getting locations and removing non-ascii/blank cells
location <-temp_df[,12]
location <-iconv(location, "latin1", "ASCII", sub="")
location[location==""] <- NA
location <- na.omit(location)
location.df <-data.frame(location)

#geocoding
library(ggplot2)
library(ggmap)
lonlat <- geocode(location)
lonlat <-na.omit(lonlat)
lonlat.df <-data.frame(lonlat)

write.csv(lonlat, "E:\\Dic\\lonlat10.csv")

#Reverse Geocoding
library("ggmap")
geocode_data <- read.csv(file.choose())
for (i in 1:2490)
{
  revcodesmore <- revgeocode(location=as.numeric(geocode_data[i,2:3]),output=c("more"),messaging = FALSE, sensor = FALSE, override_limit = FALSE, client = "", signature = "")

  write.table(revcodesmore$administrative_area_level_1,col.names=FALSE,file="t_states.csv",append=TRUE,sep=",")
}

count_statenames <- read.csv(file.choose(),header=FALSE,sep=",")  #choose t_states

unique_statecount <- as.data.frame(count_statenames[,2])
state_counts <- as.data.frame(table(unique_statecount))

library(zipcode)
data(zipcode)
head(zipcode)
zipcode[,3]
state_abb <- as.data.frame(table(zipcode[,3]))
state_abb[,1]
state_fullnames <- state.name[match(state_abb[,1],state.abb)]
state_fullnames <- as.data.frame(na.omit(state_fullnames))
for (i in 1:188)
{
  for (j in 1:50)
  {
    if(as.character(state_counts[i,1])==as.character(state_fullnames[j,]))
    {
      write.table(state_counts[i,],file="uniquestatecounts_new1.csv",col.names=FALSE,append=TRUE,sep=",")
    }
  }
}

library(fiftystater)
library(RColorBrewer)


plotdata <- read.csv(file.choose(), header=FALSE, sep=",")  #choose uniquestatecounts_new1.csv
plot_data_77<- data.frame(plotdata)
state_name <- tolower(plot_data_77[,2])
Count <- plot_data_77[,3]

data_new <- data.frame(state_name,Count)

library(scales)
p <- ggplot(data_new, aes(map_id = state_name)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = Count), map = fifty_states,color="black") + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_fixed(1.3) +
  scale_x_continuous(breaks =NULL) + 
  scale_y_continuous(breaks =NULL) +
  labs(fill = "ILI Activity Level",title = "2017-18 Influenza Season 2018",x = "", y = "") +
  theme(legend.position = "right", 
        panel.background = element_blank(),panel.border =  element_blank()) + scale_fill_continuous(low = "yellow", high = "red")

p

